package com.wheaties.choice.iteration

object AcceptAll extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = true
}

object AcceptNone extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = false
}

//Location based

class AcceptEvery(n: Int) extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = count % n == 0
}

class AcceptEveryF(f: Int => Int, init: Int) extends ReplacingScheme{
  require(init > 0)

  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = count % init == 0

  def replace = new AcceptEveryF(f, f(init))
}

class AcceptFirst(n: Int) extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = count < n - 1
}

class AcceptAt(n: Int) extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = count == n
}

//With Predicate conditions

class AcceptIf[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends IterationScheme{
  def accept[B <: A](value: B, count: Int) = pred(value)
}

class AcceptUntil[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends ReplacingScheme{
  def accept[B <: A](value: B, count: Int) = pred(value)

  def replace = AcceptNone
}

class AcceptOnce[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends ReplacingScheme{
  def accept[B <: A](value: B, count: Int) = pred(value)

  def replace = AcceptAll
}

//Now we get into the part where it can be more stateful without breaking immutability

class AcceptIfState[@specialized(Int, Long, Float, Double) -A](pred: (A,A) => Boolean, prev: A) extends IterationScheme{
  def accept[B <: A](value: B, count: Int) = pred(prev, value)

  override def next[B <:A](value: B, count: Int) = new AcceptIfState[A](pred, value)
}

class AcceptUntilState[@specialized(Int, Long, Float, Double) -A](pred: (A,A) => Boolean, prev: A) extends IterationScheme{
  def accept[B <: A](value: B, count: Int) = pred(prev, value)

  override def next[B <:A](value: B, count: Int) = if(pred(prev, value)) AcceptNone else new AcceptUntilState[A](pred, value)
}

class AcceptOnceState[@specialized(Int, Long, Float, Double) -A](pred: (A,A) => Boolean, prev: A) extends IterationScheme{
  def accept[B <: A](value: B, count: Int) = pred(prev, value)

  override def next[B <:A](value: B, count: Int) = if(pred(prev, value)) AcceptAll else new AcceptOnceState[A](pred, value)
}