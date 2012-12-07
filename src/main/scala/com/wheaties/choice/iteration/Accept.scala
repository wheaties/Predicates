package com.wheaties.choice.iteration

object AcceptAll extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = true

  def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = this
}

object AcceptNone extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = false

  def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = false
}

//TODO: think about how to lose all this mutable state but still remain GC friendly!
class AcceptEvery(n: Int) extends IterationScheme{

  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = count % n == 0

  def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = this
}

class AcceptEveryF(f: Int => Int, init: Int) extends IterationScheme{
  require(init > 0)

  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = count % init == 0

  def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = if(accept(value, count)) new AcceptEveryF[A](f, f(init))
}

class AcceptFirst(n: Int) extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = count < n - 1

  def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = this
}

class AcceptIf[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends IterationScheme{
  def accept[B <: A](value: B, count: Int) = pred(value)

  def next[B <: A](value: B, count: Int) = this
}

class AcceptUntil[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends IterationScheme{
  def accept[B <: A](value: A, count: Int) = pred(value)

  def next[B <: A](value: B, count: Int) = if(accept(value, count)) AcceptNone else this
}

class AcceptOnce[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends IterationScheme{
  def accept[B <: A](value: B, count: Int) = pred(value)

  def next[B <: A](value: B, count: Int) = if(accept(value, count)) AcceptAll else this
}