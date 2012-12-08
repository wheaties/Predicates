package com.wheaties.choice.iteration

object AcceptAll extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = true
}

object AcceptNone extends IterationScheme{
  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = false
}

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