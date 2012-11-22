package com.wheaties.choice

//TODO: Think about how to do "last." Perhaps need to feed this a Builder object?
trait IterationScheme{
  def accept[A](value: A, sat: A => Boolean): Boolean
}

class AcceptAll extends IterationScheme{
  def accept[A](value: A, sat: A => Boolean) = sat(value)
}

class AcceptEvery(n: Int) extends IterationScheme{
  private var step = -1

  def accept[A](value: A, sat: A => Boolean) ={
    val eval = sat(value)
    if(eval) step += 1
    if(step == n) step = 0

    eval && step == 0
  }
}

class AcceptFirst(n: Int) extends IterationScheme{
  private var count = -1

  def accept[A](value: A, sat: A => Boolean) ={
    val eval = sat(value)
    if(eval) count += 1

    eval && count < n
  }
}

//TODO: This works for limiting up to but not over. It doesn't limit less than.
//TODO: throwing the exception here is bad. If composing two Choices with "and" condition, could cause problems.
class AcceptExactly(n: Int) extends IterationScheme{
  private var count = 0

  def accept[A,B >: A](value: A, sat: B => Boolean) ={
    val eval = sat(value)
    if(eval) count += 1
    if(n < count) throw new Exception("Exceeded acceptable limit of %s" format n)

    eval
  }
}
