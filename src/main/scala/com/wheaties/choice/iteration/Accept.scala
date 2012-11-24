package com.wheaties.choice.iteration

class AcceptAll extends IterationScheme{
  def accept[A](value: A) = true
}

class AcceptEvery(n: Int) extends IterationScheme{
  private var step = -1

  def accept[A](value: A) ={
    step += 1
    if(step == n) step = 0

    step == 0
  }
}

class AcceptEveryF(f: Int => Int, init: Int) extends IterationScheme{
  require(init > 0)

  private var step = init
  private var current = 0

  def accept[A](value: A) ={
    current += 1
    if(current == step){
      step = f(step)

      true
    }
    else{
      false
    }
  }
}

class AcceptFirst(n: Int) extends IterationScheme{
  private var count = -1

  def accept[A](value: A) ={
    count += 1

    count < n
  }
}

//// This works for limiting up to but not over. It doesn't limit less than.
//// throwing the exception here is bad. If composing two Choices with "and" condition, could cause problems.
//class AcceptExactly(n: Int) extends IterationScheme{
//  private var count = 0
//
//  def accept[A](value: A) ={
//    count += 1
//    if(n < count) throw new Exception("Exceeded acceptable limit of %s" format n)
//
//    true
//  }
//}

class AcceptIf[B](pred: B => Boolean) extends IterationScheme{
  def accept[A <: B](value: A) = pred(value)
}

class AcceptUntil[B](pred: B => Boolean) extends IterationScheme{
  private var accepted = true

  def accept[A <: B](value: A) ={
    if(accepted && pred(value)) accepted = false

    accepted
  }
}

class AcceptOnce[B](pred: B => Boolean) extends IterationScheme{
  private var accepted = false

  def accept[A <: B](value: A) ={
    if(!accepted && pred(value)) accepted = true

    accepted
  }
}