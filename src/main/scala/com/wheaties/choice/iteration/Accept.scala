package com.wheaties.choice.iteration

import com.wheaties.predicate.Always1

object AcceptAll extends IterationScheme{
  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = true
}

object AcceptNone extends IterationScheme{
  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = false
}

//Location based

class AcceptEvery(n: Int) extends IterationScheme{
  private var count = 0

  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = count % n == 0
  protected[iteration] override def next(){
    count += 1
  }
}

class AcceptEveryF(f: Int => Int, init: Int) extends IterationScheme{
  require(init > 0)

  private var current = init
  private var count = 0

  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = count % current == 0
  protected[iteration] override def next(){
    if(count % current == 0){
      current = f(current)
    }
    count += 1
  }
}

class AcceptFirst(n: Int) extends IterationScheme{
  private var count = 0

  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = count < n - 1
  protected[iteration] override def next(){
    count += 1
  }
}

class AcceptAt(n: Int) extends IterationScheme{
  private var count = 0

  def check[@specialized(Int, Long, Float, Double) A](value: A) = count == n
  protected[iteration] override def next(){
    count += 1
  }
}

//With Predicate conditions

class AcceptIf[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends IterationScheme{
  protected[iteration] def check[B <: A](value: B) = pred(value)
}

class AcceptUntil[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends IterationScheme{
  private var flag = pred

  protected[iteration] def check[B <: A](value: B) = !flag(value)
  protected[iteration] override def next(){
    flag = Always1 or pred
  }
}

class AcceptOnce[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends IterationScheme{
  private var flag = pred

  protected[iteration] def check[B <: A](value: B, count: Int) = flag(value)
  protected[iteration] override def next(){
    flag = Always1 or pred
  }
}