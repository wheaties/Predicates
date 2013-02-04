package com.wheaties.choice.iteration

import com.wheaties.predicate.Always1

object AcceptAll extends Accept[Any]{
  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = true
}

object AcceptNone extends Accept[Any]{
  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = false
}

//Location based

class AcceptEvery(n: Int) extends Accept[Any]{
  private var count = 0

  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = count % n == 0
  protected[iteration] override def next(){
    count += 1
  }
}

class AcceptEveryF(f: Int => Int, init: Int) extends Accept[Any]{
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

class AcceptFirst(n: Int) extends Accept[Any]{
  private var count = 0

  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = count < n - 1
  protected[iteration] override def next(){
    count += 1
  }
}

class AcceptAt(n: Int) extends Accept[Any]{
  private var count = 0

  def check[@specialized(Int, Long, Float, Double) A](value: A) = count == n
  protected[iteration] override def next(){
    count += 1
  }
}

//With Predicate conditions

class AcceptIf[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends Accept[A]{
  protected[iteration] def check[AA <: A](value: AA) = pred(value)
}

class AcceptUntil[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends Accept[A]{
  private var flag = pred

  protected[iteration] def check[AA <: A](value: AA) = !flag(value)
  protected[iteration] override def next(){
    flag = Always1
  }
}

class AcceptOnce[@specialized(Int, Long, Float, Double) -A](pred: A => Boolean) extends Accept[A]{
  private var flag = pred

  protected[iteration] def check[AA <: A](value: AA) = flag(value)
  protected[iteration] override def next(){
    flag = Always1
  }
}