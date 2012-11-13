package com.wheaties.choice

sealed trait Plan
trait PlanSat extends Plan  //Type for Take with a defined satisfiability
trait PlanLim extends Plan  //Type for Take with a defined limit
trait PlanSatLim extends Plan //Type for Take with both a limit ad a satisfiability

//TODO: perhaps this should be a part of "Choice" parameterized on "Choose" and "Skip"
trait Chooser[P <: Plan] extends Choice with Predicate-Like[Chooser[P]]{
  def every(n: Int)(implicit limit: Limit[Chooser,P]) = limit.every(this, n)
  def all(implicit limit: Limit[Chooser,P]) = limit.all(this)
  def first(n: Int)(implicit limit: Limit[Chooser,P]) = limit.first(this, n)
  def last(n: Int)(implicit limit: Limit[Chooser,P]) = limit.last(this, n)
  def exactly(n: Int)(implicit limit: Limit[Chooser,P]) = limit.exactly(this, n)

  def satisfying[B](pred: B => Boolean)(implicit cond: Conditional[Chooser,P]) = cond.condition[B](this, pred)
}

//TODO: fill in the nitty gritty
object Choose extends Chooser[Plan]

//Think about how to do "last." Perhaps need to feed this a Builder object?
trait IterationScheme{
  def accept[A,B >: A](value: A, sat: B => Boolean): Boolean
}

class AcceptAll extends IterationScheme{
  def accept[A,B >: A](value: A, sat: B => Boolean) = sat(value)
}

class AcceptEvery(n: Int) extends IterationScheme{
  private var step = -1
  
  def accept[A,B >: A](value: A, sat: B => Boolean) ={
    val eval = sat(value)
    if(eval) step += 1
    if(step == n) step = 0

    eval && step == 0
  }
}

class AcceptFirst(n: Int) extends IterationScheme{
  private var count = -1

  def accept[A,B >: A](value: A, sat: B => Boolean) ={
    val eval = sat(value)
    if(eval) count += 1

    eval && count < n
  }
}

class AcceptExactly(n: Int) extends IterationScheme{
  private var count = -1

  def accept[A,B >: A](value: A, sat: B => Boolean) ={
    val eval = sat(value)
    if(eval) count += 1
    if(n <= count) throw new Exception("Exceeded acceptable limit of %s" format n)

    eval
  }
}
