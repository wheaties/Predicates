package com.wheaties.choice

sealed trait Taker
trait TakeS extends Taker  //Type for Take with a defined satisfiability
trait TakeL extends Taker  //Type for Take with a defined limit
trait TakeSL extends Taker //Type for Take with both a limit ad a satisfiability

trait Take[A <: Taker] extends Choice with Predicate-Like[Take[A]]{
  def every(n: Int)(implicit limit: Limit[A]) = limit.every(this, n)
  def all(implicit limit: Limit[A]) = limit.all(this)
  def first(n: Int)(implicit limit: Limit[A]) = limit.first(this, n)
  def last(n: Int)(implicit limit: Limit[A]) = limit.last(this, n)
  def exactly(n: Int)(implicit limit: Limit[A]) = limit.exactly(this, n)

  def satisfying[B](pred: B => Boolean)(implicit cond: Conditional[A]) = cond.condition[B](this, pred)
}