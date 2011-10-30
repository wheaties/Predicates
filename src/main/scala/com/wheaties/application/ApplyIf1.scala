package com.wheaties.application

import com.wheaties.predicate.Predicate1

object ApplyIf1{
  implicit def applyIf[A,B](func: Function1[A,B])(pred: Predicate1[A])= ApplyIf1(pred, func)
}
//TODO: PredicateApplis aren't able to handle other Pred Apps as arguments, i.e. they aren't composable with themselves
//TODO: Need to make a FunctionFactory class with memoize and query methods

trait PredicateApplication1[A,B] extends Function1[A,B]{
  def applyIf(pred0: Predicate1[A])(func: Function1[A,B]):PredicateApplication1[A,B]
  def elif(pred0: Predicate1[A])(func: Function1[A,B]):PredicateApplication1[A,B]
  def elseApply(func: Function1[A,B]):PredicateApplication1[A,B]

  def query(arg0: A):Function1[A,B]
  def memoize(arg0: A):(B,Function1[A,B])
}

trait UnclosedApplication1[A,B] extends Function1[A,Option[B]]{
  def applyIf(pred0: Predicate1[A])(func: Function1[A,B]):UnclosedApplication1[A,B]
  def elif(pred0: Predicate1[A])(func: Function1[A,B]):UnclosedApplication1[A,B]
  def elseApply(func: Function1[A,B]):PredicateApplication1[A,B]

  def query(arg0: A):Option[Function1[A,B]]
  def memoize(arg0: A):Option[(B,Function1[A,B])]
}

case class ApplyIf1[A,B](pred: Predicate1[A], thatTrue: Function1[A,B]) extends UnclosedApplication1[A,B]{
  def applyIf(pred0: Predicate1[A])(func: Function1[A,B]) = ApplyEitherIf1(pred0, func, this)
  def elseApply(func: Function1[A,B]) = ApplyEither1(pred, thatTrue, ApplyElse1(func))
  def elif(pred0: Predicate1[A])(func: Function1[A,B]) = ApplyEitherIf1(pred, thatTrue, ApplyIf1(pred0, func))

  def query(arg0: A) = if(pred(arg0)) Some(thatTrue) else None
  def memoize(arg0: A) = if(pred(arg0)) Some(thatTrue(arg0), thatTrue) else None

  def apply(arg: A) = if(pred(arg)) Some(thatTrue(arg)) else None
}

case class ApplyEitherIf1[A,B](pred: Predicate1[A], thatTrue: Function1[A,B], thatFalse: UnclosedApplication1[A,B])
  extends UnclosedApplication1[A,B]{

  def applyIf(pred0: Predicate1[A])(func: Function1[A,B]) = ApplyEitherIf1(pred0, func, this)
  def elif(pred0: Predicate1[A])(func: Function1[A,B]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  def elseApply(func: Function1[A,B]) = ApplyEither1(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

  def query(arg0: A) = if(pred(arg0)) Some(thatTrue) else thatFalse.query(arg0)
  def memoize(arg0: A) = if(pred(arg0)) Some(thatTrue(arg0), thatTrue) else thatFalse.memoize(arg0)

  def apply(arg: A) = if(pred(arg)) Some(thatTrue(arg)) else thatFalse(arg)
}

case class ApplyEither1[A,B](pred: Predicate1[A], thatTrue: Function1[A,B], thatFalse: PredicateApplication1[A,B])
  extends PredicateApplication1[A,B]{

  def applyIf(pred0: Predicate1[A])(func: Function1[A,B]) = ApplyEither1(pred0, func, this)
  def elif(pred0: Predicate1[A])(func: Function1[A,B]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  def elseApply(func: Function1[A,B]) = copy(thatFalse = thatFalse.elseApply(func))

  def query(arg0: A) = if(pred(arg0)) thatTrue else thatFalse.query(arg0)
  def memoize(arg0: A) = if(pred(arg0)) (thatTrue(arg0), thatTrue) else thatFalse.memoize(arg0)

  def apply(arg: A) = if(pred(arg)) thatTrue(arg) else thatFalse(arg)
}

case class ApplyElse1[A,B](thatFalse: Function1[A,B]) extends PredicateApplication1[A,B]{
  def applyIf(pred0: Predicate1[A])(func: Function1[A,B]) = ApplyEither1(pred0, func, this)
  def elif(pred0: Predicate1[A])(thatTrue: Function1[A,B]) = ApplyEither1(pred0, thatTrue, this)
  def elseApply(func: Function1[A,B]) = ApplyElse1(func)

  def query(arg0: A) = thatFalse
  def memoize(arg0: A) = (apply(arg0), this)

  def apply(arg: A) = thatFalse(arg)
}