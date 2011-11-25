package com.wheaties.application

import com.wheaties.predicate.Predicate1
import com.wheaties.function.{WrappedFunction, UnclosedFunctionFactory1, ClosedFunctionFactory1}

object ApplyIf1{
  implicit def applyIf[A,B](func: Function1[A,B])(pred: Predicate1[A])= ApplyIf1(pred, WrappedFunction(func))
  implicit def applyIf[A,B](func: ClosedFunctionFactory1[A,B])(pred: Predicate1[A])= ApplyIf1(pred, func)
}

trait PredicateApplication1[A,B] extends ClosedFunctionFactory1[A,B]{
  def elif(pred0: Predicate1[A]):ClosedApplicationElif1[A,B]
  def elseApply(func: Function1[A,B]):PredicateApplication1[A,B] = elseApply(WrappedFunction(func))
  def elseApply(func: ClosedFunctionFactory1[A,B]):PredicateApplication1[A,B]
}

trait ClosedApplicationElif1[A,B]{
  def apply(func: Function1[A,B]):PredicateApplication1[A,B] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[A,B]):PredicateApplication1[A,B]
}

trait UnclosedApplication1[A,B] extends UnclosedFunctionFactory1[A,B]{
  def elif(pred0: Predicate1[A]):UnclosedApplicationElif1[A,B]
  def elseApply(func: Function1[A,B]):PredicateApplication1[A,B] = elseApply(WrappedFunction(func))
  def elseApply(func: ClosedFunctionFactory1[A,B]):PredicateApplication1[A,B]
}

trait UnclosedApplicationElif1[A,B]{
  def apply(func: Function1[A,B]):UnclosedApplication1[A,B] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[A,B]):UnclosedApplication1[A,B]
}

case class ApplyIf1[A,B](pred: Predicate1[A], thatTrue: ClosedFunctionFactory1[A,B]) extends UnclosedApplication1[A,B]{
  def elif(pred0: Predicate1[A]) = new UnclosedApplicationElif1[A,B] {
    def apply(func: ClosedFunctionFactory1[A,B])= ApplyEitherIf1(pred, thatTrue, ApplyIf1(pred0, func))
  }
  def elseApply(func: ClosedFunctionFactory1[A,B]) = ApplyEither1(pred, thatTrue, ApplyElse1(func))

  def query(arg0: A) = if(pred(arg0)) Some(thatTrue.query(arg0)) else None
  def memoize(arg0: A) = if(pred(arg0)) Some(thatTrue.memoize(arg0)) else None

  def apply(arg: A) = if(pred(arg)) Some(thatTrue(arg)) else None
}

case class ApplyEitherIf1[A,B](pred: Predicate1[A],
                               thatTrue: ClosedFunctionFactory1[A,B],
                               thatFalse: UnclosedApplication1[A,B])
  extends UnclosedApplication1[A,B]{

  def elif(pred0: Predicate1[A]) = new UnclosedApplicationElif1[A,B] {
    def apply(func: ClosedFunctionFactory1[A,B])= copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseApply(func: ClosedFunctionFactory1[A,B]) = ApplyEither1(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

  def query(arg0: A) = if(pred(arg0)) Some(thatTrue.query(arg0)) else thatFalse.query(arg0)
  def memoize(arg0: A) = if(pred(arg0)) Some(thatTrue.memoize(arg0)) else thatFalse.memoize(arg0)

  def apply(arg: A) = if(pred(arg)) Some(thatTrue(arg)) else thatFalse(arg)
}

case class ApplyEither1[A,B](pred: Predicate1[A],
                             thatTrue: ClosedFunctionFactory1[A,B],
                             thatFalse: PredicateApplication1[A,B])
  extends PredicateApplication1[A,B]{

  def elif(pred0: Predicate1[A]) = new ClosedApplicationElif1[A,B] {
    def apply(func: ClosedFunctionFactory1[A,B]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseApply(func: ClosedFunctionFactory1[A,B]) = copy(thatFalse = thatFalse.elseApply(func))

  def query(arg0: A) = if(pred(arg0)) thatTrue.query(arg0) else thatFalse.query(arg0)
  def memoize(arg0: A) = if(pred(arg0)) thatTrue.memoize(arg0) else thatFalse.memoize(arg0)

  def apply(arg: A) = if(pred(arg)) thatTrue(arg) else thatFalse(arg)
}

case class ApplyElse1[A,B](thatFalse: ClosedFunctionFactory1[A,B]) extends PredicateApplication1[A,B]{

  def elif(pred0: Predicate1[A]) = new ClosedApplicationElif1[A,B] {
    def apply(func: ClosedFunctionFactory1[A,B]) = ApplyEither1(pred0, func, ApplyElse1.this)
  }
  def elseApply(func: ClosedFunctionFactory1[A,B]) = ApplyElse1(func)

  def query(arg0: A) = thatFalse.query(arg0)
  def memoize(arg0: A) = thatFalse.memoize(arg0)

  def apply(arg: A) = thatFalse(arg)
}