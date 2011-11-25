package com.wheaties.application

import com.wheaties.predicate.Predicate1
import com.wheaties.predicate.Predicate._
import com.wheaties.function.{UnclosedFunctionFactory1, WrappedFunction, ClosedFunctionFactory1}

trait PredicatedAndThen[A,B,C] extends ClosedFunctionFactory1[A,C]{
  val that: ClosedFunctionFactory1[A,B]

  def elif(pred0: Predicate1[B]):ClosedAndThenElif[A,B,C]
  def elseThen(func: Function1[B,C]):PredicatedAndThen[A,B,C] = elseThen(WrappedFunction(func))
  def elseThen(func: ClosedFunctionFactory1[B,C]):PredicatedAndThen[A,B,C]

  def query(arg0: A) ={
    val (out, outFunc) = that.memoize(arg0)
    outFunc andThen queryNext(out)
  }
  def memoize(arg0: A) ={
    val (out, outFunc) = that.memoize(arg0)
    val (out2, outFunc2) = memoizeNext(out)
    (out2, outFunc andThen outFunc2)
  }

  protected[application] def checkNext(arg0: B):C
  protected[application] def memoizeNext(arg0: B):(C,Function1[B,C])
  protected[application] def queryNext(arg0: B):Function1[B,C]
}

trait ClosedAndThenElif[A,B,C]{
  def apply(func: Function1[B,C]):PredicatedAndThen[A,B,C] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[B,C]):PredicatedAndThen[A,B,C]
}

trait UnclosedAndThen[A,B,C] extends UnclosedFunctionFactory1[A,C]{
  val that: ClosedFunctionFactory1[A,B]

  def elif(pred0: Predicate1[B]):UnclosedAndThenElif[A,B,C]
  def elseThen(func: Function1[B,C]):PredicatedAndThen[A,B,C] = elseThen(WrappedFunction(func))
  def elseThen(func: ClosedFunctionFactory1[B,C]):PredicatedAndThen[A,B,C]

  def query(arg0: A) ={
    val (out, outFunc) = that.memoize(arg0)
    queryNext(out).map(outFunc andThen _)
  }
  def memoize(arg0: A) ={
    val (out, outFunc) = that.memoize(arg0)
    memoizeNext(out).map(result => (result._1, outFunc andThen result._2))
  }

  protected[application] def checkNext(arg0: B):Option[C]
  protected[application] def memoizeNext(arg0: B):Option[(C,Function1[B,C])]
  protected[application] def queryNext(arg0: B):Option[Function1[B,C]]
}

trait UnclosedAndThenElif[A,B,C]{
  def apply(func: Function1[B,C]):UnclosedAndThen[A,B,C] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[B,C]):UnclosedAndThen[A,B,C]
}

/**
 * Applies a predicate to the return of the enclosed function to determine if an andThen operation should be performed.
 */
case class AndThenIf[A,B,C](pred: Predicate[B], that: ClosedFunctionFactory1[A,B], thatTrue: ClosedFunctionFactory1[B,C])
  extends UnclosedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new UnclosedAndThenElif[A,B,C]{
    def apply(thatFalse: ClosedFunctionFactory1[B,C])
      = AndThenEitherIf(pred, that, thatTrue, AndThenIf(pred0, that, thatFalse))
  }
  def elseThen(func: ClosedFunctionFactory1[B,C]) = AndThenEither(pred, that, thatTrue, AndThenElse(that, func))

  def apply(arg: A) = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) Some(thatTrue(arg0)) else None
  protected[application] def memoizeNext(arg0: B) = {
    if(pred(arg0)){
      Some(thatTrue.memoize(arg0))
    }
    else None
  }
  protected[application] def queryNext(arg0: B) = if(pred(arg0)) Some(thatTrue.query(arg0)) else None
}

case class AndThenEitherIf[A,B,C](pred: Predicate[B],
                                  that: ClosedFunctionFactory1[A,B],
                                  thatTrue: ClosedFunctionFactory1[B,C],
                                  thatFalse: UnclosedAndThen[A,B,C])
  extends UnclosedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new UnclosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseThen(func: ClosedFunctionFactory1[B,C]) = AndThenEither(pred, that, thatTrue, thatFalse.elseThen(func))

  def apply(arg: A) = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) Some(thatTrue(arg0)) else thatFalse.checkNext(arg0)
  protected[application] def memoizeNext(arg0: B) = {
    if(pred(arg0)){
      Some(thatTrue.memoize(arg0))
    }
    else thatFalse.memoizeNext(arg0)
  }
  protected[application] def queryNext(arg0: B) ={
    if(pred(arg0)) Some(thatTrue.query(arg0)) else thatFalse.queryNext(arg0)
  }
}

/**
 * Applies a predicate to the return of the enclosed function to determine which function to apply as an andThen operation
 */
case class AndThenEither[A,B,C](pred: Predicate[B],
                                that: ClosedFunctionFactory1[A,B],
                                thatTrue: ClosedFunctionFactory1[B,C],
                                thatFalse: PredicatedAndThen[A,B,C])
  extends PredicatedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new ClosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseThen(func: ClosedFunctionFactory1[B,C]) = copy(thatFalse = thatFalse.elseThen(func))

  def apply(arg: A):C = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) thatTrue(arg0) else thatFalse.checkNext(arg0)
  protected[application] def memoizeNext(arg0: B) ={
    if(pred(arg0)) thatTrue.memoize(arg0) else thatFalse.memoizeNext(arg0)
  }
  protected[application] def queryNext(arg0: B) = if(pred(arg0)) thatTrue.query(arg0) else thatFalse.queryNext(arg0)
}

case class AndThenElse[A,B,C](that: ClosedFunctionFactory1[A,B], thatFalse: ClosedFunctionFactory1[B,C])
  extends PredicatedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new ClosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = AndThenEither(pred0, that, func, AndThenElse(that, thatFalse))
  }
  def elseThen(func: ClosedFunctionFactory1[B,C]) = AndThenElse(that, func)

  def apply(arg0: A) = checkNext(that(arg0))

  protected[application] def checkNext(arg0: B) = thatFalse(arg0)
  protected[application] def memoizeNext(arg0: B) = thatFalse.memoize(arg0)
  protected[application] def queryNext(arg0: B) = thatFalse.query(arg0)
}