package com.wheaties.application

import com.wheaties.predicate.Predicate1
import com.wheaties.predicate.Predicate._

//TODO: make the func wrapped itself a function factory
//TODO: also make the AndThenElse a function factory
//TODO: add an apply to the open and closed AndThenElif, define the func1 in terms of the funcfactory version
//TODO: rethink query and querynext...
trait PredicatedAndThen[A,B,C] extends ClosedFunctionFactory1[A,C]{
  def elif(pred0: Predicate1[B]):ClosedAndThenElif[A,B,C]
  def elseThen(func: Function1[B,C]):PredicatedAndThen[A,B,C]

  protected[application] def checkNext(arg0: B):C
  protected[application] def memoizeNext(arg0: B):(C,Function1[A,C])
  protected[application] def queryNext(arg0: B):Function1[A,C]
}

trait ClosedAndThenElif[A,B,C]{
  def apply(func: Function1[B,C]):PredicatedAndThen[A,B,C] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[B,C]):PredicatedAndThen[A,B,C]
  def apply[D](func: PredicatedAndThen[B,D,C]):PredicatedAndThen[A,B,C]
}

trait UnclosedAndThen[A,B,C] extends UnclosedFunctionFactory1[A,C]{
  def elif(pred0: Predicate1[B]):UnclosedAndThenElif[A,B,C]
  def elseThen(func: Function1[B,C]):PredicatedAndThen[A,B,C]

  protected[application] def checkNext(arg0: B):Option[C]
  protected[application] def memoizeNext(arg0: B):Option[(C,Function1[A,C])]
  protected[application] def queryNext(arg0: B):Option[Function1[A,C]]
}

trait UnclosedAndThenElif[A,B,C]{
  def apply(func: Function1[B,C]):UnclosedAndThen[A,B,C] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[B,C]):UnclosedAndThen[A,B,C]
  def apply[D](func: PredicatedAndThen[B,D,C]):UnclosedAndThen[A,B,C]
}

/**
 * Applies a predicate to the return of the enclosed function to determine if an andThen operation should be performed.
 */
case class AndThenIf[A,B,C](pred: Predicate[B], that: ClosedFunctionFactory1[A,B], thatTrue: ClosedFunctionFactory1[B,C])
  extends UnclosedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new UnclosedAndThenElif[A,B,C]{
    def apply(thatFalse: ClosedFunctionFactory1[B,C])
      = AndThenEitherIf(pred, that, thatTrue, AndThenIf(pred0, that, thatFalse))
    def apply[D](thatFalse: PredicatedAndThen[B,D,C])
      = AndThenEitherIf(pred, that, thatTrue, AndThenIf2(pred0, that, thatFalse))
  }
  def elseThen(func: Function1[B,C]) = AndThenEither(pred, that, thatTrue, AndThenElse(that, func))

  def query(arg0: A) = queryNext(that(arg0))
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg: A) = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) Some(thatTrue(arg0)) else None
  protected[application] def memoizeNext(arg0: B) = {
    if(pred(arg0)){
      Some((thatTrue(arg0), that andThen thatTrue))
    }
    else None
  }
  protected[application] def queryNext(arg0: B) = if(pred(arg0)) Some(that andThen thatTrue) else None
}

case class AndThenEitherIf[A,B,C](pred: Predicate[B],
                                  that: Function[A,B],
                                  thatTrue: Function[B,C],
                                  thatFalse: UnclosedAndThen[A,B,C])
  extends UnclosedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new UnclosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[D](func: PredicatedAndThen[B,D,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseThen(func: Function1[B,C]) = AndThenEither(pred, that, thatTrue, thatFalse.elseThen(func))

  def query(arg0: A) = queryNext(that(arg0))
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg: A) = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) Some(thatTrue(arg0)) else thatFalse.checkNext(arg0)
  protected[application] def memoizeNext(arg0: B) = {
    if(pred(arg0)){
      Some((thatTrue(arg0), that andThen thatTrue))
    }
    else thatFalse.memoizeNext(arg0)
  }
  protected[application] def queryNext(arg0: B) ={
    if(pred(arg0)) Some(that andThen thatTrue) else thatFalse.queryNext(arg0)
  }
}

/**
 * Applies a predicate to the return of the enclosed function to determine which function to apply as an andThen operation
 */
case class AndThenEither[A,B,C](pred: Predicate[B],
                                that: Function[A,B],
                                thatTrue: Function1[B,C],
                                thatFalse: PredicatedAndThen[A,B,C])
  extends PredicatedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new ClosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[D](func: PredicatedAndThen[B,D,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseThen(func: Function1[B,C]) = copy(thatFalse = thatFalse.elseThen(func))

  def query(arg0: A) = queryNext(that(arg0))
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg: A):C = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) thatTrue(arg0) else thatFalse.checkNext(arg0)
  protected[application] def memoizeNext(arg0: B) ={
    if(pred(arg0)) (thatTrue(arg0), that andThen thatTrue) else thatFalse.memoizeNext(arg0)
  }
  protected[application] def queryNext(arg0: B) = if(pred(arg0)) that andThen thatTrue else thatFalse.queryNext(arg0)
}

case class AndThenElse[A,B,C](that: Function1[A,B], thatFalse: Function1[B,C]) extends PredicatedAndThen[A,B,C]{
  def elif(pred0: Predicate1[B]) = new ClosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = AndThenEither(pred0, that, func, AndThenElse(that, thatFalse))
    def apply[D](func: PredicatedAndThen[B,D,C]) = AndThenEither2(pred0, that, func, AndThenElse(that, thatFalse))
  }
  def elseThen(func: Function1[B,C]) = AndThenElse(that, func)

  def query(arg0: A) = that andThen thatFalse
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg0: A) = checkNext(that(arg0))

  protected[application] def checkNext(arg0: B) = thatFalse(arg0)
  protected[application] def memoizeNext(arg0: B) = (thatFalse(arg0), that andThen thatFalse)
  protected[application] def queryNext(arg0: B) = that andThen thatFalse
}

case class AndThenIf2[A,B,C,D](pred: Predicate[B], that: ClosedFunctionFactory1[A,B], thatTrue: PredicatedAndThen[B,D,C])
  extends UnclosedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new UnclosedAndThenElif[A,B,C]{
    def apply(thatFalse: ClosedFunctionFactory1[B,C])
      = AndThenEitherIf(pred, that, thatTrue, AndThenIf(pred0, that, thatFalse))
    def apply[D](thatFalse: PredicatedAndThen[B,D,C])
      = AndThenEitherIf(pred, that, thatTrue, AndThenIf2(pred0, that, thatFalse))
  }
  def elseThen(func: Function1[B,C]) = AndThenEither(pred, that, thatTrue, AndThenElse(that, func))

  def query(arg0: A) = queryNext(that(arg0))
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg: A) = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) Some(thatTrue(arg0)) else None
  protected[application] def memoizeNext(arg0: B) = {
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      Some((out, that andThen outFunc))
    }
    else None
  }
  protected[application] def queryNext(arg0: B) = if(pred(arg0)) Some(that andThen thatTrue.query(arg0)) else None
}

//TODO: this one needs an elif "D"
case class AndThenEitherIf2[A,B,C,D](pred: Predicate[B],
                                    that: Function[A,B],
                                    thatTrue: PredicatedAndThen[B,D,C],
                                    thatFalse: UnclosedAndThen[A,B,C])
  extends UnclosedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new UnclosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[D](func: PredicatedAndThen[B,D,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseThen(func: Function1[B,C]) = AndThenEither(pred, that, thatTrue, thatFalse.elseThen(func))

  def query(arg0: A) = queryNext(that(arg0))
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg: A) = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) Some(thatTrue(arg0)) else thatFalse.checkNext(arg0)
  protected[application] def memoizeNext(arg0: B) = {
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      Some((out, that andThen outFunc))
    }
    else thatFalse.memoizeNext(arg0)
  }
  protected[application] def queryNext(arg0: B) ={
    if(pred(arg0)) Some(that andThen thatTrue.query(arg0)) else thatFalse.queryNext(arg0)
  }
}

case class AndThenEither2[A,B,C,D](pred: Predicate[B],
                                  that: Function[A,B],
                                  thatTrue: PredicatedAndThen[B,D,C],
                                  thatFalse: PredicatedAndThen[A,B,C])
  extends PredicatedAndThen[A,B,C]{

  def elif(pred0: Predicate1[B]) = new ClosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[D](func: PredicatedAndThen[B,D,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseThen(func: Function1[B,C]) = copy(thatFalse = thatFalse.elseThen(func))

  def query(arg0: A) = queryNext(that(arg0))
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg: A):C = checkNext(that(arg))

  protected[application] def checkNext(arg0: B) = if(pred(arg0)) thatTrue(arg0) else thatFalse.checkNext(arg0)
  protected[application] def memoizeNext(arg0: B) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      (out, that andThen outFunc)
    }
    else thatFalse.memoizeNext(arg0)
  }
  protected[application] def queryNext(arg0: B) ={
    if(pred(arg0)) that andThen thatTrue.query(arg0) else thatFalse.queryNext(arg0)
  }
}

case class AndThenElse2[A,B,C,D](that: Function1[A,B], thatFalse: PredicatedAndThen[B,D,C]) extends PredicatedAndThen[A,B,C]{
  def elif(pred0: Predicate1[B]) = new ClosedAndThenElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[B,C]) = AndThenEither(pred0, that, func, AndThenElse2.this)
    def apply[D](func: PredicatedAndThen[B,D,C]) = AndThenEither2(pred0, that, func, AndThenElse2.this)
  }
  def elseThen(func: Function1[B,C]) = AndThenElse(that, func)

  def query(arg0: A) = queryNext(that(arg0))
  def memoize(arg0: A) = memoizeNext(that(arg0))

  def apply(arg0: A) = checkNext(that(arg0))

  protected[application] def checkNext(arg0: B) = thatFalse(arg0)
  protected[application] def memoizeNext(arg0: B) ={
    val (out, outFunc) = thatFalse.memoize(arg0)
    (out, that andThen outFunc)
  }
  protected[application] def queryNext(arg0: B) = that andThen thatFalse.query(arg0)
}