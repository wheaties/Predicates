package com.wheaties.application

import com.wheaties.predicate.Predicate1
import com.wheaties.predicate.Predicate._
import com.wheaties.function.{WrappedFunction, UnclosedFunctionFactory1, ClosedFunctionFactory1}

trait PredicatedCompose[A,B,C] extends ClosedFunctionFactory1[C,B]{
  def elif(pred0: Predicate1[C]):ClosedComposeElif[A,B,C]
  def elseCompose(func: Function1[C,A]):PredicatedCompose[A,B,C] = elseCompose(WrappedFunction(func))
  def elseCompose(func: ClosedFunctionFactory1[C,A]):PredicatedCompose[A,B,C]

  def query(arg0: C):Function1[C,B]
  def memoize(arg0: C):(B,Function1[C,B])
}

trait ClosedComposeElif[A,B,C]{
  def apply(func: Function1[C,A]):PredicatedCompose[A,B,C] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[C,A]):PredicatedCompose[A,B,C]
}

trait UnclosedCompose[A,B,C] extends UnclosedFunctionFactory1[C,B]{
  def elif(pred0: Predicate1[C]):UnclosedComposeElif[A,B,C]
  def elseCompose(func: Function1[C,A]):PredicatedCompose[A,B,C] = elseCompose(WrappedFunction(func))
  def elseCompose(func: ClosedFunctionFactory1[C,A]):PredicatedCompose[A,B,C]

  def query(arg0: C):Option[Function1[C,B]]
  def memoize(arg0: C):Option[(B,Function1[C,B])]
}

trait UnclosedComposeElif[A,B,C]{
  def apply(func: Function1[C,A]):UnclosedCompose[A,B,C] = apply(WrappedFunction(func))
  def apply(func: ClosedFunctionFactory1[C,A]):UnclosedCompose[A,B,C]
}

/**
 * Applies a predicate on the argument to determine if a compose operation should be performed.
 */
case class ComposeIf[A,B,C](pred: Predicate[C], that: ClosedFunctionFactory1[A,B], thatTrue: ClosedFunctionFactory1[C,A])
  extends UnclosedCompose[A,B,C]{

  def elif(pred0: Predicate1[C]) = new UnclosedComposeElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[C,A]) = ComposeEitherIf(pred, that, thatTrue, ComposeIf(pred0, that, func))
  }
  def elseCompose(thatFalse: ClosedFunctionFactory1[C,A])
    = ComposeEither(pred, that, thatTrue, ComposeElse(that, thatFalse))

  def query(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      Some(that.query(out) compose outFunc)
    } else None
  }
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      val (out2, outFunc2) = that.memoize(out)
      Some((out2, outFunc2 compose outFunc))
    }
    else None
  }

  def apply(arg: C) = if(pred(arg)) Some(that(thatTrue(arg))) else None
}

case class ComposeEitherIf[A,B,C](pred: Predicate[C],
                                  that: ClosedFunctionFactory1[A,B],
                                  thatTrue: ClosedFunctionFactory1[C,A],
                                  thatFalse: UnclosedCompose[A,B,C])
  extends UnclosedCompose[A,B,C] {

  def elif(pred0: Predicate1[C]) = new UnclosedComposeElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[C,A]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseCompose(func: ClosedFunctionFactory1[C,A])
    = ComposeEither(pred, that, thatTrue, thatFalse.elseCompose(func))

  def query(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      Some(that.query(out) compose outFunc)
    } else thatFalse.query(arg0)
  }
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      val (out2, outFunc2) = that.memoize(out)
      Some((out2, outFunc2 compose outFunc))
    }
    else thatFalse.memoize(arg0)
  }

  def apply(arg: C) = if(pred(arg)) Some(that(thatTrue(arg))) else thatFalse(arg)
}

case class ComposeEither[A,B,C](pred: Predicate[C],
                                that: ClosedFunctionFactory1[A,B],
                                thatTrue: ClosedFunctionFactory1[C,A],
                                thatFalse: PredicatedCompose[A,B,C])
  extends PredicatedCompose[A,B,C] {

  def elif(pred0: Predicate1[C]) = new ClosedComposeElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[C,A]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseCompose(func: ClosedFunctionFactory1[C,A]) = copy(thatFalse = thatFalse.elseCompose(func))

  def query(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      that.query(out) compose outFunc
    } else thatFalse.query(arg0)
  }
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      val (out2, outFunc2) = that.memoize(out)
      (out2, outFunc2 compose outFunc)
    }
    else thatFalse.memoize(arg0)
  }

  def apply(arg: C) = if(pred(arg)) that(thatTrue(arg)) else thatFalse(arg)
}

case class ComposeElse[A,B,C](that: ClosedFunctionFactory1[A,B], thatFalse: ClosedFunctionFactory1[C,A])
  extends PredicatedCompose[A,B,C]{

  def elif(pred0: Predicate1[C]) = new ClosedComposeElif[A,B,C]{
    def apply(func: ClosedFunctionFactory1[C,A]) = ComposeEither(pred0, that, func, ComposeElse.this)
  }
  def elseCompose(func: ClosedFunctionFactory1[C,A]) = copy(thatFalse = func)

  def query(arg0: C) ={
    val (out, outFunc) = thatFalse.memoize(arg0)
    that.query(out) compose outFunc
  }
  def memoize(arg0: C) ={
    val (out, outFunc) = thatFalse.memoize(arg0)
    val (out2, outFunc2) = that.memoize(out)
    (out2, outFunc2 compose outFunc)
  }

  def apply(arg: C) = that(thatFalse(arg))
}