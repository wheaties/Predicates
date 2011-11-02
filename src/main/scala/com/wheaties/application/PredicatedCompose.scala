package com.wheaties.application

import com.wheaties.predicate.Predicate1
import com.wheaties.predicate.Predicate._


trait PredicatedCompose[A,B,C] extends Function1[C,B]{
  def elif(pred0: Predicate1[C]):ClosedComposeElif[A,B,C]
  def elseCompose(func: Function1[C,A]):PredicatedCompose[A,B,C]
  def elseCompose[D](func: PredicatedCompose[D,A,C]):PredicatedCompose[A,B,C]

  def query(arg0: C):Function1[C,B]
  def memoize(arg0: C):(B,Function1[C,B])
}

trait ClosedComposeElif[A,B,C]{
  def apply(func: Function1[C,A]):PredicatedCompose[A,B,C]
  def apply[D](func: PredicatedCompose[D,A,C]):PredicatedCompose[A,B,C]
}

trait UnclosedCompose[A,B,C] extends Function1[C,Option[B]]{
  def elif(pred0: Predicate1[C]):UnclosedComposeElif[A,B,C]
  def elseCompose(func: Function1[C,A]):PredicatedCompose[A,B,C]
  def elseCompose[D](func: PredicatedCompose[D,A,C]):PredicatedCompose[A,B,C]

  def query(arg0: C):Option[Function1[C,B]]
  def memoize(arg0: C):Option[(B,Function1[C,B])]
}

trait UnclosedComposeElif[A,B,C]{
  def apply(func: Function1[C,A]):UnclosedCompose[A,B,C]
  def apply[D](func: PredicatedCompose[D,A,C]):UnclosedCompose[A,B,C]
}

/**
 * Applies a predicate on the argument to determine if a compose operation should be performed.
 */
case class ComposeIf[A,B,C](pred: Predicate[C], that: Function[A,B], thatTrue: Function[C,A])
  extends UnclosedCompose[A,B,C]{

  def elif(pred0: Predicate1[C]) = new UnclosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = ComposeEitherIf(pred, that, thatTrue, ComposeIf(pred0, that, func))
    def apply[D](func: PredicatedCompose[D,A,C]) = ComposeEitherIf(pred, that, thatTrue, ComposeIf2(pred0, that, func))
  }
  def elseCompose(thatFalse: Function1[C,A]) = ComposeEither(pred, that, thatTrue, ComposeElse(that, thatFalse))
  def elseCompose[D](thatFalse: PredicatedCompose[D,A,C])
    = ComposeEither(pred, that, thatTrue, ComposeElse(that, thatFalse))

  def query(arg0: C) = if(pred(arg0)) Some(that compose thatTrue) else None
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val outFunc = that compose thatTrue
      Some((outFunc(arg0), outFunc))
    }
    else None
  }

  def apply(arg: C) = if(pred(arg)) Some(that(thatTrue(arg))) else None
}

case class ComposeEitherIf[A,B,C](pred: Predicate[C],
                                  that: Function[A,B],
                                  thatTrue: Function[C,A],
                                  thatFalse: UnclosedCompose[A,B,C])
  extends UnclosedCompose[A,B,C] {

  def elif(pred0: Predicate1[C]) = new UnclosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[D](func: PredicatedCompose[D,A,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseCompose(func: Function1[C,A]) = ComposeEither(pred, that, thatTrue, thatFalse.elseCompose(func))
  def elseCompose[D](func: PredicatedCompose[D,A,C]) = ComposeEither(pred, that, thatTrue, thatFalse.elseCompose(func))

  def query(arg0: C) = if(pred(arg0)) Some(that compose thatTrue) else thatFalse.query(arg0)
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val outFunc = that compose thatTrue
      Some((outFunc(arg0), outFunc))
    }
    else thatFalse.memoize(arg0)
  }

  def apply(arg: C) = if(pred(arg)) Some(that(thatTrue(arg))) else thatFalse(arg)
}

case class ComposeEither[A,B,C](pred: Predicate[C],
                                that: Function[A,B],
                                thatTrue: Function[C,A],
                                thatFalse: PredicatedCompose[A,B,C])
  extends PredicatedCompose[A,B,C] {

  def elif(pred0: Predicate1[C]) = new ClosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[D](func: PredicatedCompose[D,A,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseCompose(func: Function1[C,A]) = copy(thatFalse = thatFalse.elseCompose(func))
  def elseCompose[D](func: PredicatedCompose[D,A,C]) = copy(thatFalse = thatFalse.elseCompose(func))

  def query(arg0: C) = if(pred(arg0)) that compose thatTrue else thatFalse.query(arg0)
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val outFunc = that compose thatTrue
      (outFunc(arg0), outFunc)
    }
    else thatFalse.memoize(arg0)
  }

  def apply(arg: C) = if(pred(arg)) that(thatTrue(arg)) else thatFalse(arg)
}

case class ComposeElse[A,B,C](that: Function[A,B], thatFalse: Function[C,A]) extends PredicatedCompose[A,B,C]{
  def elif(pred0: Predicate1[C]) = new ClosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = ComposeEither(pred0, that, func, ComposeElse.this)
    def apply[D](func: PredicatedCompose[D,A,C]) = ComposeEither2(pred0, that, func, ComposeElse.this)
  }
  def elseCompose(func: Function1[C,A]) = copy(thatFalse = func)
  def elseCompose[D](func: PredicatedCompose[D,A,C]) = ComposeElse2(that, func)

  def query(arg0: C) = that compose thatFalse
  def memoize(arg0: C) = (apply(arg0), query(arg0))

  def apply(arg: C) = that(thatFalse(arg))
}

//Classes for which the "true" composition is, itself, a nested predicated composition
case class ComposeIf2[A,B,C,D](pred: Predicate[C], that: Function[A,B], thatTrue: PredicatedCompose[D,A,C])
  extends UnclosedCompose[A,B,C]{

  def elif(pred0: Predicate1[C]) = new UnclosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = ComposeEitherIf2(pred, that, thatTrue, ComposeIf(pred0, that, func))
    def apply[E](func: PredicatedCompose[E,A,C]) = ComposeEitherIf2(pred, that, thatTrue, ComposeIf2(pred0, that, func))
  }
  def elseCompose(thatFalse: Function1[C,A]) = ComposeEither2(pred, that, thatTrue, ComposeElse(that, thatFalse))
  def elseCompose[E](thatFalse: PredicatedCompose[E,A,C])
    = ComposeEither2(pred, that, thatTrue, ComposeElse2(that, thatFalse))

  def query(arg0: C) = if(pred(arg0)) Some(that compose thatTrue.query(arg0)) else None
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      Some((that(out), that compose outFunc))
    }
    else None
  }

  def apply(arg: C) = if(pred(arg)) Some(that(thatTrue(arg))) else None
}

case class ComposeEitherIf2[A,B,C,D](pred: Predicate[C],
                                    that: Function[A,B],
                                    thatTrue: PredicatedCompose[D,A,C],
                                    thatFalse: UnclosedCompose[A,B,C])
  extends UnclosedCompose[A,B,C] {

  def elif(pred0: Predicate1[C]) = new UnclosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[E](func: PredicatedCompose[E,A,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseCompose(func: Function1[C,A]) = ComposeEither2(pred, that, thatTrue, thatFalse.elseCompose(func))
  def elseCompose[E](func: PredicatedCompose[E,A,C]) = ComposeEither2(pred, that, thatTrue, thatFalse.elseCompose(func))

  def query(arg0: C) = if(pred(arg0)) Some(that compose thatTrue.query(arg0)) else thatFalse.query(arg0)
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      Some((that(out), that compose outFunc))
    }
    else thatFalse.memoize(arg0)
  }

  def apply(arg: C) = if(pred(arg)) Some(that(thatTrue(arg))) else thatFalse(arg)
}

case class ComposeEither2[A,B,C,D](pred: Predicate[C],
                                   that: Function1[A,B],
                                   thatTrue: PredicatedCompose[D,A,C],
                                   thatFalse: PredicatedCompose[A,B,C])
  extends PredicatedCompose[A,B,C]{

  def elif(pred0: Predicate1[C]) = new ClosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = copy(thatFalse = thatFalse.elif(pred0)(func))
    def apply[E](func: PredicatedCompose[E,A,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
  }
  def elseCompose(func: Function1[C,A]) = copy(thatFalse = thatFalse.elseCompose(func))
  def elseCompose[E](func: PredicatedCompose[E,A,C]) = copy(thatFalse = thatFalse.elseCompose(func))

  def query(arg0: C) = if(pred(arg0)) that compose thatTrue.query(arg0) else thatFalse.query(arg0)
  def memoize(arg0: C) ={
    if(pred(arg0)){
      val (out, outFunc) = thatTrue.memoize(arg0)
      (that(out), that compose outFunc)
    }
    else thatFalse.memoize(arg0)
  }

  def apply(arg: C) = if(pred(arg)) that(thatTrue(arg)) else thatFalse(arg)
}

case class ComposeElse2[A,B,C,D](that: Function1[A,B], thatFalse: PredicatedCompose[D,A,C])
  extends PredicatedCompose[A,B,C]{

  def elif(pred0: Predicate1[C]) = new ClosedComposeElif[A,B,C]{
    def apply(func: Function1[C,A]) = ComposeEither(pred0, that, func, ComposeElse2.this)
    def apply[E](func: PredicatedCompose[E,A,C]) = ComposeEither2(pred0, that, func, ComposeElse2.this)
  }
  def elseCompose(func: Function1[C,A]) = ComposeElse(that, func)
  def elseCompose[E](func: PredicatedCompose[E,A,C]) = ComposeElse2(that, func)

  def query(arg0: C) = that compose thatFalse.query(arg0)
  def memoize(arg0: C) = (apply(arg0), query(arg0))

  def apply(arg: C) = that(thatFalse(arg))
}