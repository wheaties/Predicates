package com.wheaties.application

import com.wheaties.predicate.Predicate._
import com.wheaties.predicate.Predicate1

object PredicatedComposition{
  implicit def func2pred[A,B](func: Function1[A,B]) = new PredicatedComposition[A,B] { val wrapped = func }
}

trait PredicatedComposition[A,B]{
  val wrapped: Function1[A,B]
  implicit def andThenIf[C](pred: Predicate[B]) = new UnclosedAndThenElif[A,B,C]{
    def apply(that: ClosedFunctionFactory1[B,C]) = AndThenIf(pred, WrappedFunction(wrapped), that)
    def apply[D](that: PredicatedAndThen[B,D,C]) = AndThenIf2(pred, WrappedFunction(wrapped), that)
  }
  implicit def composeIf[C](pred: Predicate[C]) = new UnclosedComposeElif[A,B,C]{
    def apply(that: Function[C,A]) = ComposeIf(pred, wrapped, that)
    def apply[D](that: PredicatedCompose[D,A,C]) = ComposeIf2(pred, wrapped, that)
  }
}

