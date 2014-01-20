package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate2
import com.wheaties.logical.{Negation, Disjunction, Conjunction}
import com.wheaties.partials.PartialFunction2

object FunctionOps2 {
  implicit class F2P2[T1, T2](f: (T1, T2) => Boolean) extends Predicate2[T1, T2]{
    def apply(arg1: T1, arg2: T2) = f(arg1, arg2)
  }

  implicit class Untupled2[T1, T2, R](f: PartialFunction[(T1,T2), R]){
    def untupled = new PartialFunction2[T1, T2, R]{
      def isDefinedAt(arg1: T1, arg2: T2) = f isDefinedAt((arg1, arg2))

      def apply(arg1: T1, arg2: T2) = f((arg1, arg2))
    }
  }

  implicit def conj2[T1, T2] = new Conjunction[(T1, T2) => Boolean] {
    def conjunction(p: (T1, T2) => Boolean, q: (T1, T2) => Boolean) = p and q
  }

  implicit def disj2[T1, T2] = new Disjunction[(T1, T2) => Boolean] {
    def disjunction(p: (T1, T2) => Boolean, q: (T1, T2) => Boolean) = p or q
  }

  implicit def neg2[T1, T2] = new Negation[(T1, T2) => Boolean] {
    def not(p: (T1, T2) => Boolean) = new Predicate2[T1, T2] {
      def apply(arg1: T1, arg2: T2) = !p(arg1, arg2)
    }
  }
}
