package com.wheaties.ops

import com.wheaties.predicate.Predicate1
import com.wheaties.logical.{Negation, Disjunction, Conjunction}
import com.wheaties.predicate.defined.DefinedAt

object FunctionOps1{
  implicit class F2DefinedAt[T1, R](f: T1 => R){
    def definedAt[TT <: T1](pred: TT => Boolean) = new DefinedAt(f, pred)
  }

  implicit class F2P1[T1](f: T1 => Boolean) extends Predicate1[T1]{
    def apply(arg1: T1) = f(arg1)
  }

  implicit def conj1[T1] = new Conjunction[T1 => Boolean] {
    def conjunction(p: T1 => Boolean, q: T1 => Boolean) = p and q
  }

  implicit def disj1[T1] = new Disjunction[T1 => Boolean] {
    def disjunction(p: T1 => Boolean, q: T1 => Boolean) = p or q
  }

  implicit def neg1[T1] = new Negation[T1 => Boolean] {
    def not(p: T1 => Boolean) = new Predicate1[T1] {
      def apply(arg1: T1) = !p(arg1)
    }
  }
}
