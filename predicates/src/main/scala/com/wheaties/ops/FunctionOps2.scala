package com.wheaties.ops

import com.wheaties.partials.PartialFunction2
import com.wheaties.predicate.Predicate2
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

trait FunctionOps2 {
  class DefinedAt2[-T1, -T2, R](f: (T1, T2) => R, p: (T1, T2) => Boolean) extends PartialFunction2[T1, T2, R]{
    def apply(arg1: T1, arg2: T2) = if(p(arg1, arg2)) f(arg1, arg2) else throw new IllegalArgumentException((arg1, arg2) toString ())

    override def applyOrElse[TT1 <: T1, TT2 <: T2, RR >: R](arg1: TT1, arg2: TT2, default: (TT1, TT2) => RR): RR =
      if(p(arg1, arg2)) f(arg1, arg2) else default(arg1, arg2)

    def isDefinedAt(arg1: T1, arg2: T2) = p(arg1, arg2)

    def orAt[TT1 <: T1, TT2 <: T2](q: (TT1, TT2) => Boolean) = new DefinedAt2(f, p or q)
    def andAt[TT1 <: T1, TT2 <: T2](q: (TT1, TT2) => Boolean) = new DefinedAt2(f, p and q)
    def xorAt[TT1 <: T1, TT2 <: T2](q: (TT1, TT2) => Boolean) = new DefinedAt2(f, p xor q)
    def norAt[TT1 <: T1, TT2 <: T2](q: (TT1, TT2) => Boolean) = new DefinedAt2(f, p nor q)
    def nandAt[TT1 <: T1, TT2 <: T2](q: (TT1, TT2) => Boolean) = new DefinedAt2(f, p nand q)
    def nxorAt[TT1 <: T1, TT2 <: T2](q: (TT1, TT2) => Boolean) = new DefinedAt2(f, p nxor q)
  }

  implicit class F2DefinedAt2[T1, T2, R](f: (T1, T2) => R){
    def definedAt[TT1 <: T1, TT2 <: TT1](pred: (TT1, TT2) => Boolean) = new DefinedAt2(f, pred)
  }

  implicit class F2P2[T1, T2](f: (T1, T2) => Boolean) extends Predicate2[T1, T2]{
    def apply(arg1: T1, arg2: T2) = f(arg1, arg2)
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
