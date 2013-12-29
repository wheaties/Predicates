package com.wheaties.ops

import com.wheaties.predicate.Predicate1
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

trait FunctionOps1{
  class DefinedAt[-T1, +R](f: T1 => R, p: T1 => Boolean) extends PartialFunction[T1, R]{
    def apply(v1: T1): R = if(isDefinedAt(v1)) f(v1) else throw new IllegalArgumentException(v1 toString ())

    override def applyOrElse[TT <: T1, RR >: R](x: TT, default: TT => RR): RR = if(p(x)) f(x) else default(x)

    def isDefinedAt(x: T1) = p(x)

    def orAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p or q)
    def andAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p and q)
    def xorAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p xor q)
    def norAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p nor q)
    def nandAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p nand q)
    def nxorAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p nxor q)
  }

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
