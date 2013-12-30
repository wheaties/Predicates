package com.wheaties.predicate.defined

import com.wheaties.partials.PartialFunction2
import com.wheaties.ops.FunctionOps2

class DefinedAt2[-T1, -T2, R](f: (T1, T2) => R, p: (T1, T2) => Boolean) extends PartialFunction2[T1, T2, R]{
  import FunctionOps2._

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
