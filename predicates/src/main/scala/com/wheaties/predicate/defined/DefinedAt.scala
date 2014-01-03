package com.wheaties.predicate.defined

import com.wheaties.ops.FunctionOps1

class DefinedAt[@specialized(Int,Long,Float,Double) -T1, +R](f: T1 => R, p: T1 => Boolean)
    extends PartialFunction[T1, R]{
  import FunctionOps1._

  def apply(v1: T1): R = if(isDefinedAt(v1)) f(v1) else throw new NotDefinedForException(v1)

  override def applyOrElse[TT <: T1, RR >: R](x: TT, default: TT => RR): RR = if(p(x)) f(x) else default(x)

  def isDefinedAt(x: T1) = p(x)

  def orAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p or q)
  def andAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p and q)
  def xorAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p xor q)
  def norAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p nor q)
  def nandAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p nand q)
  def nxorAt[TT <: T1](q: TT => Boolean) = new DefinedAt[TT, R](f, p nxor q)
}
