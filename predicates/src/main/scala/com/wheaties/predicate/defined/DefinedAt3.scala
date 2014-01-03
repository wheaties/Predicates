package com.wheaties.predicate.defined

import com.wheaties.predicate.ops.FunctionOps3
import com.wheaties.predicate.partials.PartialFunction3

class DefinedAt3[-T1, -T2, -T3, R](f: (T1, T2, T3) => R, p: (T1, T2, T3) => Boolean)
			extends PartialFunction3[T1, T2, T3, R]{
	import FunctionOps3._
	def apply(arg1: T1, arg2: T2, arg3: T3) = if(isDefinedAt(arg1, arg2, arg3)) f(arg1, arg2, arg3) else throw new NotDefinedForException(arg1, arg2, arg3)

	override def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, default: (TT1, TT2, TT3) => RR): RR = if(p(arg1, arg2, arg3)) f(arg1, arg2, arg3) else default(arg1, arg2, arg3)

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3) = p(arg1, arg2, arg3)

	def orAt[TT1 <: T1, TT2 <: T2, TT3 <: T3](q: (TT1, TT2, TT3) => Boolean) = new DefinedAt3[TT1, TT2, TT3, R](f, p or q)
	def andAt[TT1 <: T1, TT2 <: T2, TT3 <: T3](q: (TT1, TT2, TT3) => Boolean) = new DefinedAt3[TT1, TT2, TT3, R](f, p and q)
	def xorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3](q: (TT1, TT2, TT3) => Boolean) = new DefinedAt3[TT1, TT2, TT3, R](f, p xor q)
	def norAt[TT1 <: T1, TT2 <: T2, TT3 <: T3](q: (TT1, TT2, TT3) => Boolean) = new DefinedAt3[TT1, TT2, TT3, R](f, p nor q)
	def nandAt[TT1 <: T1, TT2 <: T2, TT3 <: T3](q: (TT1, TT2, TT3) => Boolean) = new DefinedAt3[TT1, TT2, TT3, R](f, p nand q)
	def nxorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3](q: (TT1, TT2, TT3) => Boolean) = new DefinedAt3[TT1, TT2, TT3, R](f, p nxor q)

}

object DefinedAt3{
	implicit class F2DefinedAt3[T1, T2, T3](f: (T1, T2, T3) => Boolean){
		def definedAt[TT1 <: T1, TT2 <: T2, TT3 <: T3](pred: (TT1, TT2, TT3) => Boolean) = new DefinedAt3(f, pred)
	}
}