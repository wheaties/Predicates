package com.wheaties.predicate.defined

import com.wheaties.predicate.ops.FunctionOps6
import com.wheaties.predicate.partials.PartialFunction6

class DefinedAt6[-T1, -T2, -T3, -T4, -T5, -T6, R](f: (T1, T2, T3, T4, T5, T6) => R, p: (T1, T2, T3, T4, T5, T6) => Boolean)
			extends PartialFunction6[T1, T2, T3, T4, T5, T6, R]{
	import FunctionOps6._
	def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6) = if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6)) f(arg1, arg2, arg3, arg4, arg5, arg6) else throw new NotDefinedForException(arg1, arg2, arg3, arg4, arg5, arg6)

	override def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, default: (TT1, TT2, TT3, TT4, TT5, TT6) => RR): RR = if(p(arg1, arg2, arg3, arg4, arg5, arg6)) f(arg1, arg2, arg3, arg4, arg5, arg6) else default(arg1, arg2, arg3, arg4, arg5, arg6)

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6) = p(arg1, arg2, arg3, arg4, arg5, arg6)

	def orAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](q: (TT1, TT2, TT3, TT4, TT5, TT6) => Boolean) = new DefinedAt6[TT1, TT2, TT3, TT4, TT5, TT6, R](f, p or q)
	def andAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](q: (TT1, TT2, TT3, TT4, TT5, TT6) => Boolean) = new DefinedAt6[TT1, TT2, TT3, TT4, TT5, TT6, R](f, p and q)
	def xorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](q: (TT1, TT2, TT3, TT4, TT5, TT6) => Boolean) = new DefinedAt6[TT1, TT2, TT3, TT4, TT5, TT6, R](f, p xor q)
	def norAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](q: (TT1, TT2, TT3, TT4, TT5, TT6) => Boolean) = new DefinedAt6[TT1, TT2, TT3, TT4, TT5, TT6, R](f, p nor q)
	def nandAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](q: (TT1, TT2, TT3, TT4, TT5, TT6) => Boolean) = new DefinedAt6[TT1, TT2, TT3, TT4, TT5, TT6, R](f, p nand q)
	def nxorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](q: (TT1, TT2, TT3, TT4, TT5, TT6) => Boolean) = new DefinedAt6[TT1, TT2, TT3, TT4, TT5, TT6, R](f, p nxor q)

}

object DefinedAt6{
	implicit class F2DefinedAt6[T1, T2, T3, T4, T5, T6](f: (T1, T2, T3, T4, T5, T6) => Boolean){
		def definedAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](pred: (TT1, TT2, TT3, TT4, TT5, TT6) => Boolean) = new DefinedAt6(f, pred)
	}
}