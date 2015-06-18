package com.wheaties.predicate.defined

import com.wheaties.predicate.partials.PartialFunction5

class DefinedAt5[-T1, -T2, -T3, -T4, -T5, R](f: (T1, T2, T3, T4, T5) => R, p: (T1, T2, T3, T4, T5) => Boolean)
			extends PartialFunction5[T1, T2, T3, T4, T5, R]{
	import com.wheaties.predicate._
	def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5) = if(isDefinedAt(arg1, arg2, arg3, arg4, arg5)) f(arg1, arg2, arg3, arg4, arg5) else throw new NotDefinedForException(arg1, arg2, arg3, arg4, arg5)

	override def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, default: (TT1, TT2, TT3, TT4, TT5) => RR): RR = if(p(arg1, arg2, arg3, arg4, arg5)) f(arg1, arg2, arg3, arg4, arg5) else default(arg1, arg2, arg3, arg4, arg5)

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5) = p(arg1, arg2, arg3, arg4, arg5)

	def orAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](q: (TT1, TT2, TT3, TT4, TT5) => Boolean) = new DefinedAt5[TT1, TT2, TT3, TT4, TT5, R](f, p or q)
	def andAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](q: (TT1, TT2, TT3, TT4, TT5) => Boolean) = new DefinedAt5[TT1, TT2, TT3, TT4, TT5, R](f, p and q)
	def xorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](q: (TT1, TT2, TT3, TT4, TT5) => Boolean) = new DefinedAt5[TT1, TT2, TT3, TT4, TT5, R](f, p xor q)
	def norAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](q: (TT1, TT2, TT3, TT4, TT5) => Boolean) = new DefinedAt5[TT1, TT2, TT3, TT4, TT5, R](f, p nor q)
	def nandAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](q: (TT1, TT2, TT3, TT4, TT5) => Boolean) = new DefinedAt5[TT1, TT2, TT3, TT4, TT5, R](f, p nand q)
	def nxorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](q: (TT1, TT2, TT3, TT4, TT5) => Boolean) = new DefinedAt5[TT1, TT2, TT3, TT4, TT5, R](f, p nxor q)

}

object DefinedAt5{
	implicit class F2DefinedAt5[T1, T2, T3, T4, T5](f: (T1, T2, T3, T4, T5) => Boolean){
		def definedAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](pred: (TT1, TT2, TT3, TT4, TT5) => Boolean) = new DefinedAt5(f, pred)
	}
}