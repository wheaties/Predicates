package com.wheaties.predicate.defined

import com.wheaties.predicate.partials.PartialFunction7

class DefinedAt7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R, p: (T1, T2, T3, T4, T5, T6, T7) => Boolean)
			extends PartialFunction7[T1, T2, T3, T4, T5, T6, T7, R]{
	import com.wheaties.predicate._
	def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7) = if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7)) f(arg1, arg2, arg3, arg4, arg5, arg6, arg7) else throw new NotDefinedForException(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

	override def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => RR): RR = if(p(arg1, arg2, arg3, arg4, arg5, arg6, arg7)) f(arg1, arg2, arg3, arg4, arg5, arg6, arg7) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7) = p(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

	def orAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => Boolean) = new DefinedAt7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, R](f, p or q)
	def andAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => Boolean) = new DefinedAt7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, R](f, p and q)
	def xorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => Boolean) = new DefinedAt7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, R](f, p xor q)
	def norAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => Boolean) = new DefinedAt7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, R](f, p nor q)
	def nandAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => Boolean) = new DefinedAt7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, R](f, p nand q)
	def nxorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => Boolean) = new DefinedAt7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, R](f, p nxor q)

}

object DefinedAt7{
	implicit class F2DefinedAt7[T1, T2, T3, T4, T5, T6, T7](f: (T1, T2, T3, T4, T5, T6, T7) => Boolean){
		def definedAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](pred: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => Boolean) = new DefinedAt7(f, pred)
	}
}