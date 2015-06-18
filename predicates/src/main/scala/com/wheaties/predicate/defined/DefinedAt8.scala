package com.wheaties.predicate.defined

import com.wheaties.predicate.partials.PartialFunction8

class DefinedAt8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, p: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean)
			extends PartialFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R]{
	import com.wheaties.predicate._
	def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8) = if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else throw new NotDefinedForException(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

	override def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => RR): RR = if(p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8) = p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

	def orAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => Boolean) = new DefinedAt8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, R](f, p or q)
	def andAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => Boolean) = new DefinedAt8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, R](f, p and q)
	def xorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => Boolean) = new DefinedAt8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, R](f, p xor q)
	def norAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => Boolean) = new DefinedAt8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, R](f, p nor q)
	def nandAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => Boolean) = new DefinedAt8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, R](f, p nand q)
	def nxorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => Boolean) = new DefinedAt8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, R](f, p nxor q)

}

object DefinedAt8{
	implicit class F2DefinedAt8[T1, T2, T3, T4, T5, T6, T7, T8](f: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean){
		def definedAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](pred: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => Boolean) = new DefinedAt8(f, pred)
	}
}