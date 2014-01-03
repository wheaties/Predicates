package com.wheaties.predicate.defined

import com.wheaties.predicate.ops.FunctionOps10
import com.wheaties.predicate.partials.PartialFunction10

class DefinedAt10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R, p: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean)
			extends PartialFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]{
	import FunctionOps10._
	def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10) = if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) else throw new NotDefinedForException(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)

	override def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9, arg10: TT10, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => RR): RR = if(p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10) = p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)

	def orAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => Boolean) = new DefinedAt10[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, R](f, p or q)
	def andAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => Boolean) = new DefinedAt10[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, R](f, p and q)
	def xorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => Boolean) = new DefinedAt10[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, R](f, p xor q)
	def norAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => Boolean) = new DefinedAt10[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, R](f, p nor q)
	def nandAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => Boolean) = new DefinedAt10[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, R](f, p nand q)
	def nxorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10](q: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => Boolean) = new DefinedAt10[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, R](f, p nxor q)

}

object DefinedAt10{
	implicit class F2DefinedAt10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean){
		def definedAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10](pred: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10) => Boolean) = new DefinedAt10(f, pred)
	}
}