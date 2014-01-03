package com.wheaties.predicate.partials

trait PartialFunction9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => R){
	import PartialFunction9._

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, RR >: R](that: PartialFunction9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, RR]): PartialFunction9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, RR] =
		new orElse9(this, that)

	def lift: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Option[R] = new Lifted9(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) apply(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
}

object PartialFunction9{
	private class orElse9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R](p: PartialFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R], q: PartialFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R])
			extends PartialFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, q)
	}

	private class Lifted9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R](pf: PartialFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any = (_, _, _, _, _, _, _, _, _) => null
}