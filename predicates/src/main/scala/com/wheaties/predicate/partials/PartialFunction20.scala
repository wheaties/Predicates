package com.wheaties.predicate.partials

trait PartialFunction20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R] extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R){
	import PartialFunction20._

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10, TT11 <: T11, TT12 <: T12, TT13 <: T13, TT14 <: T14, TT15 <: T15, TT16 <: T16, TT17 <: T17, TT18 <: T18, TT19 <: T19, TT20 <: T20, RR >: R](that: PartialFunction20[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, TT14, TT15, TT16, TT17, TT18, TT19, TT20, RR]): PartialFunction20[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, TT14, TT15, TT16, TT17, TT18, TT19, TT20, RR] =
		new orElse20(this, that)

	def lift: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Option[R] = new Lifted20(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10, TT11 <: T11, TT12 <: T12, TT13 <: T13, TT14 <: T14, TT15 <: T15, TT16 <: T16, TT17 <: T17, TT18 <: T18, TT19 <: T19, TT20 <: T20, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9, arg10: TT10, arg11: TT11, arg12: TT12, arg13: TT13, arg14: TT14, arg15: TT15, arg16: TT16, arg17: TT17, arg18: TT18, arg19: TT19, arg20: TT20, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, TT14, TT15, TT16, TT17, TT18, TT19, TT20) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)) apply(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
}

object PartialFunction20{
	private class orElse20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R](p: PartialFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R], q: PartialFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R])
			extends PartialFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, q)
	}

	private class Lifted20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R](pf: PartialFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]) extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any = (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => null
}