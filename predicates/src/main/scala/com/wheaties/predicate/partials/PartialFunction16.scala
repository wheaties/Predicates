package com.wheaties.predicate.partials

trait PartialFunction16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R){
	self =>

	import PartialFunction16._

	override def tupled = new PartialFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), R]{
		def isDefinedAt(v1: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)) = self isDefinedAt(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16)
		def apply(v1: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)) = self(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13, v1._14, v1._15, v1._16)
	}

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10, TT11 <: T11, TT12 <: T12, TT13 <: T13, TT14 <: T14, TT15 <: T15, TT16 <: T16, RR >: R](that: PartialFunction16[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, TT14, TT15, TT16, RR]): PartialFunction16[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, TT14, TT15, TT16, RR] =
		new orElse16(this, that)

	def lift: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Option[R] = new Lifted16(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10, TT11 <: T11, TT12 <: T12, TT13 <: T13, TT14 <: T14, TT15 <: T15, TT16 <: T16, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9, arg10: TT10, arg11: TT11, arg12: TT12, arg13: TT13, arg14: TT14, arg15: TT15, arg16: TT16, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, TT14, TT15, TT16) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) apply(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
}

object PartialFunction16{
	private class orElse16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R](p: PartialFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R], q: PartialFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R])
			extends PartialFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, q)
	}

	private class Lifted16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R](pf: PartialFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any = (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => null
}