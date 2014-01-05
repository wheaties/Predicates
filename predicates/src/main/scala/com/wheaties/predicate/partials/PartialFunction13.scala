package com.wheaties.predicate.partials

trait PartialFunction13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R){
	self =>

	import PartialFunction13._

	override def tupled = new PartialFunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), R]{
		def isDefinedAt(v1: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)) = self isDefinedAt(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13)
		def apply(v1: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)) = self(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8, v1._9, v1._10, v1._11, v1._12, v1._13)
	}

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10, TT11 <: T11, TT12 <: T12, TT13 <: T13, RR >: R](that: PartialFunction13[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, RR]): PartialFunction13[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13, RR] =
		new orElse13(this, that)

	def lift: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Option[R] = new Lifted13(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9, TT10 <: T10, TT11 <: T11, TT12 <: T12, TT13 <: T13, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9, arg10: TT10, arg11: TT11, arg12: TT12, arg13: TT13, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, TT10, TT11, TT12, TT13) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) apply(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
}

object PartialFunction13{
	private class orElse13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R](p: PartialFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R], q: PartialFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R])
			extends PartialFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, q)
	}

	private class Lifted13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R](pf: PartialFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) extends ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => Any = (_, _, _, _, _, _, _, _, _, _, _, _, _) => null
}