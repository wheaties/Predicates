package com.wheaties.predicate.partials

trait PartialFunction8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends ((T1, T2, T3, T4, T5, T6, T7, T8) => R){
	self =>

	import PartialFunction8._

	override def tupled = new PartialFunction[(T1, T2, T3, T4, T5, T6, T7, T8), R]{
		def isDefinedAt(v1: (T1, T2, T3, T4, T5, T6, T7, T8)) = self isDefinedAt(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8)
		def apply(v1: (T1, T2, T3, T4, T5, T6, T7, T8)) = self(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7, v1._8)
	}

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, RR >: R](that: PartialFunction8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, RR]): PartialFunction8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, RR] =
		new orElse8(this, that)

	def lift: (T1, T2, T3, T4, T5, T6, T7, T8) => Option[R] = new Lifted8(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) apply(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
}

object PartialFunction8{
	private class orElse8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R](p: PartialFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R], q: PartialFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R])
			extends PartialFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, q)
	}

	private class Lifted8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R](pf: PartialFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R]) extends ((T1, T2, T3, T4, T5, T6, T7, T8) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7, T8) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any, Any, Any, Any) => Any = (_, _, _, _, _, _, _, _) => null
}