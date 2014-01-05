package com.wheaties.predicate.partials

trait PartialFunction7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends ((T1, T2, T3, T4, T5, T6, T7) => R){
	self =>

	import PartialFunction7._

	override def tupled = new PartialFunction[(T1, T2, T3, T4, T5, T6, T7), R]{
		def isDefinedAt(v1: (T1, T2, T3, T4, T5, T6, T7)) = self isDefinedAt(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7)
		def apply(v1: (T1, T2, T3, T4, T5, T6, T7)) = self(v1._1, v1._2, v1._3, v1._4, v1._5, v1._6, v1._7)
	}

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, RR >: R](that: PartialFunction7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, RR]): PartialFunction7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, RR] =
		new orElse7(this, that)

	def lift: (T1, T2, T3, T4, T5, T6, T7) => Option[R] = new Lifted7(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, default: (TT1, TT2, TT3, TT4, TT5, TT6, TT7) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7)) apply(arg1, arg2, arg3, arg4, arg5, arg6, arg7) else default(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
}

object PartialFunction7{
	private class orElse7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R](p: PartialFunction7[T1, T2, T3, T4, T5, T6, T7, R], q: PartialFunction7[T1, T2, T3, T4, T5, T6, T7, R])
			extends PartialFunction7[T1, T2, T3, T4, T5, T6, T7, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, q)
	}

	private class Lifted7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R](pf: PartialFunction7[T1, T2, T3, T4, T5, T6, T7, R]) extends ((T1, T2, T3, T4, T5, T6, T7) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, arg7, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5, T6, T7) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any, Any, Any) => Any = (_, _, _, _, _, _, _) => null
}