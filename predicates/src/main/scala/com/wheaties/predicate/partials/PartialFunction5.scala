package com.wheaties.predicate.partials

trait PartialFunction5[-T1, -T2, -T3, -T4, -T5, +R] extends ((T1, T2, T3, T4, T5) => R){
	self =>

	import PartialFunction5._

	override def tupled = new PartialFunction[(T1, T2, T3, T4, T5), R]{
		def isDefinedAt(v1: (T1, T2, T3, T4, T5)) = self isDefinedAt(v1._1, v1._2, v1._3, v1._4, v1._5)
		def apply(v1: (T1, T2, T3, T4, T5)) = self(v1._1, v1._2, v1._3, v1._4, v1._5)
	}

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, RR >: R](that: PartialFunction5[TT1, TT2, TT3, TT4, TT5, RR]): PartialFunction5[TT1, TT2, TT3, TT4, TT5, RR] =
		new orElse5(this, that)

	def lift: (T1, T2, T3, T4, T5) => Option[R] = new Lifted5(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, default: (TT1, TT2, TT3, TT4, TT5) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5)) apply(arg1, arg2, arg3, arg4, arg5) else default(arg1, arg2, arg3, arg4, arg5)
}

object PartialFunction5{
	private class orElse5[-T1, -T2, -T3, -T4, -T5, +R](p: PartialFunction5[T1, T2, T3, T4, T5, R], q: PartialFunction5[T1, T2, T3, T4, T5, R])
			extends PartialFunction5[T1, T2, T3, T4, T5, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, q)
	}

	private class Lifted5[-T1, -T2, -T3, -T4, -T5, +R](pf: PartialFunction5[T1, T2, T3, T4, T5, R]) extends ((T1, T2, T3, T4, T5) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any) => Any = (_, _, _, _, _) => null
}