package com.wheaties.predicate.partials

trait PartialFunction4[-T1, -T2, -T3, -T4, +R] extends ((T1, T2, T3, T4) => R){
	import PartialFunction4._

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, RR >: R](that: PartialFunction4[TT1, TT2, TT3, TT4, RR]): PartialFunction4[TT1, TT2, TT3, TT4, RR] =
		new orElse4(this, that)

	def lift: (T1, T2, T3, T4) => Option[R] = new Lifted4(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, default: (TT1, TT2, TT3, TT4) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4)) apply(arg1, arg2, arg3, arg4) else default(arg1, arg2, arg3, arg4)
}

object PartialFunction4{
	private class orElse4[-T1, -T2, -T3, -T4, +R](p: PartialFunction4[T1, T2, T3, T4, R], q: PartialFunction4[T1, T2, T3, T4, R])
			extends PartialFunction4[T1, T2, T3, T4, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4) = p.isDefinedAt(arg1, arg2, arg3, arg4) || q.isDefinedAt(arg1, arg2, arg3, arg4)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4) = p applyOrElse (arg1, arg2, arg3, arg4, q)
	}

	private class Lifted4[-T1, -T2, -T3, -T4, +R](pf: PartialFunction4[T1, T2, T3, T4, R]) extends ((T1, T2, T3, T4) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, defaultFn.asInstanceOf[(T1, T2, T3, T4) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any) => Any = (_, _, _, _) => null
}