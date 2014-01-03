package com.wheaties.predicate.partials

trait PartialFunction6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends ((T1, T2, T3, T4, T5, T6) => R){
	import PartialFunction6._

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6): Boolean

	def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, RR >: R](that: PartialFunction6[TT1, TT2, TT3, TT4, TT5, TT6, RR]): PartialFunction6[TT1, TT2, TT3, TT4, TT5, TT6, RR] =
		new orElse6(this, that)

	def lift: (T1, T2, T3, T4, T5, T6) => Option[R] = new Lifted6(this)

	def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, default: (TT1, TT2, TT3, TT4, TT5, TT6) => RR): RR =
		if(isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6)) apply(arg1, arg2, arg3, arg4, arg5, arg6) else default(arg1, arg2, arg3, arg4, arg5, arg6)
}

object PartialFunction6{
	private class orElse6[-T1, -T2, -T3, -T4, -T5, -T6, +R](p: PartialFunction6[T1, T2, T3, T4, T5, T6, R], q: PartialFunction6[T1, T2, T3, T4, T5, T6, R])
			extends PartialFunction6[T1, T2, T3, T4, T5, T6, R]{

		def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6) = p.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6) || q.isDefinedAt(arg1, arg2, arg3, arg4, arg5, arg6)

		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6) = p applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, q)
	}

	private class Lifted6[-T1, -T2, -T3, -T4, -T5, -T6, +R](pf: PartialFunction6[T1, T2, T3, T4, T5, T6, R]) extends ((T1, T2, T3, T4, T5, T6) => Option[R]){
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6): Option[R] = Option{
			pf applyOrElse (arg1, arg2, arg3, arg4, arg5, arg6, defaultFn.asInstanceOf[(T1, T2, T3, T4, T5, T6) => R])
		}
	}

	private val defaultFn: (Any, Any, Any, Any, Any, Any) => Any = (_, _, _, _, _, _) => null
}