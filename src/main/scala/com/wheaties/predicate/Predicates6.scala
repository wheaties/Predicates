package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate6[T1, T2, T3, T4, T5, T6] extends Function6[T1, T2, T3, T4, T5, T6, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](that: Function6[TT1, TT2, TT3, TT4, TT5, TT6, Boolean]) = new Predicate6[TT1, TT2, TT3, TT4, TT5, TT6]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6) = self(arg1, arg2, arg3, arg4, arg5, arg6) || that(arg1, arg2, arg3, arg4, arg5, arg6)
	}
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](that: Function6[TT1, TT2, TT3, TT4, TT5, TT6, Boolean]) = new Predicate6[TT1, TT2, TT3, TT4, TT5, TT6]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6) = self(arg1, arg2, arg3, arg4, arg5, arg6) && that(arg1, arg2, arg3, arg4, arg5, arg6)
	}
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](that: Function6[TT1, TT2, TT3, TT4, TT5, TT6, Boolean]) = new Predicate6[TT1, TT2, TT3, TT4, TT5, TT6]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6) = if(self(arg1, arg2, arg3, arg4, arg5, arg6)) !that(arg1, arg2, arg3, arg4, arg5, arg6) else that(arg1, arg2, arg3, arg4, arg5, arg6)
	}
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](that: Function6[TT1, TT2, TT3, TT4, TT5, TT6, Boolean]) = new Predicate6[TT1, TT2, TT3, TT4, TT5, TT6]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6) = !(self(arg1, arg2, arg3, arg4, arg5, arg6) || that(arg1, arg2, arg3, arg4, arg5, arg6))
	}
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](that: Function6[TT1, TT2, TT3, TT4, TT5, TT6, Boolean]) = new Predicate6[TT1, TT2, TT3, TT4, TT5, TT6]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6) = !(self(arg1, arg2, arg3, arg4, arg5, arg6) && that(arg1, arg2, arg3, arg4, arg5, arg6))
	}
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6](that: Function6[TT1, TT2, TT3, TT4, TT5, TT6, Boolean]) = new Predicate6[TT1, TT2, TT3, TT4, TT5, TT6]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6) = if(self(arg1, arg2, arg3, arg4, arg5, arg6)) that(arg1, arg2, arg3, arg4, arg5, arg6) else !that(arg1, arg2, arg3, arg4, arg5, arg6)
	}
	override def toString() = "<predicate6>"

}
object Predicate6{
	implicit def not[T1, T2, T3, T4, T5, T6] = new Negation[Predicate6[T1, T2, T3, T4, T5, T6]]{
		def not(pred: Predicate6[T1, T2, T3, T4, T5, T6]) = new Predicate6[T1, T2, T3, T4, T5, T6]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6) = !pred(arg1, arg2, arg3, arg4, arg5, arg6)
		}
	}
}
object Always6 extends Predicate6[Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any) = true
}
object Never6 extends Predicate6[Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any) = false
}