package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate5[T1, T2, T3, T4, T5] extends Function5[T1, T2, T3, T4, T5, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](that: Function5[TT1, TT2, TT3, TT4, TT5, Boolean]) = new Predicate5[TT1, TT2, TT3, TT4, TT5]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5) = self(arg1, arg2, arg3, arg4, arg5) || that(arg1, arg2, arg3, arg4, arg5)
	}
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](that: Function5[TT1, TT2, TT3, TT4, TT5, Boolean]) = new Predicate5[TT1, TT2, TT3, TT4, TT5]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5) = self(arg1, arg2, arg3, arg4, arg5) && that(arg1, arg2, arg3, arg4, arg5)
	}
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](that: Function5[TT1, TT2, TT3, TT4, TT5, Boolean]) = new Predicate5[TT1, TT2, TT3, TT4, TT5]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5) = if(self(arg1, arg2, arg3, arg4, arg5)) !that(arg1, arg2, arg3, arg4, arg5) else that(arg1, arg2, arg3, arg4, arg5)
	}
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](that: Function5[TT1, TT2, TT3, TT4, TT5, Boolean]) = new Predicate5[TT1, TT2, TT3, TT4, TT5]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5) = !(self(arg1, arg2, arg3, arg4, arg5) || that(arg1, arg2, arg3, arg4, arg5))
	}
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](that: Function5[TT1, TT2, TT3, TT4, TT5, Boolean]) = new Predicate5[TT1, TT2, TT3, TT4, TT5]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5) = !(self(arg1, arg2, arg3, arg4, arg5) && that(arg1, arg2, arg3, arg4, arg5))
	}
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5](that: Function5[TT1, TT2, TT3, TT4, TT5, Boolean]) = new Predicate5[TT1, TT2, TT3, TT4, TT5]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5) = if(self(arg1, arg2, arg3, arg4, arg5)) that(arg1, arg2, arg3, arg4, arg5) else !that(arg1, arg2, arg3, arg4, arg5)
	}
	override def toString() = "<predicate5>"

}
object Predicate5{
	implicit def not[T1, T2, T3, T4, T5] = new Negation[Predicate5[T1, T2, T3, T4, T5]]{
		def not(pred: Predicate5[T1, T2, T3, T4, T5]) = new Predicate5[T1, T2, T3, T4, T5]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5) = !pred(arg1, arg2, arg3, arg4, arg5)
		}
	}
}
object Always5 extends Predicate5[Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any) = true
}
object Never5 extends Predicate5[Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any) = false
}