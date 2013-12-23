package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate4[T1, T2, T3, T4] extends Function4[T1, T2, T3, T4, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = new Predicate4[TT1, TT2, TT3, TT4]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4) = self(arg1, arg2, arg3, arg4) || that(arg1, arg2, arg3, arg4)
	}
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = new Predicate4[TT1, TT2, TT3, TT4]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4) = self(arg1, arg2, arg3, arg4) && that(arg1, arg2, arg3, arg4)
	}
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = new Predicate4[TT1, TT2, TT3, TT4]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4) = if(self(arg1, arg2, arg3, arg4)) !that(arg1, arg2, arg3, arg4) else that(arg1, arg2, arg3, arg4)
	}
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = new Predicate4[TT1, TT2, TT3, TT4]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4) = !(self(arg1, arg2, arg3, arg4) || that(arg1, arg2, arg3, arg4))
	}
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = new Predicate4[TT1, TT2, TT3, TT4]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4) = !(self(arg1, arg2, arg3, arg4) && that(arg1, arg2, arg3, arg4))
	}
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = new Predicate4[TT1, TT2, TT3, TT4]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4) = if(self(arg1, arg2, arg3, arg4)) that(arg1, arg2, arg3, arg4) else !that(arg1, arg2, arg3, arg4)
	}
	override def toString() = "<predicate4>"

}
object Predicate4{
	implicit def not[T1, T2, T3, T4] = new Negation[Predicate4[T1, T2, T3, T4]]{
		def not(pred: Predicate4[T1, T2, T3, T4]) = new Predicate4[T1, T2, T3, T4]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4) = !pred(arg1, arg2, arg3, arg4)
		}
	}
}
object Always4 extends Predicate4[Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any) = true
}
object Never4 extends Predicate4[Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any) = false
}