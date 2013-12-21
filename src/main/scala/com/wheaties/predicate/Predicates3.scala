package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate3[T1, T2, T3] extends Function3[T1, T2, T3, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3) = self(arg1, arg2, arg3) || that(arg1, arg2, arg3)
	}
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3) = self(arg1, arg2, arg3) && that(arg1, arg2, arg3)
	}
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3) = if(self(arg1, arg2, arg3)) !that(arg1, arg2, arg3) else that(arg1, arg2, arg3)
	}
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3) = !(self(arg1, arg2, arg3) || that(arg1, arg2, arg3))
	}
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3) = !(self(arg1, arg2, arg3) && that(arg1, arg2, arg3))
	}
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3) = if(self(arg1, arg2, arg3)) that(arg1, arg2, arg3) else !that(arg1, arg2, arg3)
	}
	override def toString() = "<predicate3>"

}
object Predicate3{
	implicit def not[T1, T2, T3] = new Negation[Predicate3[T1, T2, T3]]{
		def not(pred: Predicate3[T1, T2, T3]) = new Predicate3[T1, T2, T3]{
			def apply(arg1: T1, arg2: T2, arg3: T3) = !pred(arg1, arg2, arg3)
		}
	}
}
object Always3 extends Predicate3[Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any) = true
}
object Never3 extends Predicate3[Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any) = false
}