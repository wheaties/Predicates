package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate2[T1, T2] extends Function2[T1, T2, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
		def apply(arg1: TT1, arg2: TT2) = self(arg1, arg2) || that(arg1, arg2)
	}
	def and[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
		def apply(arg1: TT1, arg2: TT2) = self(arg1, arg2) && that(arg1, arg2)
	}
	def xor[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
		def apply(arg1: TT1, arg2: TT2) = if(self(arg1, arg2)) !that(arg1, arg2) else that(arg1, arg2)
	}
	def nor[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
		def apply(arg1: TT1, arg2: TT2) = !(self(arg1, arg2) || that(arg1, arg2))
	}
	def nand[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
		def apply(arg1: TT1, arg2: TT2) = !(self(arg1, arg2) && that(arg1, arg2))
	}
	def nxor[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
		def apply(arg1: TT1, arg2: TT2) = if(self(arg1, arg2)) that(arg1, arg2) else !that(arg1, arg2)
	}
	override def toString() = "<predicate2>"

}
object Predicate2{
	implicit def not[T1, T2] = new Negation[Predicate2[T1, T2]]{
		def not(pred: Predicate2[T1, T2]) = new Predicate2[T1, T2]{
			def apply(arg1: T1, arg2: T2) = !pred(arg1, arg2)
		}
	}
}
object Always2 extends Predicate2[Any,Any]{
	def apply(arg1: Any, arg2: Any) = true
}
object Never2 extends Predicate2[Any,Any]{
	def apply(arg1: Any, arg2: Any) = false
}