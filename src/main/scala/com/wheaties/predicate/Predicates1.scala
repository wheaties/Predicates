package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate1[T1] extends Function1[T1, Boolean]{
	self =>

	def or[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
		def apply(arg1: TT1) = self(arg1) || that(arg1)
	}
	def and[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
		def apply(arg1: TT1) = self(arg1) && that(arg1)
	}
	def xor[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
		def apply(arg1: TT1) = if(self(arg1)) !that(arg1) else that(arg1)
	}
	def nor[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
		def apply(arg1: TT1) = !(self(arg1) || that(arg1))
	}
	def nand[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
		def apply(arg1: TT1) = !(self(arg1) && that(arg1))
	}
	def nxor[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
		def apply(arg1: TT1) = if(self(arg1)) that(arg1) else !that(arg1)
	}
	override def toString() = "<predicate1>"

}
object Predicate1{
	implicit def not[T1] = new Negation[Predicate1[T1]]{
		def not(pred: Predicate1[T1]) = new Predicate1[T1]{
			def apply(arg1: T1) = !pred(arg1)
		}
	}
}
object Always1 extends Predicate1[Any]{
	def apply(arg1: Any) = true
}
object Never1 extends Predicate1[Any]{
	def apply(arg1: Any) = false
}