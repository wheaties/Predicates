package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate0 extends Function0[Boolean]{
	self =>

	def or(that: Function0[Boolean]) = new Predicate0{
		def apply() = self() || that()
	}
	def and(that: Function0[Boolean]) = new Predicate0{
		def apply() = self() && that()
	}
	def xor(that: Function0[Boolean]) = new Predicate0{
		def apply() = if(self()) !that() else that()
	}
	def nor(that: Function0[Boolean]) = new Predicate0{
		def apply() = !(self() || that())
	}
	def nand(that: Function0[Boolean]) = new Predicate0{
		def apply() = !(self() && that())
	}
	def nxor(that: Function0[Boolean]) = new Predicate0{
		def apply() = if(self()) that() else !that()
	}
	override def toString() = "<predicate0>"

}
object Predicate0{
	implicit def not = new Negation[Predicate0]{
		def not(pred: Predicate0) = new Predicate0{
			def apply() = !pred()
		}
	}
}
object Always0 extends Predicate0{
	def apply() = true
}
object Never0 extends Predicate0{
	def apply() = false
}