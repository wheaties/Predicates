package com.wheaties.predicate.defined

import com.wheaties.predicate.partials.PartialFunction4

class DefinedAt4[-T1, -T2, -T3, -T4, R](f: (T1, T2, T3, T4) => R, p: (T1, T2, T3, T4) => Boolean)
			extends PartialFunction4[T1, T2, T3, T4, R]{
	import com.wheaties.predicate._
	def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4) = if(isDefinedAt(arg1, arg2, arg3, arg4)) f(arg1, arg2, arg3, arg4) else throw new NotDefinedForException(arg1, arg2, arg3, arg4)

	override def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, default: (TT1, TT2, TT3, TT4) => RR): RR = if(p(arg1, arg2, arg3, arg4)) f(arg1, arg2, arg3, arg4) else default(arg1, arg2, arg3, arg4)

	def isDefinedAt(arg1: T1, arg2: T2, arg3: T3, arg4: T4) = p(arg1, arg2, arg3, arg4)

	def orAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](q: (TT1, TT2, TT3, TT4) => Boolean) = new DefinedAt4[TT1, TT2, TT3, TT4, R](f, p or q)
	def andAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](q: (TT1, TT2, TT3, TT4) => Boolean) = new DefinedAt4[TT1, TT2, TT3, TT4, R](f, p and q)
	def xorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](q: (TT1, TT2, TT3, TT4) => Boolean) = new DefinedAt4[TT1, TT2, TT3, TT4, R](f, p xor q)
	def norAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](q: (TT1, TT2, TT3, TT4) => Boolean) = new DefinedAt4[TT1, TT2, TT3, TT4, R](f, p nor q)
	def nandAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](q: (TT1, TT2, TT3, TT4) => Boolean) = new DefinedAt4[TT1, TT2, TT3, TT4, R](f, p nand q)
	def nxorAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](q: (TT1, TT2, TT3, TT4) => Boolean) = new DefinedAt4[TT1, TT2, TT3, TT4, R](f, p nxor q)

}

object DefinedAt4{
	implicit class F2DefinedAt4[T1, T2, T3, T4](f: (T1, T2, T3, T4) => Boolean){
		def definedAt[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](pred: (TT1, TT2, TT3, TT4) => Boolean) = new DefinedAt4(f, pred)
	}
}