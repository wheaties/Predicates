package com.wheaties.predicate

trait Predicate4[-A, -B, -C, -D] extends Function4[A, B, C, D, Boolean] {
  self =>
  
	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = Or4(this, that)
	def orNot[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = OrNot4(this, that)
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = And4(this, that)
	def andNot[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = AndNot4(this, that)
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = Xor4(this, that)
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = Nxor4(this, that)
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = Nand4(this, that)
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4](that: Function4[TT1, TT2, TT3, TT4, Boolean]) = Nor4(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D):Boolean
}

case class Or4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) || pred2(arg0, arg1, arg2, arg3)
}

case class OrNot4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) || !pred2(arg0, arg1, arg2, arg3)
}

case class And4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) && pred2(arg0, arg1, arg2, arg3)
}

case class AndNot4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) && !pred2(arg0, arg1, arg2, arg3)
}

case class Xor4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred1(arg0, arg1, arg2, arg3)) !pred2(arg0, arg1, arg2, arg3) else pred2(arg0, arg1, arg2, arg3)
}

case class Nxor4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred1(arg0, arg1, arg2, arg3)) pred2(arg0, arg1, arg2, arg3) else !pred2(arg0, arg1, arg2, arg3)
}

case class Nand4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = !(pred1(arg0, arg1, arg2, arg3) && pred2(arg0, arg1, arg2, arg3))
}

case class Nor4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends new Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = !(pred1(arg0, arg1, arg2, arg3) || pred2(arg0, arg1, arg2, arg3))
}

case class Not4[A,B,C,D](pred: Predicate4[A,B,C,D]) extends Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = !pred(arg0, arg1, arg2, arg3)
}

object Always4 extends Predicate4[Any,Any,Any,Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any) = true
}
	
object Never4 extends Predicate4[Any,Any,Any,Any]{
	def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any) = false
}
