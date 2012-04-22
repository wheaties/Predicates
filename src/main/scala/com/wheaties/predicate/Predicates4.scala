package com.wheaties.predicate

trait Predicate4[-A, -B, -C, -D] extends Function4[A, B, C, D, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = Or4(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = OrNot4(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = And4(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = AndNot4(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = Xor4(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = Nxor4(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = Nand4(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D](that: Predicate4[AA, BB, CC, DD]) = Nor4(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D):Boolean
}


trait CompoundPredicate4[-A,-B,-C,-D] extends Predicate4[A,B,C,D]{
val pred1: Predicate4[A,B,C,D]
val pred2: Predicate4[A,B,C,D]
}

object CompoundPredicate4{
  def unapply[A,B,C,D](pred: CompoundPredicate4[A,B,C,D]):Option[(Predicate4[A,B,C,D], Predicate4[A,B,C,D])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) || pred2(arg0, arg1, arg2, arg3)
}

case class OrNot4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) || !pred2(arg0, arg1, arg2, arg3)
}

case class And4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) && pred2(arg0, arg1, arg2, arg3)
}

case class AndNot4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred1(arg0, arg1, arg2, arg3) && !pred2(arg0, arg1, arg2, arg3)
}

case class Xor4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred1(arg0, arg1, arg2, arg3)) !pred2(arg0, arg1, arg2, arg3) else pred2(arg0, arg1, arg2, arg3)
}

case class Nxor4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred1(arg0, arg1, arg2, arg3)) pred2(arg0, arg1, arg2, arg3) else !pred2(arg0, arg1, arg2, arg3)
}

case class Nand4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = !(pred1(arg0, arg1, arg2, arg3) && pred2(arg0, arg1, arg2, arg3))
}

case class Nor4[A,B,C,D](pred1: Predicate4[A,B,C,D], pred2: Predicate4[A,B,C,D]) extends CompoundPredicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = !(pred1(arg0, arg1, arg2, arg3) || pred2(arg0, arg1, arg2, arg3))
}

case class Not4[A,B,C,D](pred: Predicate4[A,B,C,D]) extends Predicate4[A,B,C,D]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D) = !pred(arg0, arg1, arg2, arg3)
}

case object Always4 extends Predicate4[Any,Any,Any,Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any) = true
}
	
case object Never4 extends Predicate4[Any,Any,Any,Any]{
	def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any) = false
}
