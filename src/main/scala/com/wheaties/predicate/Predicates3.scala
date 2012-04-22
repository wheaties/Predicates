package com.wheaties.predicate

trait Predicate3[-A, -B, -C] extends Function3[A, B, C, Boolean] {
	def or[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = Or3(this, that)
	def orNot[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = OrNot3(this, that)
	def and[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = And3(this, that)
	def andNot[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = AndNot3(this, that)
	def xor[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = Xor3(this, that)
	def nxor[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = Nxor3(this, that)
	def nand[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = Nand3(this, that)
	def nor[AA <: A, BB <: B, CC <: C](that: Predicate3[AA, BB, CC]) = Nor3(this, that)

	def apply(arg0: A, arg1: B, arg2: C):Boolean
}


trait CompoundPredicate3[-A,-B,-C] extends Predicate3[A,B,C]{
val pred1: Predicate3[A,B,C]
val pred2: Predicate3[A,B,C]
}

object CompoundPredicate3{
  def unapply[A,B,C](pred: CompoundPredicate3[A,B,C]):Option[(Predicate3[A,B,C], Predicate3[A,B,C])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = pred1(arg0, arg1, arg2) || pred2(arg0, arg1, arg2)
}

case class OrNot3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = pred1(arg0, arg1, arg2) || !pred2(arg0, arg1, arg2)
}

case class And3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = pred1(arg0, arg1, arg2) && pred2(arg0, arg1, arg2)
}

case class AndNot3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = pred1(arg0, arg1, arg2) && !pred2(arg0, arg1, arg2)
}

case class Xor3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = if(pred1(arg0, arg1, arg2)) !pred2(arg0, arg1, arg2) else pred2(arg0, arg1, arg2)
}

case class Nxor3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = if(pred1(arg0, arg1, arg2)) pred2(arg0, arg1, arg2) else !pred2(arg0, arg1, arg2)
}

case class Nand3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = !(pred1(arg0, arg1, arg2) && pred2(arg0, arg1, arg2))
}

case class Nor3[A,B,C](pred1: Predicate3[A,B,C], pred2: Predicate3[A,B,C]) extends CompoundPredicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = !(pred1(arg0, arg1, arg2) || pred2(arg0, arg1, arg2))
}

case class Not3[A,B,C](pred: Predicate3[A,B,C]) extends Predicate3[A,B,C]{
  def apply(arg0: A, arg1: B, arg2: C) = !pred(arg0, arg1, arg2)
}

case object Always3 extends Predicate3[Any, Any, Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any) = true
}

case object Never3 extends Predicate3[Any, Any, Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any) = false
}
