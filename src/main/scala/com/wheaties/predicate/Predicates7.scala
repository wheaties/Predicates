package com.wheaties.predicate

trait Predicate7[-A, -B, -C, -D, -E, -F, -G] extends Function7[A, B, C, D, E, F, G, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = Or7(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = OrNot7(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = And7(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = AndNot7(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = Xor7(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = Nxor7(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = Nand7(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G](that: Predicate7[AA, BB, CC, DD, EE, FF, GG]) = Nor7(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G):Boolean
}

trait CompoundPredicate7[-A,-B,-C,-D,-E,-F,-G] extends Predicate7[A,B,C,D,E,F,G]{
  val pred1: Predicate7[A,B,C,D,E,F,G]
  val pred2: Predicate7[A,B,C,D,E,F,G]
}

object CompoundPredicate7{
  def unapply[A,B,C,D,E,F,G](pred: CompoundPredicate7[A,B,C,D,E,F,G]):Option[(Predicate7[A,B,C,D,E,F,G], Predicate7[A,B,C,D,E,F,G])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class OrNot7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class And7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class AndNot7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class Xor7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class Nxor7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case class Nand7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6))
}

case class Nor7[A,B,C,D,E,F,G](pred1: Predicate7[A,B,C,D,E,F,G], pred2: Predicate7[A,B,C,D,E,F,G]) extends CompoundPredicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6))
}

case class Not7[A,B,C,D,E,F,G](pred: Predicate7[A,B,C,D,E,F,G]) extends Predicate7[A,B,C,D,E,F,G]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6)
}

case object Always7 extends Predicate7[Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any) = true
}

case object Never7 extends Predicate7[Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any) = false
}
