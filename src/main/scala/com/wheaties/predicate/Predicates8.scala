package com.wheaties.predicate

trait Predicate8[-A, -B, -C, -D, -E, -F, -G, -H] extends Function8[A, B, C, D, E, F, G, H, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = Or8(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = OrNot8(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = And8(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = AndNot8(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = Xor8(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = Nxor8(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = Nand8(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H](that: Predicate8[AA, BB, CC, DD, EE, FF, GG, HH]) = Nor8(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H):Boolean
}

trait CompoundPredicate8[A,B,C,D,E,F,G,H] extends Predicate8[A,B,C,D,E,F,G,H]{
  val pred1: Predicate8[A,B,C,D,E,F,G,H]
  val pred2: Predicate8[A,B,C,D,E,F,G,H]
}

object CompoundPredicate8{
  def unapply[A,B,C,D,E,F,G,H](pred: CompoundPredicate8[A,B,C,D,E,F,G,H]):Option[(Predicate8[A,B,C,D,E,F,G,H], Predicate8[A,B,C,D,E,F,G,H])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)
}

case class OrNot8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)
}

case class And8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)
}

case class AndNot8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)
}

case class Xor8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)
}

case class Nxor8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)
}

case class Nand8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7))
}

case class Nor8[A,B,C,D,E,F,G,H](pred1: Predicate8[A,B,C,D,E,F,G,H], pred2: Predicate8[A,B,C,D,E,F,G,H]) extends CompoundPredicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7))
}

case class Not8[A,B,C,D,E,F,G,H](pred: Predicate8[A,B,C,D,E,F,G,H]) extends Predicate8[A,B,C,D,E,F,G,H]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)
}

case object Always8 extends Predicate8[Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any) = true
}

case object Never8 extends Predicate8[Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any) = false
}
