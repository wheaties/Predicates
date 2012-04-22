package com.wheaties.predicate

trait Predicate10[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J] extends Function10[A, B, C, D, E, F, G, H, I, J, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = Or10(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = OrNot10(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = And10(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = AndNot10(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = Xor10(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = Nxor10(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = Nand10(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J](that: Predicate10[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ]) = Nor10(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J):Boolean
}

trait CompoundPredicate10[-A,-B,-C,-D,-E,-F,-G,-H,-I,-J] extends Predicate10[A,B,C,D,E,F,G,H,I,J]{
  val pred1: Predicate10[A,B,C,D,E,F,G,H,I,J]
  val pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]
}

object CompoundPredicate10{
  def unapply[A,B,C,D,E,F,G,H,I,J](pred: CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]):Option[(Predicate10[A,B,C,D,E,F,G,H,I,J], Predicate10[A,B,C,D,E,F,G,H,I,J])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
}

case class OrNot10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
}

case class And10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
}

case class AndNot10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
}

case class Xor10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
}

case class Nxor10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
}

case class Nand10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9))
}

case class Nor10[A,B,C,D,E,F,G,H,I,J](pred1: Predicate10[A,B,C,D,E,F,G,H,I,J], pred2: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends CompoundPredicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9))
}

case class Not10[A,B,C,D,E,F,G,H,I,J](pred: Predicate10[A,B,C,D,E,F,G,H,I,J]) extends Predicate10[A,B,C,D,E,F,G,H,I,J]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
}

case object Always10 extends Predicate10[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any) = true
}

case object Never10 extends Predicate10[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any) = false
}