package com.wheaties.predicate

trait Predicate12[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L] extends Function12[A, B, C, D, E, F, G, H, I, J, K, L, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = Or12(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = OrNot12(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = And12(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = AndNot12(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = Xor12(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = Nxor12(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = Nand12(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L](that: Predicate12[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL]) = Nor12(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L):Boolean
}

trait CompoundPredicate12[-A,-B,-C,-D,-E,-F,-G,-H,-I,-J,-K,-L] extends Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  val pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]
  val pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]
}

object CompoundPredicate12{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L](pred: CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]):Option[(Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], Predicate12[A,B,C,D,E,F,G,H,I,J,K,L])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)
}

case class OrNot12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)
}

case class And12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)
}

case class AndNot12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)
}

case class Xor12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)
}

case class Nxor12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)
}

case class Nand12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11))
}

case class Nor12[A,B,C,D,E,F,G,H,I,J,K,L](pred1: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], pred2: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends CompoundPredicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11))
}

case class Not12[A,B,C,D,E,F,G,H,I,J,K,L](pred: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) extends Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)
}

case object Always12 extends Predicate12[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any) = true
}

case object Never12 extends Predicate12[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any) = false
}