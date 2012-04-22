package com.wheaties.predicate

trait Predicate13[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M] extends Function13[A, B, C, D, E, F, G, H, I, J, K, L, M, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = Or13(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = OrNot13(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = And13(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = AndNot13(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = Xor13(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = Nxor13(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = Nand13(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M](that: Predicate13[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM]) = Nor13(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M):Boolean
}


trait CompoundPredicate13[-A,-B,-C,-D,-E,-F,-G,-H,-I,-J,-K,-L,-M] extends Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  val pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]
  val pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]
}

object CompoundPredicate13{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M](pred: CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]):Option[(Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)
}

case class OrNot13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)
}

case class And13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)
}

case class AndNot13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)
}

case class Xor13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)
}

case class Nxor13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)
}

case class Nand13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12))
}

case class Nor13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred1: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], pred2: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends CompoundPredicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12))
}

case class Not13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)
}

case object Always13 extends Predicate13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any) = true
}

case object Never13 extends Predicate13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any) = false
}