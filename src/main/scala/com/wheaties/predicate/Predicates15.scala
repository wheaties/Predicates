package com.wheaties.predicate

trait Predicate15[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O] extends Function15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = Or15(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = OrNot15(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = And15(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = AndNot15(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = Xor15(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = Nxor15(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = Nand15(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O](that: Predicate15[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO]) = Nor15(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O):Boolean
}


trait CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] extends Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  val pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
  val pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
}

object CompoundPredicate15{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred: CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):Option[(Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)
}

case class OrNot15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)
}

case class And15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)
}

case class AndNot15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)
}

case class Xor15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)
}

case class Nxor15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)
}

case class Nand15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14))
}

case class Nor15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred1: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred2: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends CompoundPredicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14))
}

case class Not15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)
}

case object Always15 extends Predicate15[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any) = true
}

case object Never15 extends Predicate15[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any) = false
}