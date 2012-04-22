package com.wheaties.predicate

trait Predicate17[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P, -Q] extends Function17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = Or17(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = OrNot17(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = And17(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = AndNot17(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = Xor17(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = Nxor17(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = Nand17(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P, QQ <: Q](that: Predicate17[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ]) = Nor17(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q):Boolean
}


trait CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] extends Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  val pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
  val pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
}

object CompoundPredicate17{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred: CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):Option[(Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)
}

case class OrNot17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)
}

case class And17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)
}

case class AndNot17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)
}

case class Xor17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)
}

case class Nxor17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)
}

case class Nand17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16))
}

case class Nor17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred1: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred2: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends CompoundPredicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16))
}

case class Not17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)
}

case object Always17 extends Predicate17[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any) = true
}

case object Never17 extends Predicate17[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any) = false
}