package com.wheaties.predicate

trait Predicate16[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K, -L, -M, -N, -O, -P] extends Function16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = Or16(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = OrNot16(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = And16(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = AndNot16(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = Xor16(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = Nxor16(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = Nand16(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K, LL <: L, MM <: M, NN <: N, OO <: O, PP <: P](that: Predicate16[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP]) = Nor16(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P):Boolean
}


trait CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] extends Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  val pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
  val pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
}

object CompoundPredicate16{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred: CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):Option[(Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)
}

case class OrNot16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)
}

case class And16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)
}

case class AndNot16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)
}

case class Xor16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)
}

case class Nxor16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)
}

case class Nand16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15))
}

case class Nor16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred1: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred2: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends CompoundPredicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15))
}

case class Not16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)
}

case object Always16 extends Predicate16[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any) = true
}

case object Never16 extends Predicate16[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any) = false
}