package com.wheaties.predicate

trait Predicate11[-A, -B, -C, -D, -E, -F, -G, -H, -I, -J, -K] extends Function11[A, B, C, D, E, F, G, H, I, J, K, Boolean] {
	def or[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = Or11(this, that)
	def orNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = OrNot11(this, that)
	def and[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = And11(this, that)
	def andNot[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = AndNot11(this, that)
	def xor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = Xor11(this, that)
	def nxor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = Nxor11(this, that)
	def nand[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = Nand11(this, that)
	def nor[AA <: A, BB <: B, CC <: C, DD <: D, EE <: E, FF <: F, GG <: G, HH <: H, II <: I, JJ <: J, KK <: K](that: Predicate11[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK]) = Nor11(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K):Boolean
}

trait CompoundPredicate11[-A,-B,-C,-D,-E,-F,-G,-H,-I,-J,-K] extends Predicate11[A,B,C,D,E,F,G,H,I,J,K]{
  val pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K]
  val pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]
}

object CompoundPredicate11{
  def unapply[A,B,C,D,E,F,G,H,I,J,K](pred: CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]):Option[(Predicate11[A,B,C,D,E,F,G,H,I,J,K], Predicate11[A,B,C,D,E,F,G,H,I,J,K])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)
}

case class OrNot11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)
}

case class And11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)
}

case class AndNot11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)
}

case class Xor11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)
}

case class Nxor11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)
}

case class Nand11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10))
}

case class Nor11[A,B,C,D,E,F,G,H,I,J,K](pred1: Predicate11[A,B,C,D,E,F,G,H,I,J,K], pred2: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10))
}

case class Not11[A,B,C,D,E,F,G,H,I,J,K](pred: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) extends Predicate11[A,B,C,D,E,F,G,H,I,J,K]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)
}

case object Always11 extends Predicate11[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any) = true
}

case object Never11 extends Predicate11[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any) = false
}