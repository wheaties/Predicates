package com.wheaties.predicate

trait Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean] {
	def or(that: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = Or19(this, that)
	def orNot(that: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = OrNot19(this, that)
	def and(that: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = And19(this, that)
	def andNot(that: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = AndNot19(this, that)
	def xor(that: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = Xor19(this, that)
	def nand(that: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = Nand19(this, that)
	def nor(that: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = Nor19(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S):Boolean
}

trait CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  val pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
  val pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
}

object CompoundPredicate19{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred: CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):Option[(Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)
}

case class OrNot19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)
}

case class And19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)
}

case class AndNot19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)
}

case class Xor19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)
}

case class Nand19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18))
}

case class Nor19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred1: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred2: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends CompoundPredicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18))
}

case class Not19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)
}

case object Always19 extends Predicate19[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any,arg18: Any) = true
}

case object Never19 extends Predicate19[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any,arg18: Any) = false
}