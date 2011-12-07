package com.wheaties.predicate

trait Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean] {
	def or(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = Or18(this, that)
	def orNot(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = OrNot18(this, that)
	def and(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = And18(this, that)
	def andNot(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = AndNot18(this, that)
	def xor(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = Xor18(this, that)
  def nxor(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = Nxor18(this, that)
	def nand(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = Nand18(this, that)
	def nor(that: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = Nor18(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R):Boolean
}

trait CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  val pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
  val pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
}

object CompoundPredicate18{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred: CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):Option[(Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)
}

case class OrNot18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)
}

case class And18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)
}

case class AndNot18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)
}

case class Xor18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)
}

case class Nxor18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)
}

case class Nand18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17))
}

case class Nor18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred1: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred2: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends CompoundPredicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17))
}

case class Not18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)
}

case object Always18 extends Predicate18[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any) = true
}

case object Never18 extends Predicate18[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any) = false
}