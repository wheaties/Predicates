package com.wheaties.predicate

trait Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] extends Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean] {
	def or(that: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = Or21(this, that)
	def orNot(that: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = OrNot21(this, that)
	def and(that: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = And21(this, that)
	def andNot(that: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = AndNot21(this, that)
	def xor(that: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = Xor21(this, that)
	def nand(that: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = Nand21(this, that)
	def nor(that: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = Nor21(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U):Boolean
}

trait CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] extends Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  val pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
  val pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
}

object CompoundPredicate21{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred: CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):Option[(Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)
}

case class OrNot21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)
}

case class And21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)
}

case class AndNot21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)
}

case class Xor21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)
}

case class Nand21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20))
}

case class Nor21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred1: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred2: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends CompoundPredicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20))
}

case class Not21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred: Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T,arg20: U) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20)
}

case object Always21 extends Predicate21[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any,arg18: Any,arg19: Any,arg20: Any) = true
}

case object Never21 extends Predicate21[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any,arg18: Any,arg19: Any,arg20: Any) = false
}