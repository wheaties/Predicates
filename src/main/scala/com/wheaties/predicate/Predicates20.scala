package com.wheaties.predicate

trait Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean] {
	def or(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = Or20(this, that)
	def orNot(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = OrNot20(this, that)
	def and(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = And20(this, that)
	def andNot(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = AndNot20(this, that)
	def xor(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = Xor20(this, that)
  def nxor(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = Nxor20(this, that)
	def nand(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = Nand20(this, that)
	def nor(that: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = Nor20(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T):Boolean
}

trait CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  val pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
  val pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
}

object CompoundPredicate20{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred: CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):Option[(Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)
}

case class OrNot20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)
}

case class And20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)
}

case class AndNot20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)
}

case class Xor20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)
}

case class Nxor20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)
}

case class Nand20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19))
}

case class Nor20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred1: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred2: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends CompoundPredicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19))
}

case class Not20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)
}

case object Always20 extends Predicate20[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any,arg18: Any,arg19: Any) = true
}

case object Never20 extends Predicate20[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any,arg14: Any,arg15: Any,arg16: Any,arg17: Any,arg18: Any,arg19: Any) = false
}