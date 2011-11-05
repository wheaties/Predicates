package com.wheaties.predicate

trait Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean] {
	def or(that: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = Or14(this, that)
	def orNot(that: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = OrNot14(this, that)
	def and(that: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = And14(this, that)
	def andNot(that: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = AndNot14(this, that)
	def xor(that: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = Xor14(this, that)
	def nand(that: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = Nand14(this, that)
	def nor(that: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = Nor14(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N):Boolean
}

trait CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  val pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
  val pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
}

object CompoundPredicate14{
  def unapply[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred: CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):Option[(Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)
}

case class OrNot14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)
}

case class And14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)
}

case class AndNot14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)
}

case class Xor14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)
}

case class Nand14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13))
}

case class Nor14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred1: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred2: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends CompoundPredicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13))
}

case class Not14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)
}

case object Always14 extends Predicate14[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any) = true
}

case object Never14 extends Predicate14[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any,arg9: Any,arg10: Any,arg11: Any,arg12: Any,arg13: Any) = false
}