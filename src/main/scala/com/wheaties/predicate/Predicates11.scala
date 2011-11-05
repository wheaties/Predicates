package com.wheaties.predicate

trait Predicate11[A,B,C,D,E,F,G,H,I,J,K] extends Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean] {
	def or(that: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = Or11(this, that)
	def orNot(that: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = OrNot11(this, that)
	def and(that: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = And11(this, that)
	def andNot(that: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = AndNot11(this, that)
	def xor(that: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = Xor11(this, that)
	def nand(that: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = Nand11(this, that)
	def nor(that: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = Nor11(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K):Boolean
}

trait CompoundPredicate11[A,B,C,D,E,F,G,H,I,J,K] extends Predicate11[A,B,C,D,E,F,G,H,I,J,K]{
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