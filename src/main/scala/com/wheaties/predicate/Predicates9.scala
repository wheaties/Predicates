package com.wheaties.predicate

trait Predicate9[A,B,C,D,E,F,G,H,I] extends Function9[A,B,C,D,E,F,G,H,I,Boolean] {
	def or(that: Predicate9[A,B,C,D,E,F,G,H,I]) = Or9(this, that)
	def orNot(that: Predicate9[A,B,C,D,E,F,G,H,I]) = OrNot9(this, that)
	def and(that: Predicate9[A,B,C,D,E,F,G,H,I]) = And9(this, that)
	def andNot(that: Predicate9[A,B,C,D,E,F,G,H,I]) = AndNot9(this, that)
	def xor(that: Predicate9[A,B,C,D,E,F,G,H,I]) = Xor9(this, that)
  def nxor(that: Predicate8[A,B,C,D,E,F,G,H,I]) = Nxor9(this, that)
	def nand(that: Predicate9[A,B,C,D,E,F,G,H,I]) = Nand9(this, that)
	def nor(that: Predicate9[A,B,C,D,E,F,G,H,I]) = Nor9(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I):Boolean
}

trait CompoundPredicate9[A,B,C,D,E,F,G,H,I] extends Predicate9[A,B,C,D,E,F,G,H,I]{
  val pred1: Predicate9[A,B,C,D,E,F,G,H,I]
  val pred2: Predicate9[A,B,C,D,E,F,G,H,I]
}

object CompoundPredicate9{
  def unapply[A,B,C,D,E,F,G,H,I](pred: CompoundPredicate9[A,B,C,D,E,F,G,H,I]):Option[(Predicate9[A,B,C,D,E,F,G,H,I], Predicate9[A,B,C,D,E,F,G,H,I])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) || pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
}

case class OrNot9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
}

case class And9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
}

case class AndNot9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
}

case class Xor9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) else pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
}

case class Nxor9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)) pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
}

case class Nand9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) && pred2(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8))
}

case class Nor9[A,B,C,D,E,F,G,H,I](pred1: Predicate9[A,B,C,D,E,F,G,H,I], pred2: Predicate9[A,B,C,D,E,F,G,H,I]) extends CompoundPredicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) || pred1(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8))
}

case class Not9[A,B,C,D,E,F,G,H,I](pred: Predicate9[A,B,C,D,E,F,G,H,I]) extends Predicate9[A,B,C,D,E,F,G,H,I]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = !pred(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)
}

case object Always9 extends Predicate9[Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any) = true
}

case object Never9 extends Predicate9[Any,Any,Any,Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any,arg1: Any,arg2: Any,arg3: Any,arg4: Any,arg5: Any,arg6: Any,arg7: Any,arg8: Any) = false
}