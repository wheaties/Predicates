package com.wheaties.predicate

trait Predicate6[A,B,C,D,E,F] extends Function6[A,B,C,D,E,F,Boolean] {
	def or(that: Predicate6[A,B,C,D,E,F]) = Or6(this, that)
	def orNot(that: Predicate6[A,B,C,D,E,F]) = OrNot6(this, that)
	def and(that: Predicate6[A,B,C,D,E,F]) = And6(this, that)
	def andNot(that: Predicate6[A,B,C,D,E,F]) = AndNot6(this, that)
	def xor(that: Predicate6[A,B,C,D,E,F]) = Xor6(this, that)
  def nxor(that: Predicate6[A,B,C,D,E,F]) = Nxor6(this, that)
	def nand(that: Predicate6[A,B,C,D,E,F]) = Nand6(this, that)
	def nor(that: Predicate6[A,B,C,D,E,F]) = Nor6(this, that)

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F):Boolean
}

trait CompoundPredicate6[A,B,C,D,E,F] extends Predicate6[A,B,C,D,E,F]{
  val pred1: Predicate6[A,B,C,D,E,F]
  val pred2: Predicate6[A,B,C,D,E,F]
}

object CompoundPredicate6{
  def unapply[A,B,C,D,E,F](pred: CompoundPredicate6[A,B,C,D,E,F]):Option[(Predicate6[A,B,C,D,E,F], Predicate6[A,B,C,D,E,F])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = pred1(arg0,arg1,arg2,arg3,arg4,arg5) || pred2(arg0,arg1,arg2,arg3,arg4,arg5)
}

case class OrNot6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = pred1(arg0,arg1,arg2,arg3,arg4,arg5) || !pred2(arg0,arg1,arg2,arg3,arg4,arg5)
}

case class And6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = pred1(arg0,arg1,arg2,arg3,arg4,arg5) && pred2(arg0,arg1,arg2,arg3,arg4,arg5)
}

case class AndNot6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = pred1(arg0,arg1,arg2,arg3,arg4,arg5) && !pred2(arg0,arg1,arg2,arg3,arg4,arg5)
}

case class Xor6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5)) !pred2(arg0,arg1,arg2,arg3,arg4,arg5) else pred2(arg0,arg1,arg2,arg3,arg4,arg5)
}

case class Nxor6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = if(pred1(arg0,arg1,arg2,arg3,arg4,arg5)) pred2(arg0,arg1,arg2,arg3,arg4,arg5) else !pred2(arg0,arg1,arg2,arg3,arg4,arg5)
}

case class Nand6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5) && pred2(arg0,arg1,arg2,arg3,arg4,arg5))
}

case class Nor6[A,B,C,D,E,F](pred1: Predicate6[A,B,C,D,E,F], pred2: Predicate6[A,B,C,D,E,F]) extends CompoundPredicate6[A,B,C,D,E,F]{
  def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = !(pred1(arg0,arg1,arg2,arg3,arg4,arg5) || pred1(arg0,arg1,arg2,arg3,arg4,arg5))
}

case class Not6[A,B,C,D,E,F](pred: Predicate6[A,B,C,D,E,F]) extends Predicate6[A,B,C,D,E,F]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = !pred(arg0,arg1,arg2,arg3,arg4,arg5)
}

case object Always6 extends Predicate6[Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any) = true
}

case object Never6 extends Predicate6[Any,Any,Any,Any,Any,Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any) = false
}
