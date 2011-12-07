package com.wheaties.predicate

trait Predicate5[A,B,C,D,E] extends Function5[A,B,C,D,E,Boolean] {
	def or(that: Predicate5[A,B,C,D,E]) = Or5(this, that)
  def orNot(that: Predicate5[A,B,C,D,E]) = OrNot5(this, that)
	def and(that: Predicate5[A,B,C,D,E]) = And5(this, that)
	def andNot(that: Predicate5[A,B,C,D,E]) = AndNot5(this, that)
	def xor(that: Predicate5[A,B,C,D,E]) = Xor5(this, that)
  def nxor(that: Predicate5[A,B,C,D,E]) = Nxor5(this, that)
	def nand(that: Predicate5[A,B,C,D,E]) = Nand5(this, that)
	def nor(that: Predicate5[A,B,C,D,E]) = Nor5(this, that)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E):Boolean
}

trait CompoundPredicate5[A,B,C,D,E] extends Predicate5[A,B,C,D,E]{
  val pred1: Predicate5[A,B,C,D,E]
  val pred2: Predicate5[A,B,C,D,E]
}

object CompoundPredicate5{
  def unapply[A,B,C,D,E](pred: CompoundPredicate5[A,B,C,D,E]):Option[(Predicate5[A,B,C,D,E], Predicate5[A,B,C,D,E])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = pred1(arg0, arg1, arg2, arg3, arg4) || pred2(arg0, arg1, arg2, arg3, arg4)
}

case class OrNot5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = pred1(arg0, arg1, arg2, arg3, arg4) || !pred2(arg0, arg1, arg2, arg3, arg4)
}

case class And5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = pred1(arg0, arg1, arg2, arg3, arg4) && pred2(arg0, arg1, arg2, arg3, arg4)
}

case class AndNot5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = pred1(arg0, arg1, arg2, arg3, arg4) && !pred2(arg0, arg1, arg2, arg3, arg4)
}

case class Xor5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred1(arg0, arg1, arg2, arg3, arg4)) !pred2(arg0, arg1, arg2, arg3, arg4) else pred2(arg0, arg1, arg2, arg3, arg4)
}

case class Nxor5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred1(arg0, arg1, arg2, arg3, arg4)) pred2(arg0, arg1, arg2, arg3, arg4) else !pred2(arg0, arg1, arg2, arg3, arg4)
}

case class Nand5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = !(pred1(arg0, arg1, arg2, arg3, arg4) && pred2(arg0, arg1, arg2, arg3, arg4))
}

case class Nor5[A,B,C,D,E](pred1: Predicate5[A,B,C,D,E], pred2: Predicate5[A,B,C,D,E]) extends CompoundPredicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = !(pred1(arg0, arg1, arg2, arg3, arg4) || pred2(arg0, arg1, arg2, arg3, arg4))
}

case class Not5[A,B,C,D,E](pred: Predicate5[A,B,C,D,E]) extends Predicate5[A,B,C,D,E]{
  def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = !pred(arg0, arg1, arg2, arg3, arg4)
}

case object Always5 extends Predicate5[Any,Any,Any,Any,Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any, arg4: Any) = true
}

case object Never5 extends Predicate5[Any,Any,Any,Any,Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any, arg3: Any, arg4: Any) = false
}
