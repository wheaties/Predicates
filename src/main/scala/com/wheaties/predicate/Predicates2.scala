package com.wheaties.predicate

trait Predicate2[-A, -B] extends Function2[A, B, Boolean] {
	def or[AA <: A, BB <: B](that: Predicate2[AA, BB]) = Or2(this, that)
	def orNot[AA <: A, BB <: B](that: Predicate2[AA, BB]) = OrNot2(this, that)
	def and[AA <: A, BB <: B](that: Predicate2[AA, BB]) = And2(this, that)
	def andNot[AA <: A, BB <: B](that: Predicate2[AA, BB]) = AndNot2(this, that)
	def xor[AA <: A, BB <: B](that: Predicate2[AA, BB]) = Xor2(this, that)
	def nxor[AA <: A, BB <: B](that: Predicate2[AA, BB]) = Nxor2(this, that)
	def nand[AA <: A, BB <: B](that: Predicate2[AA, BB]) = Nand2(this, that)
	def nor[AA <: A, BB <: B](that: Predicate2[AA, BB]) = Nor2(this, that)

	def apply(arg0: A, arg1: B):Boolean
}


trait CompoundPredicate2[-A,-B] extends Predicate2[A,B]{
  val pred1: Predicate2[A,B]
  val pred2: Predicate2[A,B]
}

object CompoundPredicate2{
  def unapply[A,B](pred: CompoundPredicate2[A,B]):Option[(Predicate2[A,B], Predicate2[A,B])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = pred1(arg0, arg1) || pred2(arg0, arg1)
}

case class OrNot2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = pred1(arg0, arg1) || !pred2(arg0, arg1)
}

case class And2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = pred1(arg0, arg1) && pred2(arg0, arg1)
}

case class AndNot2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = pred1(arg0, arg1) && !pred2(arg0, arg1)
}

case class Xor2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = if(pred1(arg0, arg1)) !pred2(arg0, arg1) else pred2(arg0, arg1)
}

case class Nxor2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = if(pred1(arg0, arg1)) pred2(arg0, arg1) else !pred2(arg0, arg1)
}

case class Nand2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = !(pred1(arg0, arg1) && pred2(arg0, arg1))
}

case class Nor2[A,B](pred1: Predicate2[A,B], pred2: Predicate2[A,B]) extends CompoundPredicate2[A,B]{
  def apply(arg0: A, arg1: B) = !(pred1(arg0, arg1) || pred2(arg0, arg1))
}

case class Not2[A,B](pred: Predicate2[A,B]) extends Predicate2[A,B]{
  def apply(arg0: A, arg1: B) = !pred(arg0, arg1)
}

case object Always2 extends Predicate2[Any, Any]{
def apply(arg0: Any, arg1: Any) = true
}

case object Never2 extends Predicate2[Any, Any]{
  def apply(arg0: Any, arg1: Any) = false
}