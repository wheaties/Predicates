package com.wheaties.predicate

trait Predicate2[A,B] extends Function2[A,B,Boolean] {
	def or(that: Predicate2[A,B]) = Or2(this, that)
  def orNot(that: Predicate2[A,B]) = OrNot2(this, that)
	def and(that: Predicate2[A,B]) = And2(this, that)
	def andNot(that: Predicate2[A,B]) = AndNot2(this, that)
	def xor(that: Predicate2[A,B]) = Xor2(this, that)
	def nand(that: Predicate2[A,B]) = Nand2(this, that)
	def nor(that: Predicate2[A,B]) = Nor2(this, that)

	def apply(arg0: A, arg1: B):Boolean
}

trait CompoundPredicate2[A,B] extends Predicate2[A,B]{
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