package com.wheaties.predicate


trait Predicate1[A] extends Function[A,Boolean] {
  def or(that: Predicate1[A]) = Or1(this, that)
  def orNot(that: Predicate1[A]) = OrNot1(this, that)
  def and(that: Predicate1[A]) = And1(this, that)
  def andNot(that: Predicate1[A]) = AndNot1(this, that)
  def xor(that: Predicate1[A]) = Xor1(this, that)
  def nxor(that: Predicate1[A]) = Nxor1(this, that)
  def nand(that: Predicate1[A]) = Nand1(this, that)
  def nor(that: Predicate1[A]) = Nor1(this, that)

  def apply(arg0: A):Boolean
}

trait CompoundPredicate1[A] extends Predicate1[A]{
  val pred1: Predicate1[A]
  val pred2: Predicate1[A]
}

object CompoundPredicate1{
  def unapply[A](pred: CompoundPredicate1[A]):Option[(Predicate1[A], Predicate1[A])] = {
    Some(pred.pred1, pred.pred2)
  }
}

case class Or1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = pred1(arg0) || pred2(arg0)
}

case class OrNot1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = pred1(arg0) || !pred2(arg0)
}

case class And1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = pred1(arg0) && pred2(arg0)
}

case class AndNot1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = pred1(arg0) && !pred2(arg0)
}

case class Xor1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = if(pred1(arg0)) !pred2(arg0) else pred2(arg0)
}

case class Nxor1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = if(pred1(arg0)) pred2(arg0) else !pred2(arg0)
}

case class Nand1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = !(pred1(arg0) && pred2(arg0))
}

case class Nor1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends CompoundPredicate1[A]{
  def apply(arg0: A) = !(pred1(arg0) || pred2(arg0))
}

case class Not1[A](pred: Predicate1[A]) extends Predicate1[A]{
  def apply(arg0: A) = !pred(arg0)
}

case object Always1 extends Predicate1[Any]{
  def apply(arg0: Any) = true
}

case object Never1 extends Predicate1[Any]{
  def apply(arg0: Any) = false
}