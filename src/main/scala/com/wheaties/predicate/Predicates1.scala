package com.wheaties.predicate

trait Predicate1[-A] extends Function[A,Boolean] {
  def or[B <: A](that: Predicate1[B]) = Or1(this, that)
  def orNot[B <: A](that: Predicate1[B]) = OrNot1(this, that)
  def and[B <: A](that: Predicate1[B]) = And1(this, that)
  def andNot[B <: A](that: Predicate1[B]) = AndNot1(this, that)
  def xor[B <: A](that: Predicate1[B]) = Xor1(this, that)
  def nxor[B <: A](that: Predicate1[B]) = Nxor1(this, that)
  def nand[B <: A](that: Predicate1[B]) = Nand1(this, that)
  def nor[B <: A](that: Predicate1[B]) = Nor1(this, that)

  def apply(arg0: A):Boolean
}

trait CompoundPredicate1[-A] extends Predicate1[A]{
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