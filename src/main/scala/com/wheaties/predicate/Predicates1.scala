package com.wheaties.predicate


trait Predicate1[A] extends Function[A,Boolean] {
  def or(that: Predicate1[A]) = Or1(this, that)
  def orNot(that: Predicate1[A]) = OrNot1(this, that)
  def and(that: Predicate1[A]) = And1(this, that)
  def andNot(that: Predicate1[A]) = AndNot1(this, that)
  def xor(that: Predicate1[A]) = Xor1(this, that)
  def nand(that: Predicate1[A]) = Nand1(this, that)
  def nor(that: Predicate1[A]) = Nor1(this, that)

  def apply(x: A):Boolean
}

//trait CompoundPredicate1[A]{
//  val pred1: Predicate1[A]
//  val pred2: Predicate1[A]
//}
//
//object CompoundPredicate1{
//  def unapply

case class Or1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = pred1(x) || pred2(x)
}

case class OrNot1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = pred1(x) || !pred2(x)
}

case class And1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = pred1(x) && pred2(x)
}

case class AndNot1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = pred1(x) && !pred2(x)
}

case class Xor1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = if(pred1(x)) !pred2(x) else pred2(x)
}

case class Nand1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = !(pred1(x) && pred2(x)) //TODO: this right?  Have I mixed up nand and nor?
}

case class Nor1[A](pred1: Predicate1[A], pred2: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = !(pred1(x) || pred2(x)) //TODO: this right?
}

case class Not1[A](pred: Predicate1[A]) extends Predicate1[A]{
  def apply(x: A) = !pred(x)
}

case object Always1 extends Predicate1[Any]{
  def apply(x: Any) = true
}

case object Never1 extends Predicate1[Any]{
  def apply(x: Any) = false
}