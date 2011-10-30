package com.wheaties.predicate

object Predicate{
  type Predicate[A] = Predicate1[A]
  type Or[A] = Or1[A]
  type OrNot[A] = OrNot1[A]
  type And[A] = And1[A]
  type AndNot[A] = AndNot1[A]
  type Xor[A] = Xor1[A]
  type Nand[A] = Nand1[A]
  type Nor[A] = Nor1[A]
  type Not[A] = Not1[A]

  implicit def convert[A](func: Function[A, Boolean]) = new Predicate1[A]{ def apply(x: A) = func.apply(x) }

}