package com.wheaties.function

trait Predicate-Like[A]{
  def and[B](that: B)(implicit con: Connective[A,B,Res]) = con.and(this, that)
  def or[B](that: B)(implicit con: Connective[A,B,Res]) = con.or(this, that)
  def xor[B](that: B)(implicit con: Connective[A,B,Res]) = con.xor(this, that)
  def nand[B](that: B)(implicit con: Connective[A,B,Res]) = con.nand(this, that)
  def nor[B](that: B)(implicit con: Connective[A,B,Res]) = con.nor(this, that)
  def nxor[B](that: B)(implicit con: Connective[A,B,Res]) = con.xnor(this, that)
}

trait Connective[A,B,Res]{
  def and(p: A, q: B): Res
  def or(p: A, q: B): Res
  def xor(p: A, q: B): Res
  def nand(p: A, q: B): Res
  def nor(p: A, q: B): Res
  def nxor(p: A, q: B): Res
}

object Not{
  def apply[A](that: A)(implicit neg: Negation[A]) = neg.not(that)
}

trait Negation[A]{
  def not(p: A): A
}