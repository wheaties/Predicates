package com.wheaties.function

trait Predicate-Like[A]{
  def and[B](that: B)(implicit con: Connective[A,B,Res]): Res = con.and(this, that)
  def or[B](that: B)(implicit con: Connective[A,B,Res]): Res = con.or(this, that)
  def xor[B](that: B)(implicit con: Connective[A,B,Res]): Res = con.xor(this, that)
  def nand[B](that: B)(implicit con: Connective[A,B,Res]): Res = con.nand(this, that)
  def nor[B](that: B)(implicit con: Connective[A,B,Res]): Res = con.nor(this, that)
  def nxor[B](that: B)(implicit con: Connective[A,B,Res]): Res = con.xnor(this, that)
}

trait Connective[A,B,Res]{
  def and(p: A, q: B): Res
  def or(p: A, q: B): Res
  def xor(p: A, q: B): Res
  def nand(p: A, q: B): Res
  def nor(p: A, q: B): Res
  def nxor(p: A, q: B): Res
}