package com.wheaties.logical

trait PredicateLike[A]{
  self: A =>

  def and[B,Res](that: B)(implicit con: Connective[A,B,Res]): Res = con.and(this, that)
  def or[B,Res](that: B)(implicit con: Connective[A,B,Res]): Res = con.or(this, that)
  def xor[B,Res](that: B)(implicit con: Connective[A,B,Res]): Res = con.xor(this, that)
  def nand[B,Res](that: B)(implicit con: Connective[A,B,Res]): Res = con.nand(this, that)
  def nor[B,Res](that: B)(implicit con: Connective[A,B,Res]): Res = con.nor(this, that)
  def nxor[B,Res](that: B)(implicit con: Connective[A,B,Res]): Res = con.nxor(this, that)
}

//Perhaps this should be typed to And, Or and Xor. Then use negation for nand, nor, and nxor?
trait Connective[A,B,Res]{
  def and(p: A, q: B): Res
  def or(p: A, q: B): Res
  def xor(p: A, q: B): Res
  def nand(p: A, q: B): Res
  def nor(p: A, q: B): Res
  def nxor(p: A, q: B): Res
}