package com.wheaties.logical

//TODO: this.type? Doesn't seem like a good solution to me.
trait PredicateLike[A]{
  def and[B,Res](that: B)(implicit con: Connective[this.type,B,Res]): Res = con.and(this, that)
  def or[B,Res](that: B)(implicit con: Connective[this.type,B,Res]): Res = con.or(this, that)
  def xor[B,Res](that: B)(implicit con: Connective[this.type,B,Res]): Res = con.xor(this, that)
  def nand[B,Res](that: B)(implicit con: Connective[this.type,B,Res]): Res = con.nand(this, that)
  def nor[B,Res](that: B)(implicit con: Connective[this.type,B,Res]): Res = con.nor(this, that)
  def nxor[B,Res](that: B)(implicit con: Connective[this.type,B,Res]): Res = con.nxor(this, that)
}

trait Connective[A,B,Res]{
  def and(p: A, q: B): Res
  def or(p: A, q: B): Res
  def xor(p: A, q: B): Res
  def nand(p: A, q: B): Res
  def nor(p: A, q: B): Res
  def nxor(p: A, q: B): Res
}