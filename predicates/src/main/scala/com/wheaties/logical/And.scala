package com.wheaties.logical

object And{
  def apply[A](x: A, y: A)(implicit con: Conjunction[A]) = con.conjunction(x, y)
}

trait Conjunction[A]{
  def conjunction(p: A, q: A): A
}