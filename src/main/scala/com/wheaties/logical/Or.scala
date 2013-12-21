package com.wheaties.logical

object Or{
  def apply[A](x: A, y: A)(implicit dis: Disjunction[A]) = dis.disjunction(x, y)
}

trait Disjunction[A]{
  def disjunction(p: A, q: A): A
}