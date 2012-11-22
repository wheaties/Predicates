package com.wheaties.logical

object Not{
  def apply[A](that: A)(implicit neg: Negation[A]) = neg.not(that)
}

trait Negation[A]{
  def not(p: A): A
}