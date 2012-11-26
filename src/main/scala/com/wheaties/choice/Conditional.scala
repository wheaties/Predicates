package com.wheaties.choice

trait Conditional[C <: ChoiceL, Out[_] <: Choice[_]]{
  def condition[B](in: C, pred: B => Boolean): Out[B]
  def until[B](in: C, pred: B => Boolean): Out[B]
  def once[B](in: C, pred: B => Boolean): Out[B]
}