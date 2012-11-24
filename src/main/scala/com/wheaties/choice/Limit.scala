package com.wheaties.choice

trait Limit[V, C <: ChoiceS[V], Out <: Choice[V]]{
  def every(in: C, n: Int): Out
  def every(in: C, f: Int => Int, n: Int = 1): Out
  def all(in: C): Out
  def first(in: C, n: Int): Out
  //def last(in: C, n: Int): Out
  //def exactly(in: C, n: Int): Out
}