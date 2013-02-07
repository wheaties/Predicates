package com.wheaties.choice.iteration

import scala.collection.mutable.Builder

trait Accum[@specialized(Int, Long, Float, Double) -Elem,+Container]{
  def +=(x: Elem)
  def result(): Container
}

object Accum{
  implicit def accumB[Elem,C[_] <: Iterable[_]] = new Accum[Elem,C[Elem]] {
    def +=(x: Elem)
    def result(): C[Elem]
  }
}

object NullAccumulator extends Accum[Any,Nothing]{
  def +=(x: Any){}
  def result() = throw new Exception("Unable to call result on NullAccumulator")
}