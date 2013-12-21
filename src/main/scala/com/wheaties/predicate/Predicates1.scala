package com.wheaties.predicate

import com.wheaties.logical._

/**
 * A predicate of 1 variable, i.e. a function of 1 variable which returns a boolean value.
 *
 * @tparam T1 A type over which this predicate is defined.
 */
trait Predicate1[@specialized(Int,Long,Float,Double) -T1] extends Function1[T1,Boolean]{
  self =>

  def or[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = self(arg1) || that(arg1)
  }
  def orNot[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = self(arg1) || !that(arg1)
  }
  def and[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = self(arg1) && that(arg1)
  }
  def andNot[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = self(arg1) && !that(arg1)
  }
  def xor[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = if(self(arg1)) !that(arg1) else that(arg1)
  }
  def nxor[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = if(self(arg1)) that(arg1) else !that(arg1)
  }
  def nand[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = !(self(arg1) && that(arg1))
  }
  def nor[TT <: T1](that: (TT => Boolean)) = new Predicate1[TT] {
    def apply(arg1: TT) = !(self(arg1) || that(arg1))
  }

  override def toString() = "<predicate1>"
}

object Predicate1{
  implicit def not[T1] = new Negation[Predicate1[T1]]{
    def not(pred: Predicate1[T1]) = new Predicate1[T1]{
      def apply(arg1: T1) = !pred(arg1)
    }
  }
}

object Always1 extends Predicate1[Any]{
  def apply(arg1: Any) = true
}

object Never1 extends Predicate1[Any]{
  def apply(arg1: Any) = false
}
