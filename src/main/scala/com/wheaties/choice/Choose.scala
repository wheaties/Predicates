package com.wheaties.choice

import com.wheaties.predicate.Predicate1
import com.wheaties.logical.{Not, Negation}

trait Choose[-V] extends Choice[V]{
  self =>

  def or[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    protected[choice] def filter[VV <: V1] = self.filter or that.filter
  }
  def and[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    protected[choice] def filter[VV <: V1] = self.filter and that.filter
  }
  def xor[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    protected[choice] def filter[VV <: V1] = self.filter xor that.filter
  }
  def nor[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    protected[choice] def filter[VV <: V1] = self.filter nor that.filter
  }
  def nand[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    protected[choice] def filter[VV <: V1] = self.filter nand that.filter
  }
  def nxor[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    protected[choice] def filter[VV <: V1] = self.filter nxor that.filter
  }

  protected[choice] def filter[VV <: V]: Predicate1[VV]
}

object Choose{
  def apply[V](f: V => Boolean) = new Choose[V]{
    protected[choice] def filter[VV <: V] = new Predicate1[VV] {
      def apply(arg1: VV) = f(arg1)
    }
  }

  def apply[V](pred: Predicate1[V]) = new Choose[V]{
    protected[choice] def filter[VV <: V] = pred
  }

  implicit def neg[V] = new Negation[Choose[V]] {
    def not(that: Choose[V]) = new Choose[V] {
      protected[choice] def filter[VV <: V] = Not(that.filter)
    }
  }
}