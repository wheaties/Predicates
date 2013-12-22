package com.wheaties.choice

import com.wheaties.predicate.Predicate1
import com.wheaties.logical.{Not, Negation}

trait Choose[-V] extends Choice[V]{
  self =>

  def or[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    protected[choice] def scheme = self.scheme or that.scheme
  }
  def and[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    override protected[choice] def scheme = self.scheme and that.scheme
  }
  def xor[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    override protected[choice] def scheme = self.scheme xor that.scheme
  }
  def nor[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    override protected[choice] def scheme = self.scheme nor that.scheme
  }
  def nand[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    override protected[choice] def scheme = self.scheme nand that.scheme
  }
  def nxor[V1 <: V](that: Choose[V1]) = new Choose[V1] {
    override protected[choice] def scheme = self.scheme nxor that.scheme
  }

  protected[choice] def filter = scheme
}

object Choose{
  def apply[V](f: V => Boolean) = new Choose[V]{
    protected[choice] def scheme = new Predicate1[V] {
      def apply(arg1: V) = f(arg1)
    }
  }

  def apply[V](pred: Predicate1[V]) = new Choose[V]{
    protected[choice] def scheme = pred
  }

  implicit def neg[V] = new Negation[Choose[V]] {
    def not(that: Choose[V]) = new Choose[V] {
      protected[choice] def scheme = Not(that scheme)
    }
  }
}