package com.wheaties.choice

import com.wheaties.logical.{Negation, Not}
import com.wheaties.predicate.Predicate1

trait Ignore[-V] extends Choice[V]{
  self =>

  def or[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    protected[choice] def scheme = self.scheme or that.scheme
  }
  def and[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    override protected[choice] def scheme = self.scheme and that.scheme
  }
  def xor[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    override protected[choice] def scheme = self.scheme xor that.scheme
  }
  def nor[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    override protected[choice] def scheme = self.scheme nor that.scheme
  }
  def nand[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    override protected[choice] def scheme = self.scheme nand that.scheme
  }
  def nxor[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    override protected[choice] def scheme = self.scheme nxor that.scheme
  }

  protected[choice] def filter = Not(scheme)
}

object Ignore{
  def apply[V](f: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme = new Predicate1[V] {
      def apply(arg1: V) = f(arg1)
    }
  }

  def apply[V](pred: Predicate1[V]) = new Ignore[V]{
    protected[choice] def scheme = pred
  }

  implicit def neg[V] = new Negation[Ignore[V]] {
    def not(that: Ignore[V]) = new Ignore[V] {
      protected[choice] def scheme = Not(that scheme)
    }
  }
}