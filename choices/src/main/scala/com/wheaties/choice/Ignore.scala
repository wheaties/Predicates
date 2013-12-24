package com.wheaties.choice

import com.wheaties.logical.{Negation, Not}
import com.wheaties.predicate.Predicate1

trait Ignore[-V] extends Choice[V]{
  self =>

  def or[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    protected[choice] def scheme[VV <: V1] = self.scheme or that.scheme
  }
  def and[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    protected[choice] def scheme[VV <: V1] = self.scheme and that.scheme
  }
  def xor[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    protected[choice] def scheme[VV <: V1] = self.scheme xor that.scheme
  }
  def nor[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    protected[choice] def scheme[VV <: V1] = self.scheme nor that.scheme
  }
  def nand[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    protected[choice] def scheme[VV <: V1] = self.scheme nand that.scheme
  }
  def nxor[V1 <: V](that: Ignore[V1]) = new Ignore[V1] {
    protected[choice] def scheme[VV <: V1] = self.scheme nxor that.scheme
  }

  protected[choice] def scheme[VV <: V]: Predicate1[VV]
  protected[choice] def filter[VV <: V] = Not(scheme)
}

object Ignore {
  def apply[V](f: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme[VV <: V] = new Predicate1[VV] {
      def apply(arg1: VV) = f(arg1)
    }
  }

  implicit def neg[V] = new Negation[Ignore[V]] {
    def not(that: Ignore[V]) = new Ignore[V] {
      protected[choice] def scheme[VV <: V] = Not(that.scheme)
    }
  }
}