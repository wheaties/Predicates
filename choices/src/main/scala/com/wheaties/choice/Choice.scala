package com.wheaties.choice

import com.wheaties.choice.iteration._
import com.wheaties.predicate.Predicate1

trait Choice[-Value]{
  self =>

  protected[choice] def filter[V <: Value]: Predicate1[V]

  def get[C](collection: C)(implicit view: View[Value, C]) = view(collection, filter)

  def set[C, A](collection: C, value: A)(implicit replace: Replace[Value, C, A]) = replace(collection, value, filter)

  def mod[C, V <: Value](collection: C, f: V => V)(implicit modify: Modify[V, C]) = modify(collection, f, filter)

  def compose[V <: Value](that: Choice[V]) = that andThen this

  def andThen[V <: Value](that: Choice[V]) = new Choice[V]{
    protected[choice] def filter[VV <: V] = self.filter and that.filter
  }
}