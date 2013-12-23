package com.wheaties.choice

import com.wheaties.choice.iteration._
import com.wheaties.predicate.Predicate1

trait Choice[-Value]{
  self =>

  protected[choice] def scheme: Predicate1[Value]
  protected[choice] def filter: Predicate1[Value]

  def get[C](collection: C)(implicit view: View[Value,C]) = view(collection, filter)

  def set[A,C](collection: C, value: A)(implicit replace: Replace[Value,C,A]) = replace(collection, value, filter)

  def mod[C](collection: C, f: Value => Value)(implicit modify: Modify[Value,C]) = modify(collection, f, filter)

  def compose[V <: Value](that: Choice[V]) = that andThen this

  def andThen[V <: Value](that: Choice[V]) = new Choice[V]{
    protected[choice] def scheme = (self filter) and  (that filter)
    protected[choice] def filter = scheme
  }
}

object Choice extends ViewImplicits with ReplaceImplicits with ModifyImplicits