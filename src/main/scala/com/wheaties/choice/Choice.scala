package com.wheaties.choice

import com.wheaties.choice.iteration._
import com.wheaties.predicate.Predicate1

trait Choice[-Value]{
  self =>

  protected[choice] def scheme: Predicate1[Value]
  protected[choice] def filter: Predicate1[Value]

  def get[C](collection: C)(implicit traverse: Traverse[Value,C]) = traverse.prune(collection, filter)

  def set[A,C](collection: C, value: A)(implicit traverse: Traverse[Value,C], replace: Replace[Value,A]) =
    traverse.replace(collection, replace, filter)

  //TODO: this needs to handle List[_], Map[_,_] and Map[Key,_] so that we can have our cake and eat it too.
  def mod[C](collection: C, f: Value => Value)(implicit traverse: Traverse[Value,C]) =
    traverse.replace(collection, f, filter)

  def partition[C](collection: C)(implicit traverse: Traverse[Value,C]) = traverse.part(collection, filter)

  //def fold[A,B,C](collection: C, init: Value, f: A)(implicit traverse: Traverse[Value,C]) =
  //  traverse.fold(collection, init, f, filter(scheme))

  //def reduce[A](collection: C, f: A)(implicit traverse: Traverse[Value,C]) =
  //  traverse.reduce(collection, f, filter(scheme))

  def compose[V <: Value](that: Choice[V]) = that andThen this

  def andThen[V <: Value](that: Choice[V]) = new Choice[V]{
    protected[choice] def scheme = (self filter) and  (that filter)
    protected[choice] def filter = scheme
  }
}

object Choice extends TraverseImplicits