package com.wheaties.choice

import iteration._
import com.wheaties.logical.{Not, Negation}

trait Choice[-Value]{
  self =>

  protected[choice] def scheme: Accept[Value]
  protected[choice] def iteration(iter: Accept[Value]): Accept[Value]

  def get[C](collection: C)(implicit prune: Prune[Value,C]) = prune(collection, iteration(scheme))

  def set[A,C](collection: C, value: A)(implicit traverse: Traverse[Value,C], replace: Replace[Value,A]) =
    traverse.onTrue(replace)(collection,iteration(scheme))

  //TODO: this needs to handle List[_], Map[_,_] and Map[Key,_] so that we can have our cake and eat it too?
  def mod[C](collection: C, f: Value => Value)(implicit traverse: Traverse[Value,C]) =
    traverse.onTrue(f)(collection,iteration(scheme))

  def partition[C](collection: C)(implicit divide: Partition[Value,C]) = divide(collection, iteration(scheme))

  //def fold[A,B,C](collection: C, init: Value, f: A)(implicit fold: Fold[Value,C,A]) =
  //  fold(collection, init, f, iteration(scheme))

  //def reduce[A](collection: C, f: A)(implicit fold: Fold[Value,C,A]) =
  //  fold(collection, f, iteration(scheme))

  def compose[V <: Value](that: Choice[V]) = that andThen this

  def andThen[V <: Value](that: Choice[V]) = new Choice[V]{
    protected[choice] def scheme = self iteration (self scheme) andThen (that iteration (that scheme))
    protected[choice] def iteration(iter: Accept[V]) = iter
  }
}

object Choice extends PruneImplicits with TraverseImplicits with PartitionImplicits{
  implicit def neg[Value] = new Negation[Choice[Value]] {
    def not(that: Choice[Value]) = new Choice[Value] {
      protected[choice] def scheme = Not(that scheme)
      protected[choice] def iteration(iter: Accept[Value]) = iter
    }
  }
}