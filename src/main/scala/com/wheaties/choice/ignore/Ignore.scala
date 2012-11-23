package com.wheaties.choice.ignore

import com.wheaties.choice.{Getter, Setter, Choice}
import com.wheaties.logical.{Not, PredicateLike}
import com.wheaties.choice.iteration.IterationScheme

abstract class Ignore[-V] extends Choice[V] with PredicateLike[Ignore[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))
}