package com.wheaties.choice.ignore

import com.wheaties.choice.Choice
import com.wheaties.logical.{Not, PredicateLike}
import com.wheaties.choice.iteration.IterationScheme
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

abstract class Ignore[-V] extends Choice[V] with PredicateLike[Ignore[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))
}