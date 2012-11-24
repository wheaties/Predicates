package com.wheaties.choice.choose

import com.wheaties.choice.Choice
import com.wheaties.logical.PredicateLike
import com.wheaties.choice.iteration.IterationScheme
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

abstract class Choose[-V] extends Choice[V] with PredicateLike[Choose[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}