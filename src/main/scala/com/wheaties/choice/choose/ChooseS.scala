package com.wheaties.choice.choose

import com.wheaties.choice.{Setter, Getter, ChoiceS}
import com.wheaties.logical.PredicateLike
import com.wheaties.choice.iteration.{AcceptIf, IterationScheme}

abstract class ChooseS[-V] extends ChoiceS[V] with PredicateLike[ChooseS[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

class ChooseIf[-V](pred: V => Boolean) extends ChooseS[V] with PredicateLike[ChooseS[V]]{
  protected[choice] def scheme = new AcceptIf[V](pred)
}