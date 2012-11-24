package com.wheaties.choice.ignore

import com.wheaties.choice.ChoiceS
import com.wheaties.logical.{Not, PredicateLike}
import com.wheaties.choice.iteration.{AcceptOnce, AcceptUntil, AcceptIf, IterationScheme}
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

abstract class IgnoreS[-V] extends ChoiceS[V] with PredicateLike[IgnoreS[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))
}

class IgnoreIf[-V](pred: V => Boolean) extends IgnoreS[V] with PredicateLike[IgnoreS[V]]{
  protected[choice] def scheme = new AcceptIf[V](pred)
}

class IgnoreUntil[-V](pred: V => Boolean) extends IgnoreS[V] with PredicateLike[IgnoreS[V]]{
  protected[choice] def scheme = new AcceptUntil[V](pred)
}

class IgnoreOnce[-V](pred: V => Boolean) extends IgnoreS[V] with PredicateLike[IgnoreS[V]]{
  protected[choice] def scheme = new AcceptOnce[V](pred)
}