package com.wheaties.choice.ignore

import com.wheaties.choice.{Setter, Getter, ChoiceL}
import com.wheaties.logical.{Not, PredicateLike}
import com.wheaties.choice.iteration.{AcceptFirst, AcceptEvery, AcceptAll, IterationScheme}

abstract class IgnoreL extends ChoiceL with PredicateLike[IgnoreL]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))
}

object IgnoreAll extends IgnoreL{
  protected[choice] def scheme = new AcceptAll
}

class IgnoreEvery(n: Int) extends IgnoreL{
  protected[choice] def scheme = new AcceptEvery(n)
}

class IgnoreFirst(n: Int) extends IgnoreL{
  protected[choice] def scheme = new AcceptFirst(n)
}