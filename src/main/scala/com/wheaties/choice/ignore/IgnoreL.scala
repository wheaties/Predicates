package com.wheaties.choice.ignore

import com.wheaties.choice.ChoiceL
import com.wheaties.logical.{Not, PredicateLike}
import com.wheaties.choice.iteration._
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

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

class IgnoreEveryF(f: Int => Int, init: Int) extends IgnoreL{
  protected[choice] def scheme = new AcceptEveryF(f, init)
}

class IgnoreFirst(n: Int) extends IgnoreL{
  protected[choice] def scheme = new AcceptFirst(n)
}