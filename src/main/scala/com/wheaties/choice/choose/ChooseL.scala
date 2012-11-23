package com.wheaties.choice.choose

import com.wheaties.choice.{Setter, Getter, ChoiceL}
import com.wheaties.logical.PredicateLike
import com.wheaties.choice.iteration.{AcceptFirst, AcceptEvery, AcceptAll, IterationScheme}

abstract class ChooseL extends ChoiceL with PredicateLike[ChooseL]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

object ChooseAll extends ChooseL{
  protected[choice] def scheme = new AcceptAll
}

class ChooseEvery(n: Int) extends ChooseL{
  protected[choice] def scheme = new AcceptEvery(n)
}

class ChooseFirst(n: Int) extends ChooseL{
  protected[choice] def scheme = new AcceptFirst(n)
}