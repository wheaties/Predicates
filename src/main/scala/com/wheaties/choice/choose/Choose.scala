package com.wheaties.choice.choose

import com.wheaties.choice.{ChoiceL, ChoiceS, Choice}
import com.wheaties.logical.PredicateLike
import com.wheaties.choice.iteration._
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

trait Choose[-V] extends Choice[V] with PredicateLike[Choose[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

trait ChooseS[-V] extends ChoiceS[V] with PredicateLike[ChooseS[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

trait ChooseL extends ChoiceL with PredicateLike[ChooseL]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

object Choose{
  def all() = new ChooseL {
    protected[choice] def scheme = new AcceptAll
  }

  def every(n: Int) = new ChooseL {
    protected[choice] def scheme = new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int = 1) = new ChooseL{
    protected[choice] def scheme = new AcceptEveryF(f, init)
  }

  def first(n: Int) = new ChooseL{
    protected[choice] def scheme = new AcceptFirst(n)
  }

  def satisfying[V](pred: V => Boolean) = new ChooseS[V]{
    protected[choice] def scheme = new AcceptIf[V](pred)
  }

  def until[V](pred: V => Boolean) = new ChooseS[V]{
    protected[choice] def scheme = new AcceptUntil[V](pred)
  }

  def once[V](pred: V => Boolean) = new ChooseS[V]{
    protected[choice] def scheme = new AcceptOnce[V](pred)
  }
}