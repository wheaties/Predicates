package com.wheaties.choice.ignore

import com.wheaties.choice.{ChoiceL, ChoiceS, Choice}
import com.wheaties.logical.{Not, PredicateLike}
import com.wheaties.choice.iteration._
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

trait Ignore[-V] extends Choice[V] with PredicateLike[Ignore[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))
}

trait IgnoreS[-V] extends ChoiceS[V] with PredicateLike[IgnoreS[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))
}

trait IgnoreL extends ChoiceL with PredicateLike[IgnoreL]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))
}

object Ignore{
  def all() = new IgnoreL {
    protected[choice] def scheme = AcceptAll
  }

  def every(n: Int) = new IgnoreL {
    protected[choice] def scheme = new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int) = new IgnoreL{
    protected[choice] def scheme = new AcceptEveryF(f, init)
  }

  def first(n: Int) = new IgnoreL{
    protected[choice] def scheme = new AcceptFirst(n)
  }

  def satisfying[V](pred: V => Boolean) = new IgnoreS[V]{
    protected[choice] def scheme = new AcceptIf[V](pred)
  }

  def until[V](pred: V => Boolean) = new IgnoreS[V]{
    protected[choice] def scheme = new AcceptUntil[V](pred)
  }

  def once[V](pred: V => Boolean) = new IgnoreS[V]{
    protected[choice] def scheme = new AcceptOnce[V](pred)
  }
}