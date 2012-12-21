package com.wheaties.choice.ignore

import com.wheaties.choice.Choice
import com.wheaties.logical.{Connective, Not, PredicateLike}
import com.wheaties.choice.iteration._
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

trait Ignore[-V] extends Choice[V] with PredicateLike[Ignore[V]]{
  self =>

  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, Not(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, Not(scheme))

  def every(n: Int) = new Ignore[V] {
    protected[choice] def scheme = (self scheme) and new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int) = new Ignore[V]{
    protected[choice] def scheme = (self scheme) and new AcceptEveryF(f, init)
  }

  def first(n: Int) = new Ignore[Any]{
    protected[choice] def scheme = (self scheme) and new AcceptFirst(n)
  }

  def satisfying[V](pred: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme = (self scheme) and new AcceptIf[V](pred)
  }

  def until[V](pred: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme = (self scheme) and new AcceptUntil[V](pred)
  }

  def once[V](pred: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme = (self scheme) and new AcceptOnce[V](pred)
  }

  protected[ignore] def con[V2 >: V] = new Connective[Ignore[V],Ignore[V2],Ignore[V]]{
    def and(p: Ignore[V], q: Ignore[V2]) = new Ignore[V] {
      protected[choice] def scheme = (p scheme) and (q scheme)
    }

    def or(p: Ignore[V], q: Ignore[V2]) = new Ignore[V] {
      protected[choice] def scheme = (p scheme) or (q scheme)
    }

    def xor(p: Ignore[V], q: Ignore[V2]) = new Ignore[V] {
      protected[choice] def scheme = (p scheme) xor (q scheme)
    }

    def nand(p: Ignore[V], q: Ignore[V2]) = new Ignore[V] {
      protected[choice] def scheme = (p scheme) nand (q scheme)
    }

    def nor(p: Ignore[V], q: Ignore[V2]) = new Ignore[V] {
      protected[choice] def scheme = (p scheme) nor (q scheme)
    }

    def nxor(p: Ignore[V], q: Ignore[V2]) = new Ignore[V] {
      protected[choice] def scheme = (p scheme) nxor (q scheme)
    }
  }
}

object Ignore{
  def all() = new Ignore[Any] {
    protected[choice] def scheme = AcceptAll
  }

  def every(n: Int) = new Ignore[Any] {
    protected[choice] def scheme = new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int) = new Ignore[Any]{
    protected[choice] def scheme = new AcceptEveryF(f, init)
  }

  def first(n: Int) = new Ignore[Any]{
    protected[choice] def scheme = new AcceptFirst(n)
  }

  def satisfying[V](pred: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme = new AcceptIf[V](pred)
  }

  def until[V](pred: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme = new AcceptUntil[V](pred)
  }

  def once[V](pred: V => Boolean) = new Ignore[V]{
    protected[choice] def scheme = new AcceptOnce[V](pred)
  }
}