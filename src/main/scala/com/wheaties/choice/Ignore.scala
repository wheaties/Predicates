package com.wheaties.choice

import com.wheaties.logical.{Not, Connective, PredicateLike}
import com.wheaties.choice.iteration._

trait Ignore[-V] extends Choice[V] with PredicateLike[Ignore[V]]{
  self =>

  protected[choice] def iteration(iter: Accept[V]) = Not(iter)

  def every(n: Int) = new Ignore[V] {
    protected[choice] def scheme = (self scheme) andThen new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int) = new Ignore[V]{
    protected[choice] def scheme = (self scheme) andThen new AcceptEveryF(f, init)
  }

  def first(n: Int) = new Ignore[V]{
    protected[choice] def scheme = (self scheme) andThen new AcceptFirst(n)
  }

  def satisfying[V1 <: V](pred: V1 => Boolean) = new Ignore[V1]{
    protected[choice] def scheme = (self scheme) andThen new AcceptIf[V1](pred)
  }

  def until[V1 <: V](pred: V1 => Boolean) = new Ignore[V1]{
    protected[choice] def scheme = (self scheme) andThen new AcceptUntil[V1](pred)
  }

  def once[V1 <: V](pred: V1 => Boolean) = new Ignore[V1]{
    protected[choice] def scheme = (self scheme) andThen new AcceptOnce[V1](pred)
  }
}

object Ignore{
  def all = new Ignore[Any] {
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

  protected[choice] def con[V,V2 >: V] = new Connective[Ignore[V],Ignore[V2],Ignore[V]]{
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