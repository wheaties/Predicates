package com.wheaties.choice

import com.wheaties.logical.{Connective, PredicateLike}
import com.wheaties.choice.iteration._

trait Choose[-V] extends Choice[V] with PredicateLike[Choose[V]]{
  self =>

  protected[choice] def iteration(iter: Accept[V]) = iter

  def every(n: Int) = new Choose[V] {
    protected[choice] def scheme = (self scheme) andThen new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int = 1) = new Choose[V]{
    protected[choice] def scheme = (self scheme) andThen new AcceptEveryF(f, init)
  }

  def first(n: Int) = new Choose[V]{
    protected[choice] def scheme = (self scheme) andThen new AcceptFirst(n)
  }

  def satisfying[V1 <: V](pred: V1 => Boolean) = new Choose[V1]{
    protected[choice] def scheme = (self scheme) andThen new AcceptIf[V1](pred)
  }

  def until[V1 <: V](pred: V1 => Boolean) = new Choose[V1]{
    protected[choice] def scheme = (self scheme) andThen new AcceptUntil[V1](pred)
  }

  def once[V1 <: V](pred: V1 => Boolean) = new Choose[V1]{
    protected[choice] def scheme = (self scheme) andThen new AcceptOnce[V1](pred)
  }
}

object Choose{
  def all = new Choose[Any] {
    protected[choice] def scheme = AcceptAll
  }

  def every(n: Int) = new Choose[Any] {
    protected[choice] def scheme = new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int = 1) = new Choose[Any]{
    protected[choice] def scheme = new AcceptEveryF(f, init)
  }

  def first(n: Int) = new Choose[Any]{
    protected[choice] def scheme = new AcceptFirst(n)
  }

  def satisfying[V](pred: V => Boolean) = new Choose[V]{
    protected[choice] def scheme = new AcceptIf[V](pred)
  }

  def until[V](pred: V => Boolean) = new Choose[V]{
    protected[choice] def scheme = new AcceptUntil[V](pred)
  }

  def once[V](pred: V => Boolean) = new Choose[V]{
    protected[choice] def scheme = new AcceptOnce[V](pred)
  }

  protected[choice] implicit def con[V,V2 >: V] = new Connective[Choose[V],Choose[V2],Choose[V]]{
    def and(p: Choose[V], q: Choose[V2]) = new Choose[V] {
      protected[choice] def scheme = (p scheme) and (q scheme)
    }

    def or(p: Choose[V], q: Choose[V2]) = new Choose[V] {
      protected[choice] def scheme = (p scheme) or (q scheme)
    }

    def xor(p: Choose[V], q: Choose[V2]) = new Choose[V] {
      protected[choice] def scheme = (p scheme) xor (q scheme)
    }

    def nand(p: Choose[V], q: Choose[V2]) = new Choose[V] {
      protected[choice] def scheme = (p scheme) nand (q scheme)
    }

    def nor(p: Choose[V], q: Choose[V2]) = new Choose[V] {
      protected[choice] def scheme = (p scheme) nor (q scheme)
    }

    def nxor(p: Choose[V], q: Choose[V2]) = new Choose[V] {
      protected[choice] def scheme = (p scheme) nxor (q scheme)
    }
  }
}