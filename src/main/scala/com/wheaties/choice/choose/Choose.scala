package com.wheaties.choice.choose

import com.wheaties.choice.{ChoiceL, ChoiceS, Choice}
import com.wheaties.logical.{Connective, PredicateLike}
import com.wheaties.choice.iteration._
import com.wheaties.choice.getter.Getter
import com.wheaties.choice.setter.Setter

trait Choose[-V] extends Choice[V] with PredicateLike[Choose[V]]{
  self =>

  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)

  def every(n: Int) = new Choose[V] {
    protected[choice] def scheme = (self scheme) and new AcceptEvery(n)
  }

  def every(f: Int => Int, init: Int = 1) = new Choose[V]{
    protected[choice] def scheme = (self scheme) and new AcceptEveryF(f, init)
  }

  def first(n: Int) = new Choose[V]{
    protected[choice] def scheme = (self scheme) and new AcceptFirst(n)
  }

  def satisfying[V2 <: V](pred: V2 => Boolean) = new Choose[V]{
    protected[choice] def scheme = (self scheme) and new AcceptIf[V2](pred)
  }

  def until[V2 <: V](pred: V2 => Boolean) = new Choose[V]{
    protected[choice] def scheme = (self scheme) and new AcceptUntil[V2](pred)
  }

  def once[V2 <: V](pred: V2 => Boolean) = new Choose[V]{
    protected[choice] def scheme = (self scheme) and new AcceptOnce[V2](pred)
  }

  protected[choose] implicit def con[V2 >: V] = new Connective[Choose[V],Choose[V2],Choose[V]]{
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
}