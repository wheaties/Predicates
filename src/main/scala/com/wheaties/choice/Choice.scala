package com.wheaties.choice

import iteration._
import com.wheaties.logical.{Not, Negation}

trait Choice[-Value]{
  self =>

  protected[choice] def scheme: Accept[Value]
  protected[choice] def iteration(iter: Accept[Value]): Accept[Value]

  //TODO: this only works for List[_] types
  def get[C[_]](collection: C[Value])(implicit traverse: Traverse[Value,Value,C], acc: Accum[Value,C[Value]]) ={
    val (out,_) = traverse(collection, acc, NullAccumulator, iteration(scheme))

    out result ()
  }

  //TODO: the replace here is wrong!
  def set[A,C[_]](collection: C[Value], value: A)(implicit acc: Accum[C,A], replace: Replace[Value,A]) ={
    traverse(collection, acc, acc, iteration(scheme))

    acc result ()
  }

  def mod[A,C](collection: C[Value], f: Value => A)(implicit acc: Accum[A,C[A]], replace: Replace[Value,A]) ={
    traverse(replace rep (acc, f), acc, collection)

    acc result ()
  }

  def partition[A,B](collection: A)(implicit left: Accum[B,A], right: Accum[B,A]) ={
    traverse(left, right, collection)

    (left result (), right result ())
  }

  //def fold[A,B,C](collection: A, init: C, f: B)(implicit acc: Accum[C,A], fold: Fold[C]) ={
  //  traverse(acc map fold(f, init), NullAccumulator, collection)
  //
  //  acc result ()
  //}

  //def reduce[A,B,C](collection: A, f: B)(implicit acc: Accum[C,A], reduce: Reduce[C]) ={
  //  traverse(acc map reduce(f), NullAccumulator, collection)
  //
  //  acc result ()
  //}

  def compose[V <: Value](that: Choice[V]) = that andThen this

  def andThen[V <: Value](that: Choice[V]) = new Choice[V]{
    protected[choice] def scheme = self iteration (self scheme) andThen (that iteration (that scheme))
    protected[choice] def iteration(iter: Accept[V]) = iter
  }
}

object Choice{
  implicit def neg[Value] = new Negation[Choice[Value]] {
    def not(that: Choice[Value]) = new Choice[Value] {
      protected[choice] def scheme = Not(that scheme)
      protected[choice] def iteration(iter: Accept[Value]) = iter
    }
  }
}