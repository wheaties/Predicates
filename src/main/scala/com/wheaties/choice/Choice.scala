package com.wheaties.choice

import com.wheaties.choice.setter.Setter
import com.wheaties.choice.getter.Getter
import iteration.{Accept, IterationScheme}
import com.wheaties.logical.{Not, Negation}

//TODO: make most of these methods defer to a single method of type
//def traverse(acc: Accum[A], acc: Accum[A], iter: IterationScheme[B], collection: A): (Accum[A], Accum[A])
trait Choice[-Value]{
  self =>

  protected[choice] def scheme: Accept[Value]
  protected[choice] def iteration(iter: Accept[Value]): Accept[Value]

  //TODO: Accumulator + Do Nothing Accumulator
  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, iteration(scheme))

  //TODO: Accumulator w/ Replacer
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, iteration(scheme))

  //TODO: Accumulator w/ Replacer
  //def mod[A,B](collection: A, f: B)(implicit setter: Setter[A,B]) = setter mod (collection, f, iteration scheme)

  //TODO: 2x Accumulator
  def partition[A](collection: A)(implicit getter: Getter[A]) = getter partition (collection, iteration(scheme))
  //def fold[A,B,C](collection: A, f: B)(implicit folder: Folder[A,B,C]) = folder fold (collection, f, iteration scheme)
  //def reduce[A,B,C](collection: A, f: B)(implicit folder: Folder[A,B,C]) = folder reduce (collection, f, iteration scheme)

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