package com.wheaties.choice

import com.wheaties.choice.setter.Setter
import com.wheaties.choice.getter.Getter
import iteration.IterationScheme
import com.wheaties.logical.{Not, Negation}

//TODO: there's got to be a way to fold
//TODO: there's got to be a way to reduce too!
trait Choice[-Value]{
  self =>

  protected[choice] def scheme: IterationScheme
  protected[choice] def iteration(iter: IterationScheme): IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, iteration(scheme))
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, iteration(scheme))
  //def mod[A,B](collection: A, f: B)(implicit setter: Setter[A,B]) = setter mod (collection, f, iteration scheme)
  def partition[A](collection: A)(implicit getter: Getter[A]) = getter partition (collection, iteration(scheme))
  //def fold[A,B,C](collection: A, f: B)(implicit folder: Folder[A,B,C]) = folder fold (collection, f, iteration scheme)
  //def reduce[A,B,C](collection: A, f: B)(implicit folder: Folder[A,B,C]) = folder reduce (collection, f, iteration scheme)

  def compose[V <: Value](that: Choice[V]): Choice[V] = that andThen this

  def andThen[V >: Value](that: Choice[V]): Choice[Value] = new Choice[Value]{
    protected[choice] def scheme = self iteration (self scheme) andThen (that iteration (that scheme))
    protected[choice] def iteration(iter: IterationScheme) = iter
  }

  implicit def neg = new Negation[Choice[Value]] {
    def not(that: Choice[Value]) = new Choice[Value] {
      protected[choice] def scheme = Not(that scheme)
      protected[choice] def iteration(iter: IterationScheme) = iter
    }
  }
}