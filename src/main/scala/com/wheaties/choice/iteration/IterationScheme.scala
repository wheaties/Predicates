package com.wheaties.choice.iteration

import com.wheaties.logical.{Connective, Not, Negation, PredicateLike}



/**
 * The whole scheme of a predicate composable object that avoids high GC overhead while at the same time is not
 * mutable is something that I've struggled to represent. To grab the highest level of efficiency I need to use a
 * mutable accumulator of previous state. I don't like putting in mutable state unless it's within a self-contained
 * function and can never "leak" out. I'm going to violate this in the design of Choices until I can figure out a
 * more pure representation that doesn't place undo burdens on the garbage collector.
 *
 * TODO: Restrict these to be only accessible to the "choice" package.
 */

trait Accept[@specialized(Int, Long, Float, Double) -A] extends PredicateLike[Accept[A]]{
  self =>

  final def accept[AA <: A](value: AA) ={
    val flag = check(value)
    if(flag) next()
    flag
  }

  protected[iteration] def check[AA <: A](value: AA): Boolean
  protected[iteration] def next(){}

  def andThen[B <: A](that: Accept[B]) = new Accept[B] {
    protected[iteration] def check[BB <: B](value: BB) = (self accept value) && (that accept value)
  }
  def compose[B <: A](that: Accept[B]) = that andThen this
}

abstract class MultiScheme[@specialized(Int, Long, Float, Double) -A,
                           @specialized(Int, Long, Float, Double) -B <: A](p: Accept[A], q: Accept[B])
    extends Accept[A]{
  override protected[iteration] def next(){
    p next ()
    q next ()
  }
}

object Accept{
  implicit def neg[A] = new Negation[Accept[A]]{
    def not(that: Accept[A]) = new Accept[A] {
      protected[iteration] def check[AA <: A](value: AA) = !(that check value)
    }
  }

  implicit def con[A,B >: A] = new Connective[Accept[A],Accept[B],Accept[A]]{
    def and(p: Accept[A], q: Accept[B]): Accept[A] = new MultiScheme(p, q) {
      protected[iteration] def check[AA <: A](value: AA) = (p check (value)) && (q check (value))
    }

    def or(p: Accept[A], q: Accept[B]): Accept[A] = new MultiScheme(p, q) {
      protected[iteration] def check[AA <: A](value: AA) = (p check (value)) || (q check (value))
    }

    def xor(p: Accept[A], q: Accept[B]): Accept[A] = new MultiScheme(p, q) {
      def check[AA <: A](value: AA) = if(p check (value)) !(q check (value)) else q check (value)
    }

    def nand(p: Accept[A], q: Accept[B]) = Not(and(p, q))
    def nor(p: Accept[A], q: Accept[B]) = Not(or(p, q))
    def nxor(p: Accept[A], q: Accept[B]) = Not(xor(p, q))
  }
}