package com.wheaties.choice.iteration

import com.wheaties.logical.{Connective, Not, Negation, PredicateLike}

/**
 * The whole scheme of a predicate composeable object that avoids high GC overhead while at the same time is not
 * mutable is something that I've struggled to represent. To grab the highest level of efficiency I need to use a
 * mutable accumulator of previous state. I don't like putting in mutable state unless it's within a self-contained
 * function and can never "leak" out. I'm going to violate this in the design of Choices.
 *
 * TODO: Restrict these to be only accessible to the "choice" package.
 */

//TODO: want first(n) satisfying(pred) to be "counted" up only when satisfying that condition, no?
trait IterationScheme extends PredicateLike[IterationScheme]{
  self =>

  def accept[@specialized(Int, Long, Float, Double) A](value: A) ={
    val flag = check(value)
    if(flag) next()
    flag
  }

  protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A): Boolean
  protected[iteration] def next(){}

  def andThen(that: IterationScheme) = new IterationScheme {
    protected[iteration] def check[A](value: A) = (self accept value) && (that accept value)
  }
  def compose(that: IterationScheme) = that andThen this

  protected[iteration] implicit def neg = new Negation[IterationScheme]{
    def not(that: IterationScheme) = new IterationScheme {
      protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) = !(that check value)
    }
  }

  protected[iteration] implicit def con = new Connective[IterationScheme,IterationScheme,IterationScheme]{
    def and(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) =
        (p check (value)) && (q check (value))
    }

    def or(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      protected[iteration] def check[@specialized(Int, Long, Float, Double) A](value: A) =
        (p check (value)) || (q check (value))
    }

    def xor(p: IterationScheme, q: IterationScheme) = new MultiScheme(p, q) {
      def check[@specialized(Int, Long, Float, Double) A](value: A) =
        if(p check (value)) !(q check (value)) else q check (value)
    }

    def nand(p: IterationScheme, q: IterationScheme) = Not(and(p, q))

    def nor(p: IterationScheme, q: IterationScheme) = Not(or(p, q))

    def nxor(p: IterationScheme, q: IterationScheme) = Not(xor(p, q))
  }
}

abstract class MultiScheme(p: IterationScheme, q: IterationScheme) extends IterationScheme{
  override protected[iteration] def next(){
    p next ()
    q next ()
  }
}