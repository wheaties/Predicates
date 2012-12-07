package com.wheaties.choice.iteration

import com.wheaties.logical.{Connective, Not, Negation, PredicateLike}

//TODO: Think about how to do "last" and "exactly" strategies.
trait IterationScheme extends PredicateLike[IterationScheme]{
  self =>

  def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int): Boolean

  def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int): IterationScheme

  def compose(that: IterationScheme): IterationScheme = new IterationScheme {
    def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
      (that accept (value, count)) && (self accept (value, count))

    def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = {
      val selfN = self next (value, count)
      val thatN = that next (value, count)

      if(selfN == self && thatN == that) this else selfN compose thatN
    }
  }

  def andThen(that: IterationScheme): IterationScheme = that compose self

  protected[iteration] implicit def neg = new Negation[IterationScheme]{
    def not(that: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) = !(that accept (value, count))

      def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val thatN = that next (value, count)

        if(thatN == that) this else Not(thatN)
      }
    }
  }

  protected[iteration] implicit def con = new Connective[IterationScheme,IterationScheme,IterationScheme]{
    def and(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        (p accept (value, count)) && (q accept (value, count))

      def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val pNext = p next (value, count)
        val qNext = q next (value, count)

        if(pNext == p && qNext == q) this else pNext and qNext
      }
    }

    def or(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        (p accept (value, count)) || (q accept (value, count))

      def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val pNext = p next (value, count)
        val qNext = q next (value, count)

        if(pNext == p && qNext == q) this else pNext or qNext
      }
    }

    def xor(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) !(q accept (value, count)) else q accept (value, count)

      def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val pNext = p next (value, count)
        val qNext = q next (value, count)

        if(pNext == p && qNext == q) this else pNext xor qNext
      }
    }

    def nand(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) false else !(q accept (value, count))

      def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val pNext = p next (value, count)
        val qNext = q next (value, count)

        if(pNext == p && qNext == q) this else pNext nand qNext
      }
    }

    def nor(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) !(q accept (value, count)) else q accept (value, count)

      def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val pNext = p next (value, count)
        val qNext = q next (value, count)

        if(pNext == p && qNext == q) this else pNext nor qNext
      }
    }

    def nxor(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[@specialized(Int, Long, Float, Double) A](value: A, count: Int) =
        if(p accept (value, count)) q accept (value, count) else !(q accept (value, count))

      def next[@specialized(Int, Long, Float, Double) A](value: A, count: Int) ={
        val pNext = p next (value, count)
        val qNext = q next (value, count)

        if(pNext == p && qNext == q) this else pNext nxor qNext
      }
    }
  }
}