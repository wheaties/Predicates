package com.wheaties.choice

import com.wheaties.logical.{Connective, Negation}

package object iteration{
  implicit object IterationNegation extends Negation[IterationScheme]{
    def not(p: IterationScheme) = new IterationScheme {
      def accept[A](value: A) = !(p accept (value))
    }
  }

  implicit object IterationConnective extends Connective[IterationScheme,IterationScheme,IterationScheme]{
    def and(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[A](value: A) ={
        val pAcc = p accept(value)
        val qAcc = q accept(value)

        pAcc && qAcc
      }
    }

    def or(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[A](value: A) ={
        val pAcc = p accept(value)
        val qAcc = q accept(value)

        pAcc || qAcc
      }
    }

    def xor(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[A](value: A) ={
        val pAcc = p accept(value)
        val qAcc = q accept(value)

        if(pAcc) !qAcc else qAcc
      }
    }

    def nand(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[A](value: A) ={
        val pAcc = p accept(value)
        val qAcc = q accept(value)

        !(pAcc && qAcc)
      }
    }

    def nor(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[A](value: A) ={
        val pAcc = p accept(value)
        val qAcc = q accept(value)

        !(pAcc || qAcc)
      }
    }

    def nxor(p: IterationScheme, q: IterationScheme) = new IterationScheme {
      def accept[A](value: A) ={
        val pAcc = p accept(value)
        val qAcc = q accept(value)

        if(pAcc) qAcc else !qAcc
      }
    }
  }
}