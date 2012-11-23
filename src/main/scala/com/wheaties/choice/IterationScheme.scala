package com.wheaties.choice

import com.wheaties.logical.{Negation, Connective, PredicateLike}

//TODO: Think about how to do "last" and "exactly" strategies.
trait IterationScheme extends PredicateLike[IterationScheme]{
  self =>

  def accept[A](value: A): Boolean

  def compose(that: IterationScheme) = new IterationScheme {
    def accept[A](value: A) = (that accept (value)) && (self accept (value))
  }

  def andThen(that: IterationScheme) = that compose self
}

class AcceptAll extends IterationScheme{
  def accept[A](value: A) = true
}

class AcceptEvery(n: Int) extends IterationScheme{
  private var step = -1

  def accept[A](value: A) ={
    step += 1
    if(step == n) step = 0

    step == 0
  }
}

class AcceptFirst(n: Int) extends IterationScheme{
  private var count = -1

  def accept[A](value: A) ={
    count += 1

    count < n
  }
}

//How would this work if S defined before L?
class AcceptIf[B](pred: B => Boolean) extends IterationScheme{
  def accept[A <: B](value: A) = pred(value)
}

//TODO: This works for limiting up to but not over. It doesn't limit less than.
//TODO: throwing the exception here is bad. If composing two Choices with "and" condition, could cause problems.
class AcceptExactly(n: Int) extends IterationScheme{
  private var count = 0

  def accept[A](value: A) ={
    count += 1
    if(n < count) throw new Exception("Exceeded acceptable limit of %s" format n)

    true
  }
}

//TODO: needs to be implicit
//Unlike regular "and" both values must be evaluated
object IterationConnective extends Connective[IterationScheme,IterationScheme,IterationScheme]{
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

      if(pAcc)  qAcc else !qAcc
    }
  }
}

//Also needs to be an implicit
class IterationNegation extends Negation[IterationScheme]{
  def not(p: IterationScheme) = new IterationScheme {
    def accept[A](value: A) = !(p accept (value))
  }
}