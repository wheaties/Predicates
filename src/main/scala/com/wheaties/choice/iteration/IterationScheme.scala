package com.wheaties.choice.iteration

import com.wheaties.logical.PredicateLike

//TODO: Think about how to do "last" and "exactly" strategies.
trait IterationScheme extends PredicateLike[IterationScheme]{
  self =>

  def accept[A](value: A): Boolean

  def compose(that: IterationScheme) = new IterationScheme {
    def accept[A](value: A) = (that accept (value)) && (self accept (value))
  }

  def andThen(that: IterationScheme) = that compose self
}