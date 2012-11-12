package com.wheaties.choice

//TODO: figure out signature for "from"
trait Choose{
  self =>

  def from[A,Result](collection: A): Result

  def compose(that: Choose): Choose = new Choose{
    def from[A,Result](collection: A) = self from(that from(collection))
  }

  def andThen(that: Choose): Choose = new Choose{
    def from[A,Result](collection: A) = that from(self from(collection))
  }
}