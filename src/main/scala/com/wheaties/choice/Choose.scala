package com.wheaties.choice

//TODO: figure out signature for "from"
trait Choice{
  self =>

  def from[A,Result](collection: A): Result

  def compose(that: Choice): Choice = new Choice{
    def from[A,Result](collection: A) = self from(that from(collection))
  }

  def andThen(that: Choice): Choice = new Choice{
    def from[A,Result](collection: A) = that from(self from(collection))
  }
}