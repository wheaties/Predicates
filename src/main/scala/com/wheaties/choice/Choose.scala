package com.wheaties.choice

//TODO: there's got to be a way to "partiion" things
//TODO: there's got to be a way to "mod" things, think maybe need Mapper[In,Mapping,Out]
trait Choice{
  self =>

  def get[A](collection: A)(implicit getter: Getter[A]): A
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]): A

  def compose(that: Choice): Choice = that andThen this

  def andThen(that: Choice): Choice = new Choice{
    def get[A](collection: A)(implicit getter: Getter[A]) = that get (self get (collection))
    def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = 
      self set(collection, that set (self get (collection), value))
  }
}

trait Getter[A]{
  def get(collection: A, scheme: IterationScheme): A
}

trait Setter[A,B]{
  def set(collection: A, value: B, scheme: IterationScheme): A
}
