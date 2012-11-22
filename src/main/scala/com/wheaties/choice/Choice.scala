package com.wheaties.choice

//TODO: there's got to be a way to "partiion" things
trait Choice{
  self =>

  def get[A](collection: A)(implicit getter: Getter[A]): A
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]): A
  //def mod[A,B,C](collection: A, f: B)(implicit modifer: Modder[A,B,C]): C

  def compose(that: Choice): Choice = that andThen this

  def andThen(that: Choice): Choice = new Choice{
    def get[A](collection: A)(implicit getter: Getter[A]) = that get (self get (collection))
    def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = 
      self set(collection, that set (self get (collection), value))
    //def mod[A,B,C](collection: A, f: B)(implicit modifer: Modder[A,B,C]) =
    //  self set(collection, that mod(self get (collection), f))
  }
}

trait Getter[A]{
  def get(collection: A, scheme: IterationScheme): A
}

trait Setter[A,B]{
  def set(collection: A, value: B, scheme: IterationScheme): A
}

//trait Modder[In,Mapper,Out]{
//  def mod(collection: In, f: Mapper): Out
//}