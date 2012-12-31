package com.wheaties.choice

import com.wheaties.choice.setter.Setter
import com.wheaties.choice.getter.Getter

//TODO: there's got to be a way to "partiion" things such that it composes without going to great lengths...
//TODO: there's got to be a way to fold
//TODO: there's got to be a way to reduce too!
trait Choice[-Value]{
  self =>

  def get[A](collection: A)(implicit getter: Getter[A]): A
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]): A
  //def mod[A,B](collection: A, f: B)(implicit setter: Setter[A,B]): A

  def compose[V <: Value](that: Choice[V]): Choice[V] = that andThen this

  def andThen[V >: Value](that: Choice[V]): Choice[Value] = new Choice[Value]{
    def get[A](collection: A)(implicit getter: Getter[A]) = that get (self get (collection))
    def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = 
      self set(collection, that set (self get (collection), value))
    //def mod[A,B](collection: A, f: B)(implicit setter: Setter[A,B]) =
    //  self set(collection, that mod(self get (collection), f))
  }
}

////should this be able to handle List[A],A=>B,List[B] as well as List[A],List[A]=>B,B? No! Don't need F[A=>B] yet.
//trait Modder[In,Func,Out]{
//  def mod(collection: In, f: Func): Out
//}