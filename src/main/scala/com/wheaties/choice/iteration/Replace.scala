package com.wheaties.choice.iteration

//TODO: change to implicit object that creates functions

trait Replace[Elem,+Action] extends (Elem => Elem){
  def apply(elem: Elem): Elem
}

class Substitute[Elem](value: Elem) extends Replace[Elem,Elem]{
  def apply(elem: Elem) = value
}

class SubIterable[Elem,To,I[To] <: Iterable[Elem]](value: I[To]) extends Replace[Elem,I[To]]{
  private val iter = value.iterator

  def apply(elem: Elem) = if(iter hasNext) iter next () else elem
}

class SubStream[Elem,To,S[To] <: Stream[Elem]](var value: S[To]) extends Replace[Elem,S[To]]{
  def apply(elem: Elem) = value match{
    case head #:: tail => value = tail; head
    case _ => elem
  }
}

class SubArray[Elem : ClassManifest, To, A[To] <: Array[Elem]](value: A[To]) extends Replace[Elem,A[To]]{
  private val iter = value.iterator

  def apply(elem: Elem) = if(iter hasNext) iter next () else elem
}

class SubKeys[Key, To, Value, C[To] <: Iterable[Key]](value: C[To])
    extends Replace[(Key,Value),C[To]]{
  private val iter = value.iterator

  def apply(elem: (Key,Value)) = if(iter hasNext) (iter next (), elem._2) else elem
}