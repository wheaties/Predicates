package com.wheaties.choice.iteration

//Really want

trait Replace[-Elem,+To] extends (Elem => To)

class ReplaceF[Elem,To](f: Elem => To) extends (Elem => To){
  def apply(elem: Elem) = f(elem)
}

class SubValue[Elem,To <: Elem](value: To) extends Replace[Elem,To]{
  def apply(elem: Elem) = value
}

class SubIterable[Elem,To <: Elem,I[_] <: Iterable[_]](value: I[To]) extends Replace[Elem,To]{
  private val iter = value.toIterator
  def apply(elem: Elem) = if(iter hasNext) iter next () else elem
}

class SubStream[Elem,To <: Elem,S[_] <: Stream[_]](var value: S[To]) extends Replace[Elem,To]{
  def apply(elem: Elem) = value match{
    case head :#: tail => value = tail; head
    case _ => elem
  }
}

class SubArray[Elem, To <: Elem,Array[To]](value: Array[To]) extends Replace[Elem,To]{
  private val iter = value.toIterator
  def apply(elem: Elem) = if(iter hasNext) iter next () else elem
}