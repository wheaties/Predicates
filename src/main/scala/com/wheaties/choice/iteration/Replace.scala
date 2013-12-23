package com.wheaties.choice.iteration

import scala.collection.TraversableLike
import scala.collection.generic._

trait Replace[Elem,Collection,Sub] extends ((Collection, Sub, Elem => Boolean) => Collection)

//TODO: Missing mutable...
trait ReplaceImplicits{
  implicit def repTraversable[Elem, Repr <: TraversableLike[Elem, Repr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, Elem] {
      def apply(coll: Repr, value: Elem, pred: Elem => Boolean): Repr ={
        def sub(elem: Elem) = if(pred(elem)) value else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def repTraversable[Elem, Array[_]] = new Replace[Elem, Array[Elem], Elem] {
    def apply(coll: Array[Elem], value: Elem, pred: Elem => Boolean): Array[Elem] ={
      def sub(elem: Elem) = if(pred(elem)) value else elem

      coll map sub
    }
  }

  implicit def repTraversableSub[Elem, Sub <: Elem, Repr <: TraversableLike[Elem, Repr], From <: TraversableLike[Sub, From]](implicit cbf: CanBuildFrom[Repr, Elem, Repr])=
    new Replace[Elem, Repr, From] {
      def apply(coll: Repr, value: From, pred: Elem => Boolean): Repr ={
        val iter = value.toIterator
        def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def repTraversableArray[Elem, Sub <: Elem, Repr <: TraversableLike[Elem, Repr], Array[_]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, Array[Sub]] {
      def apply(coll: Repr, value: Array[Sub], pred: Elem => Boolean): Repr ={
        val iter = value.toIterator
        def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def repArraySub[Elem, Sub <: Elem, Array[_]] = new Replace[Elem, Array[Elem], Array[Sub]] {
    def apply(coll: Array[Elem], value: Array[Sub], pred: Elem => Boolean): Array[Elem] ={
      val iter = value.toIterator
      def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

      coll map sub
    }
  }
}