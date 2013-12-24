package com.wheaties.choice.iteration

import scala.collection.TraversableLike
import collection.generic.CanBuildFrom
import collection.mutable.ArrayLike

trait Replace[+Elem,Collection,+Sub] extends ((Collection, Sub, Elem => Boolean) => Collection)

//TODO: Missing mutable...
trait ReplaceImplicits{
  implicit def repTraversable[Elem, Repr <: TraversableLike[Elem, Repr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, Elem] {
      def apply(coll: Repr, value: Elem, pred: Elem => Boolean): Repr ={
        def sub(elem: Elem) = if(pred(elem)) value else elem

        coll.map(sub)(cbf)
      }
    }

//  abstract class TravReplace[Elem, Repr <: TraversableLike[Elem, Repr], Sub](implicit cbf: CanBuildFrom[Repr, Elem, Repr])
//      extends Replace[Elem, Repr, Sub]{
//    def apply(coll: Repr, value: Elem, pred: Elem => Boolean): Repr ={
//      def sub(elem: Elem) = if(pred(elem)) value else elem
//
//      coll.map(sub)(cbf)
//    }
//  }

//  implicit def repList[Elem, List[Elem]] = new TravReplace[Elem, List[Elem], Elem]{}

  implicit def repArray[Elem, Repr <: ArrayLike[Elem, Repr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, Elem] {
      def apply(coll: Repr, value: Elem, pred: Elem => Boolean): Repr ={
        def sub(elem: Elem) = if(pred(elem)) value else elem

        coll.map(sub)(cbf)
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

  implicit def repTraversableArray[Elem, Sub <: Elem, Repr <: TraversableLike[Elem, Repr], ARepr <: ArrayLike[Sub, ARepr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, ARepr] {
      def apply(coll: Repr, value: ARepr, pred: Elem => Boolean): Repr ={
        val iter = value.toIterator
        def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def repArraySub[Elem, Sub <: Elem, Repr <: ArrayLike[Elem, Repr], ReprSub <: ArrayLike[Sub, ReprSub]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, ReprSub] {
      def apply(coll: Repr, value: ReprSub, pred: Elem => Boolean): Repr ={
        val iter = value.toIterator
        def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

        coll.map(sub)(cbf)
      }
  }
}