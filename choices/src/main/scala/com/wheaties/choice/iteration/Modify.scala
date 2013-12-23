package com.wheaties.choice.iteration

import scala.collection.TraversableLike
import scala.collection.generic._
import collection.mutable.ArrayLike

trait Modify[Elem, Collection] extends ((Collection, Elem => Elem, Elem => Boolean) => Collection)

trait ModifyImplicits{
  implicit def modTraversable[Elem, Repr <: TraversableLike[Elem, Repr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Modify[Elem, Repr] {
      def apply(coll: Repr, f: Elem => Elem, pred: Elem => Boolean): Repr ={
        def sub(elem: Elem) = if(pred(elem)) f(elem) else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def modArray[Elem, Repr <: ArrayLike[Elem, Repr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Modify[Elem, Repr] {
      def apply(coll: Repr, f: Elem => Elem, pred: Elem => Boolean): Repr ={
        def sub(elem: Elem) = if(pred(elem)) f(elem) else elem

        coll.map(sub)(cbf)
      }
    }
}