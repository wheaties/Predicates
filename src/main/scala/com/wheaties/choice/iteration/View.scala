package com.wheaties.choice.iteration

import scala.collection.TraversableLike

trait View[Elem,Collection] extends ((Collection, Elem => Boolean) => Collection)

trait ViewImplicits{
  implicit def viewTraversable[Elem, Repr <: TraversableLike[Elem, Repr]] = new View[Elem, Repr] {
    def apply(coll: Repr, pred: Elem => Boolean): Repr = coll filter pred
  }

  implicit def viewArray[Elem, Array[_]] = new View[Elem, Array[Elem]] {
    def apply(array: Array[Elem], pred: Elem => Boolean): Array[Elem] = array filter pred
  }

  //implicit def viewArray[Elem, Repr <: ArrayLike[Elem, Repr]]
}