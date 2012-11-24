package com.wheaties.choice

import com.wheaties.choice.iteration._

package object ignore {
  implicit def conCL[V, I1 <: Ignore[V], I2 <: IgnoreL] = new IgnoreConCL[V]
  implicit def conCS[V1, V2 >: V1, I1 <: Ignore[V1], I2 <: IgnoreS[V2]] = new IgnoreConCS[V1,V2]
  implicit def conCC[V1, V2 >: V1, I1 <: Ignore[V1], I2 <: Ignore[V2]] = new IgnoreConCC[V1,V2]

  implicit def conSS[V1, V2 >: V1, I1 <: IgnoreS[V1], I2 <: IgnoreS[V2]] = new IgnoreConSS[V1,V2]
  implicit def conSL[V, I1<: IgnoreS[V], I2<: IgnoreL] = new IgnoreConLS[V]
  implicit def conSC[V1, V2 >: V1, I1 <: IgnoreS[V1], I2 <: Ignore[V2]] = new IgnoreConSC[V1,V2]

  implicit def conLL[I1 <: IgnoreL, I2 <: IgnoreL] = IgnoreConLL
  implicit def conLS[V, I1<: IgnoreL, I2<: IgnoreS[V]] = new IgnoreConLS[V]
  implicit def conLC[V, I1 <: IgnoreL, I2 <: Ignore[V]] = new IgnoreConLC[V]

  implicit def cond[I <: IgnoreL] = new Conditional[IgnoreL,Ignore[_]]{
    def condition[B](c: IgnoreL, pred: B => Boolean) = new Ignore[B]{
      protected[choice] def scheme = (c scheme) andThen (new AcceptIf[B](pred))
    }
    def until[B](c: IgnoreL, pred: B => Boolean) = new Ignore[B] {
      protected[choice] def scheme = (c scheme) andThen (new AcceptUntil[B](pred))
    }
    def once[B](c: IgnoreL, pred: B => Boolean) = new Ignore[B] {
      protected[choice] def scheme = (c scheme) andThen (new AcceptOnce[B](pred))
    }
  }

  implicit def limit[V] = new Limit[V,IgnoreS[V],Ignore[V]]{
    def every(c: IgnoreS[V], n: Int) = new Ignore[V]{
      protected[choice] def scheme = (c scheme) andThen (new AcceptEvery(n))
    }

    def every(c: IgnoreS[V], f: Int => Int, n: Int = 1) = new Ignore[V] {
      protected[choice] def scheme = (c scheme) andThen (new AcceptEveryF(f, n))
    }

    def all(c: IgnoreS[V]) = new Ignore[V] {
      protected[choice] def scheme = c scheme
    }

    def first(c: IgnoreS[V], n: Int) = new Ignore[V] {
      protected[choice] def scheme = (c scheme) andThen (new AcceptFirst(n))
    }

    //def exactly(c: IgnoreS[V], n: Int) = new Ignore[V] {
    //  protected[choice] def scheme = (c scheme) andThen (new AcceptExactly(n))
    //}
  }
}
