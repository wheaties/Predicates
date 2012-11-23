package com.wheaties.choice

import com.wheaties.logical.{Connective, PredicateLike}

abstract class Choose[-V] extends Choice[V] with PredicateLike[Choose[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

abstract class ChooseL extends ChoiceL with PredicateLike[ChooseL]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

object ChooseAll extends ChooseL{
  protected[choice] def scheme = new AcceptAll
}

class ChooseEvery(n: Int) extends ChooseL{
  protected[choice] def scheme = new AcceptEvery(n)
}

class ChooseFirst(n: Int) extends ChooseL{
  protected[choice] def scheme = new AcceptFirst(n)
}

abstract class ChooseS[-V] extends ChoiceS[V] with PredicateLike[ChooseS[V]]{
  protected[choice] def scheme: IterationScheme

  def get[A](collection: A)(implicit getter: Getter[A]) = getter get (collection, scheme)
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = setter set (collection, value, scheme)
}

class ChooseIf[-V](pred: V => Boolean) extends ChooseS[V] with PredicateLike[ChooseS[V]]{
  protected[choice] def scheme = new AcceptIf[V](pred)
}

object Choose{
  implicit def conCL[V, C1 <: Choose[V], C2 <: ChooseL] = new ChooseConCL[V]
  implicit def conCS[V1, V2 >: V1, C1 <: Choose[V1], C2 <: ChooseS[V2]] = new ChooseConCS[V1,V2]
  implicit def conCC[V1, V2 >: V1, C1 <: Choose[V1], C2 <: Choose[V2]] = new ChooseConCC[V1,V2]
}

object ChooseS{
  implicit def conSS[V1, V2 >: V1, C1 <: ChooseS[V1], C2 <: ChooseS[V2]] = new ChooseConSS[V1,V2]
  implicit def conSL[V, C1<: ChooseS[V], C2<: ChooseL] = new ChooseConLS[V]
  implicit def conSC[V1, V2 >: V1, C1 <: ChooseS[V1], C2 <: Choose[V2]] = new ChooseConSC[V1,V2]
}

object ChooseL{
  implicit def conLL[C1 <: ChooseL, C2 <: ChooseL] = ChooseConLL
  implicit def conLS[V, C1<: ChooseL, C2<: ChooseS[V]] = new ChooseConLS[V]
  implicit def conLC[V, C1 <: ChooseL, C2 <: Choose[V]] = new ChooseConLC[V]
}

object ChooseConLL extends Connective[ChooseL,ChooseL,ChooseL]{
  def and(p: ChooseL, q: ChooseL) = new ChooseL {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: ChooseL, q: ChooseL) = new ChooseL {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: ChooseL, q: ChooseL) = new ChooseL {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: ChooseL, q: ChooseL) = new ChooseL {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: ChooseL, q: ChooseL) = new ChooseL {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: ChooseL, q: ChooseL) = new ChooseL {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConSS[V1, V2 >: V1] extends Connective[ChooseS[V1],ChooseS[V2],ChooseS[V1]]{
  def and(p: ChooseS[V1], q: ChooseS[V2]) = new ChooseS[V1] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: ChooseS[V1], q: ChooseS[V2]) = new ChooseS[V1] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: ChooseS[V1], q: ChooseS[V2]) = new ChooseS[V1] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: ChooseS[V1], q: ChooseS[V2]) = new ChooseS[V1] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: ChooseS[V1], q: ChooseS[V2]) = new ChooseS[V1] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: ChooseS[V1], q: ChooseS[V2]) = new ChooseS[V1] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConLS[V] extends Connective[ChooseL,ChooseS[V],Choose[V]]{
  def and(p: ChooseL, q: ChooseS[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: ChooseL, q: ChooseS[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: ChooseL, q: ChooseS[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: ChooseL, q: ChooseS[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: ChooseL, q: ChooseS[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: ChooseL, q: ChooseS[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConSL[V] extends Connective[ChooseS[V],ChooseL,Choose[V]]{
  def and(p: ChooseS[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: ChooseS[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: ChooseS[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: ChooseS[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: ChooseS[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: ChooseS[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConCL[V] extends Connective[Choose[V],ChooseL,Choose[V]]{
  def and(p: Choose[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: Choose[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: Choose[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: Choose[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: Choose[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: Choose[V], q: ChooseL) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConLC[V] extends Connective[ChooseL,Choose[V],Choose[V]]{
  def and(p: ChooseL, q: Choose[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: ChooseL, q: Choose[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: ChooseL, q: Choose[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: ChooseL, q: Choose[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: ChooseL, q: Choose[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: ChooseL, q: Choose[V]) = new Choose[V] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConCS[V1,V2 >: V1] extends Connective[Choose[V1],ChooseS[V2],Choose[V1]]{
  def and(p: Choose[V1], q: ChooseS[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: Choose[V1], q: ChooseS[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: Choose[V1], q: ChooseS[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: Choose[V1], q: ChooseS[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: Choose[V1], q: ChooseS[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: Choose[V1], q: ChooseS[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConSC[V1, V2 >: V1] extends Connective[ChooseS[V1],Choose[V2],Choose[V1]]{
  def and(p: ChooseS[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: ChooseS[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: ChooseS[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: ChooseS[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: ChooseS[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: ChooseS[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

class ChooseConCC[V1, V2 >: V1] extends Connective[Choose[V1],Choose[V2],Choose[V1]]{
  def and(p: Choose[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) and (q scheme)
  }

  def or(p: Choose[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) or (q scheme)
  }

  def xor(p: Choose[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) xor (q scheme)
  }

  def nand(p: Choose[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nand (q scheme)
  }

  def nor(p: Choose[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nor (q scheme)
  }

  def nxor(p: Choose[V1], q: Choose[V2]) = new Choose[V1] {
     protected[choice] def scheme = (p scheme) nxor (q scheme)
  }
}

//TODO: make implicit
class ChooseSatisfy extends Conditional[ChooseL,Choose[_]]{
  def condition[B](c: ChooseL, pred: B => Boolean) = new Choose[B]{
    protected[choice] def scheme = (c scheme) compose (new AcceptIf[B](pred))
  }
}

//TODO: make implicit
abstract class ChooseLimit[V] extends Limit[V,ChooseS[V],Choose[V]]{
  def every(c: ChooseS[V], n: Int) = new Choose[V]{
    protected[choice] def scheme = (c scheme) andThen (new AcceptEvery(n))
  }

  def all(c: ChooseS[V]) = new Choose[V] {
    protected[choice] def scheme = c scheme
  }

  def first(c: ChooseS[V], n: Int) = new Choose[V] {
    protected[choice] def scheme = (c scheme) andThen (new AcceptFirst(n))
  }

  def exactly(c: ChooseS[V], n: Int) = new Choose[V] {
    protected[choice] def scheme = (c scheme) andThen (new AcceptExactly(n))
  }
}