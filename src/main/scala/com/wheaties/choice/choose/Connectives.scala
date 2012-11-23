package com.wheaties.choice.choose

import com.wheaties.logical.Connective

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