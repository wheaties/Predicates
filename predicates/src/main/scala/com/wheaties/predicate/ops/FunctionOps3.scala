package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate3
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

trait FunctionOps3{
	implicit class F2P3[T1, T2, T3](f: (T1, T2, T3) => Boolean) extends Predicate3[T1, T2, T3]{
		def apply(arg1: T1, arg2: T2, arg3: T3) = f(arg1, arg2, arg3)
	}

	implicit def conj3[T1, T2, T3] = new Conjunction[(T1, T2, T3) => Boolean]{
		def conjunction(p: (T1, T2, T3) => Boolean, q: (T1, T2, T3) => Boolean) = p and q
	}

	implicit def disj3[T1, T2, T3] = new Disjunction[(T1, T2, T3) => Boolean]{
		def disjunction(p: (T1, T2, T3) => Boolean, q: (T1, T2, T3) => Boolean) = p or q
	}

	implicit def neg3[T1, T2, T3] = new Negation[(T1, T2, T3) => Boolean]{
		def not(p: (T1, T2, T3) => Boolean) = new Predicate3[T1, T2, T3]{
			def apply(arg1: T1, arg2: T2, arg3: T3) = !p(arg1, arg2, arg3)
		}
	}
}