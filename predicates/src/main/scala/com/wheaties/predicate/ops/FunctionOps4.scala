package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate4
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

object FunctionOps4{
	implicit class F2P4[T1, T2, T3, T4](f: (T1, T2, T3, T4) => Boolean) extends Predicate4[T1, T2, T3, T4]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4) = f(arg1, arg2, arg3, arg4)
	}

	implicit def conj4[T1, T2, T3, T4] = new Conjunction[(T1, T2, T3, T4) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4) => Boolean, q: (T1, T2, T3, T4) => Boolean) = p and q
	}

	implicit def disj4[T1, T2, T3, T4] = new Disjunction[(T1, T2, T3, T4) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4) => Boolean, q: (T1, T2, T3, T4) => Boolean) = p or q
	}

	implicit def neg4[T1, T2, T3, T4] = new Negation[(T1, T2, T3, T4) => Boolean]{
		def not(p: (T1, T2, T3, T4) => Boolean) = new Predicate4[T1, T2, T3, T4]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4) = !p(arg1, arg2, arg3, arg4)
		}
	}
}