package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate5
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

trait FunctionOps5{
	implicit class F2P5[T1, T2, T3, T4, T5](f: (T1, T2, T3, T4, T5) => Boolean) extends Predicate5[T1, T2, T3, T4, T5]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5) = f(arg1, arg2, arg3, arg4, arg5)
	}

	implicit def conj5[T1, T2, T3, T4, T5] = new Conjunction[(T1, T2, T3, T4, T5) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4, T5) => Boolean, q: (T1, T2, T3, T4, T5) => Boolean) = p and q
	}

	implicit def disj5[T1, T2, T3, T4, T5] = new Disjunction[(T1, T2, T3, T4, T5) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4, T5) => Boolean, q: (T1, T2, T3, T4, T5) => Boolean) = p or q
	}

	implicit def neg5[T1, T2, T3, T4, T5] = new Negation[(T1, T2, T3, T4, T5) => Boolean]{
		def not(p: (T1, T2, T3, T4, T5) => Boolean) = new Predicate5[T1, T2, T3, T4, T5]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5) = !p(arg1, arg2, arg3, arg4, arg5)
		}
	}
}