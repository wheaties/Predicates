package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate6
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

trait FunctionOps6{
	implicit class F2P6[T1, T2, T3, T4, T5, T6](f: (T1, T2, T3, T4, T5, T6) => Boolean) extends Predicate6[T1, T2, T3, T4, T5, T6]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6) = f(arg1, arg2, arg3, arg4, arg5, arg6)
	}

	implicit def conj6[T1, T2, T3, T4, T5, T6] = new Conjunction[(T1, T2, T3, T4, T5, T6) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4, T5, T6) => Boolean, q: (T1, T2, T3, T4, T5, T6) => Boolean) = p and q
	}

	implicit def disj6[T1, T2, T3, T4, T5, T6] = new Disjunction[(T1, T2, T3, T4, T5, T6) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4, T5, T6) => Boolean, q: (T1, T2, T3, T4, T5, T6) => Boolean) = p or q
	}

	implicit def neg6[T1, T2, T3, T4, T5, T6] = new Negation[(T1, T2, T3, T4, T5, T6) => Boolean]{
		def not(p: (T1, T2, T3, T4, T5, T6) => Boolean) = new Predicate6[T1, T2, T3, T4, T5, T6]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6) = !p(arg1, arg2, arg3, arg4, arg5, arg6)
		}
	}
}