package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate8
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

object FunctionOps8{
	implicit class F2P8[T1, T2, T3, T4, T5, T6, T7, T8](f: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean) extends Predicate8[T1, T2, T3, T4, T5, T6, T7, T8]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8) = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	}

	implicit def conj8[T1, T2, T3, T4, T5, T6, T7, T8] = new Conjunction[(T1, T2, T3, T4, T5, T6, T7, T8) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean) = p and q
	}

	implicit def disj8[T1, T2, T3, T4, T5, T6, T7, T8] = new Disjunction[(T1, T2, T3, T4, T5, T6, T7, T8) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean) = p or q
	}

	implicit def neg8[T1, T2, T3, T4, T5, T6, T7, T8] = new Negation[(T1, T2, T3, T4, T5, T6, T7, T8) => Boolean]{
		def not(p: (T1, T2, T3, T4, T5, T6, T7, T8) => Boolean) = new Predicate8[T1, T2, T3, T4, T5, T6, T7, T8]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8) = !p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
		}
	}
}