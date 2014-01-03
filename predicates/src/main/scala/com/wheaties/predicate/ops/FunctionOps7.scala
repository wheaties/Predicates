package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate7
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

object FunctionOps7{
	implicit class F2P7[T1, T2, T3, T4, T5, T6, T7](f: (T1, T2, T3, T4, T5, T6, T7) => Boolean) extends Predicate7[T1, T2, T3, T4, T5, T6, T7]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7) = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	}

	implicit def conj7[T1, T2, T3, T4, T5, T6, T7] = new Conjunction[(T1, T2, T3, T4, T5, T6, T7) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4, T5, T6, T7) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7) => Boolean) = p and q
	}

	implicit def disj7[T1, T2, T3, T4, T5, T6, T7] = new Disjunction[(T1, T2, T3, T4, T5, T6, T7) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4, T5, T6, T7) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7) => Boolean) = p or q
	}

	implicit def neg7[T1, T2, T3, T4, T5, T6, T7] = new Negation[(T1, T2, T3, T4, T5, T6, T7) => Boolean]{
		def not(p: (T1, T2, T3, T4, T5, T6, T7) => Boolean) = new Predicate7[T1, T2, T3, T4, T5, T6, T7]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7) = !p(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
		}
	}
}