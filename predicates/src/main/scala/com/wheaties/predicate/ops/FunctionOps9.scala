package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate9
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

trait FunctionOps9{
	implicit class F2P9[T1, T2, T3, T4, T5, T6, T7, T8, T9](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean) extends Predicate9[T1, T2, T3, T4, T5, T6, T7, T8, T9]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9) = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	}

	implicit def conj9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new Conjunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean) = p and q
	}

	implicit def disj9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new Disjunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean) = p or q
	}

	implicit def neg9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new Negation[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean]{
		def not(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Boolean) = new Predicate9[T1, T2, T3, T4, T5, T6, T7, T8, T9]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9) = !p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
		}
	}
}