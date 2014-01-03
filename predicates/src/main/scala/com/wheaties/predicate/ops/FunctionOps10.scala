package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate10
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

object FunctionOps10{
	implicit class F2P10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean) extends Predicate10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10) = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
	}

	implicit def conj10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new Conjunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean) = p and q
	}

	implicit def disj10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new Disjunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean) = p or q
	}

	implicit def neg10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = new Negation[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean]{
		def not(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Boolean) = new Predicate10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10) = !p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
		}
	}
}