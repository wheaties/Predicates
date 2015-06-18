package com.wheaties.predicate.ops

import com.wheaties.predicate.Predicate13
import com.wheaties.logical.{Negation, Disjunction, Conjunction}

trait FunctionOps13{
	implicit class F2P13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean) extends Predicate13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]{
		def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13) = f(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
	}

	implicit def conj13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new Conjunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean]{
		def conjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean) = p and q
	}

	implicit def disj13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new Disjunction[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean]{
		def disjunction(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean, q: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean) = p or q
	}

	implicit def neg13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = new Negation[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean]{
		def not(p: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => Boolean) = new Predicate13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13) = !p(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
		}
	}
}