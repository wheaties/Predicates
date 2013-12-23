package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate9[T1, T2, T3, T4, T5, T6, T7, T8, T9] extends Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9](that: Function9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, Boolean]) = new Predicate9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9) = self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) || that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	}
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9](that: Function9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, Boolean]) = new Predicate9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9) = self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) && that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	}
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9](that: Function9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, Boolean]) = new Predicate9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9) = if(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) !that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) else that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	}
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9](that: Function9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, Boolean]) = new Predicate9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9) = !(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) || that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
	}
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9](that: Function9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, Boolean]) = new Predicate9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9) = !(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) && that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
	}
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8, TT9 <: T9](that: Function9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9, Boolean]) = new Predicate9[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, TT9]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8, arg9: TT9) = if(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) else !that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	}
	override def toString() = "<predicate9>"

}
object Predicate9{
	implicit def not[T1, T2, T3, T4, T5, T6, T7, T8, T9] = new Negation[Predicate9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]{
		def not(pred: Predicate9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) = new Predicate9[T1, T2, T3, T4, T5, T6, T7, T8, T9]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9) = !pred(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
		}
	}
}
object Always9 extends Predicate9[Any,Any,Any,Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any, arg9: Any) = true
}
object Never9 extends Predicate9[Any,Any,Any,Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any, arg9: Any) = false
}