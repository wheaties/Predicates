package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate8[T1, T2, T3, T4, T5, T6, T7, T8] extends Function8[T1, T2, T3, T4, T5, T6, T7, T8, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](that: Function8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, Boolean]) = new Predicate8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8) = self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) || that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	}
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](that: Function8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, Boolean]) = new Predicate8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8) = self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) && that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	}
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](that: Function8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, Boolean]) = new Predicate8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8) = if(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) !that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	}
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](that: Function8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, Boolean]) = new Predicate8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8) = !(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) || that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
	}
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](that: Function8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, Boolean]) = new Predicate8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8) = !(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) && that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
	}
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7, TT8 <: T8](that: Function8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8, Boolean]) = new Predicate8[TT1, TT2, TT3, TT4, TT5, TT6, TT7, TT8]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7, arg8: TT8) = if(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else !that(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	}
	override def toString() = "<predicate8>"

}
object Predicate8{
	implicit def not[T1, T2, T3, T4, T5, T6, T7, T8] = new Negation[Predicate8[T1, T2, T3, T4, T5, T6, T7, T8]]{
		def not(pred: Predicate8[T1, T2, T3, T4, T5, T6, T7, T8]) = new Predicate8[T1, T2, T3, T4, T5, T6, T7, T8]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8) = !pred(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
		}
	}
}
object Always8 extends Predicate8[Any,Any,Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any) = true
}
object Never8 extends Predicate8[Any,Any,Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any, arg8: Any) = false
}