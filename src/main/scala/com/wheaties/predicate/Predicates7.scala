package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate7[T1, T2, T3, T4, T5, T6, T7] extends Function7[T1, T2, T3, T4, T5, T6, T7, Boolean]{
	self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](that: Function7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, Boolean]) = new Predicate7[TT1, TT2, TT3, TT4, TT5, TT6, TT7]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7) = self(arg1, arg2, arg3, arg4, arg5, arg6, arg7) || that(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	}
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](that: Function7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, Boolean]) = new Predicate7[TT1, TT2, TT3, TT4, TT5, TT6, TT7]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7) = self(arg1, arg2, arg3, arg4, arg5, arg6, arg7) && that(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	}
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](that: Function7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, Boolean]) = new Predicate7[TT1, TT2, TT3, TT4, TT5, TT6, TT7]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7) = if(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7)) !that(arg1, arg2, arg3, arg4, arg5, arg6, arg7) else that(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	}
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](that: Function7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, Boolean]) = new Predicate7[TT1, TT2, TT3, TT4, TT5, TT6, TT7]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7) = !(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7) || that(arg1, arg2, arg3, arg4, arg5, arg6, arg7))
	}
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](that: Function7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, Boolean]) = new Predicate7[TT1, TT2, TT3, TT4, TT5, TT6, TT7]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7) = !(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7) && that(arg1, arg2, arg3, arg4, arg5, arg6, arg7))
	}
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3, TT4 <: T4, TT5 <: T5, TT6 <: T6, TT7 <: T7](that: Function7[TT1, TT2, TT3, TT4, TT5, TT6, TT7, Boolean]) = new Predicate7[TT1, TT2, TT3, TT4, TT5, TT6, TT7]{
		def apply(arg1: TT1, arg2: TT2, arg3: TT3, arg4: TT4, arg5: TT5, arg6: TT6, arg7: TT7) = if(self(arg1, arg2, arg3, arg4, arg5, arg6, arg7)) that(arg1, arg2, arg3, arg4, arg5, arg6, arg7) else !that(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	}
	override def toString() = "<predicate7>"

}
object Predicate7{
	implicit def not[T1, T2, T3, T4, T5, T6, T7] = new Negation[Predicate7[T1, T2, T3, T4, T5, T6, T7]]{
		def not(pred: Predicate7[T1, T2, T3, T4, T5, T6, T7]) = new Predicate7[T1, T2, T3, T4, T5, T6, T7]{
			def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7) = !pred(arg1, arg2, arg3, arg4, arg5, arg6, arg7)
		}
	}
}
object Always7 extends Predicate7[Any,Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any) = true
}
object Never7 extends Predicate7[Any,Any,Any,Any,Any,Any,Any]{
	def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any, arg6: Any, arg7: Any) = false
}