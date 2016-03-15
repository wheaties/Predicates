package com.wheaties.predicate

import annotation.{compileTimeOnly, StaticAnnotation}
import scala.reflect.macros.whitebox
import scala.reflect.macros._
import scala.language.experimental.macros

object Macros {

  @compileTimeOnly("GeneratePredicate must be called during compilation")
  class GeneratePredicate extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro GeneratePredicate.impl
  }

  object GeneratePredicate {
    def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._

      println("bazinga")

      def modifiedClass(classDecl: ClassDef) = {
        println(s"bazinga ${classDecl.name}")
        c.Expr(
          q"""
            trait ${classDecl.name}[T1] extends Function1[T1, Boolean]{
              self =>

              def or[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
                def apply(arg1: TT1) = self(arg1) || that(arg1)
              }
              def and[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
                def apply(arg1: TT1) = self(arg1) && that(arg1)
              }
              def xor[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
                def apply(arg1: TT1) = if(self(arg1)) !that(arg1) else that(arg1)
              }
              def nor[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
                def apply(arg1: TT1) = !(self(arg1) || that(arg1))
              }
              def nand[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
                def apply(arg1: TT1) = !(self(arg1) && that(arg1))
              }
              def nxor[TT1 <: T1](that: Function1[TT1, Boolean]) = new Predicate1[TT1]{
                def apply(arg1: TT1) = if(self(arg1)) that(arg1) else !that(arg1)
              }
              def not = new Predicate1[T1]{
                def apply(arg1: T1) = !self(arg1)
              }
              override def toString() = "<predicate1>"

            }

            object ${classDecl.name.toTermName} {
              object Always extends ${classDecl.name}[Any]{
                def apply(arg1: Any) = true
              }

              val always = Always

              object Never extends ${classDecl.name}[Any]{
                def apply(arg1: Any) = false
              }

              val never = Never
            }
           """)
      }

      annottees.map(_.tree) match {
        case (classDecl: ClassDef) :: Nil => modifiedClass(classDecl)
        case _ => c.abort(c.enclosingPosition, "Invalid annottee")
      }
    }

  }
}

