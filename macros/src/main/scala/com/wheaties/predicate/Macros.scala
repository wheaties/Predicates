package com.wheaties.predicate

import annotation.{compileTimeOnly, StaticAnnotation, Annotation}
import scala.reflect.macros.whitebox
import scala.reflect.macros._
import scala.language.experimental.macros

object Macros {
//	def hello: Unit = macro impl
//
//  def impl(c: whitebox.Context): c.Expr[Unit] = {
//    import c.universe._
//    c.Expr(q"""println("Hello World")""")
//  }

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
            trait ${classDecl.name} {
              def print() = println("hello annotations")
            }
           """)
      }

      annottees.map(_.tree) match {
        case (classDecl: ClassDef) :: Nil => modifiedClass(classDecl)
        case _ => c.abort(c.enclosingPosition, "Invalid annottee")
      }
    }

  }


  object helloMacro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      import Flag._
      val result = {
        annottees.map(_.tree).toList match {
          case q"object $name extends ..$parents { ..$body }" :: Nil =>
            q"""
            object $name extends ..$parents {
              def hello: ${typeOf[String]} = "hello"
              ..$body
            }
          """
        }
      }
      c.Expr[Any](result)
    }
  }

  @compileTimeOnly("some msg")
  class hello extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro helloMacro.impl
  }
}

