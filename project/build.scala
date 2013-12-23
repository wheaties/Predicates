import sbt._
import Keys._

object Predicates extends Build{
  val root = Project(
    id = "project",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      organization := "wheaties",
      version := "0.1",
      scalaVersion := "2.10.3",
      scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps"),
      resolvers ++= Seq(
        "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
        "Sonatype OSS snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
        "Sonatype OSS releases"  at "http://oss.sonatype.org/content/repositories/releases")
     )
  )
}