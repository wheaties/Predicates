import sbt._
import Keys._

object Predicates extends Build{
  lazy val root = Project(
    id = "decisions",
    base = file("."),
    settings = baseSettings) aggregate (choices, predicates)

  lazy val predicates = Project(
    id = "predicates",
    base = file("predicates"),
    settings = baseSettings ++ Seq(
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1"
    )
  )

  lazy val choices = Project(
    id = "choices",
    base = file("choices"),
    settings = baseSettings ++ Seq(
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1"
    )
  ) dependsOn predicates

  lazy val baseSettings = Project.defaultSettings ++ Seq(
    version := "0.1",
    scalaVersion := "2.10.3",
    crossScalaVersions := Seq("2.10.1", "2.10.2"),
    scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-language:higherKinds"),
    resolvers ++= Seq(
      "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
      "Sonatype OSS snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS releases"  at "http://oss.sonatype.org/content/repositories/releases")
    )
}