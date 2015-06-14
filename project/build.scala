import sbt._
import sbt.Keys._
import xerial.sbt.Sonatype._

object Predicates extends Build{
  val predicates = Project(
    id = "predicates",
    base = file("predicates"),
    settings = Project.defaultSettings ++ baseSettings ++ Seq(
      libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"
    )
  )

  val baseSettings = Seq(
    version := "0.1",
    scalaVersion := "2.11.6",
    organization := "com.github.wheaties",
    scalacOptions := Seq("-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:higherKinds", 
      "-language:existentials",
      "-unchecked",
      "-Xfatal-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code"),
    pomExtra := predicatesPom,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    pomIncludeRepository := { x => false },
    publishMavenStyle := true,
    publishArtifact in Test := false
  )

  val predicatesPom =
    <url>http://github.com/wheaties/Predicates</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:wheaties/Predicates.git</url>
      <connection>scm:git:git@github.com:wheaties/Predicates.git</connection>
    </scm>
    <developers>
      <developer>
           <id>wheaties</id>
           <name>Owein Reese</name>
           <url>www.github.com/wheaties</url>
      </developer>
    </developers>
}