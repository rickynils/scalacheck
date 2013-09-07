import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings

import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

name := "scalacheck"

version := "1.11.0-SNAPSHOT"

organization := "org.scalacheck"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("http://www.scalacheck.org"))

scalaVersion := "2.10.2"

crossScalaVersions := Seq("2.9.3", "2.10.2", "2.11.0-M4")

mimaDefaultSettings

previousArtifact := Some("org.scalacheck" % "scalacheck_2.10" % "1.11.0-SNAPSHOT")

libraryDependencies += "org.scala-sbt" %  "test-interface" % "1.0"

libraryDependencies <++= (scalaVersion){sVer =>
  if(sVer startsWith "2.9") Seq.empty
  else Seq("org.scala-lang" % "scala-actors" % sVer)
}

libraryDependencies <++= (scalaVersion){sVer =>
  if((sVer startsWith "2.9") || (sVer startsWith "2.10")) Seq.empty
  else Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0-RC2")
}

javacOptions ++= Seq("-Xmx1024M")

scalacOptions += "-deprecation"

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://www.scalacheck.org</url>
  <licenses>
    <license>
      <name>BSD</name>
      <url>https://github.com/rickynils/scalacheck/blob/master/LICENSE</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/rickynils/scalacheck</url>
    <connection>scm:git:git@github.com:rickynils/scalacheck.git</connection>
  </scm>
  <developers>
    <developer>
      <id>rickynils</id>
      <name>Rickard Nilsson</name>
    </developer>
  </developers>
)
