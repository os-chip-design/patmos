
name := "Patmos"

scalaVersion := "2.12.12"

scalacOptions ++= Seq("-Xsource:2.11", "-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls")

libraryDependencies += scalaVersion("org.scala-lang" % "scala-compiler" % _).value

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

//libraryDependencies += "edu.berkeley.cs" %% "chisel3" % chiselVersion
libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.5.3"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.4" % "test"

//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

//addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)

