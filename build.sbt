name := "Cherizo Graph Splitting"

version := "0.1"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-optimise", "-Xlint", "-Xfatal-warnings", "-Ywarn-dead-code", "-Ywarn-unused")

libraryDependencies += "org.gnu.glpk" % "glpk-java" % "1.0.36"
