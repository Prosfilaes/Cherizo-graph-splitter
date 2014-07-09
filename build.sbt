name := "Cherizo Graph Splitting"

version := "0.1"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-optimise", "-Xlint", "-Xfatal-warnings", "-Ywarn-dead-code", "-Ywarn-unused")

libraryDependencies += "org.gnu.glpk" % "glpk-java" % "1.0.36"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.4"
