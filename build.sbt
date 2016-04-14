name := "BreezeTestGen"
organization := "de.hpi.asg"

version := "1.0"

scalaVersion := "2.11.8"

mainClass := Some("de.hpi.asg.breezetestgen.Main")

resolvers += "jitpack" at "https://jitpack.io"  // for asgcommon
resolvers += Resolver.sonatypeRepo("public")  //for scopt

val akkaVersion = "2.4.3"

libraryDependencies ++= Seq(
  // the following can be found here: https://github.com/hpiasg/asgcommon
  "com.github.hpiasg" % "asgcommon" % "v0.1-alpha",  // Breeze file parsing
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,  // for simulating handshake components
  "org.choco-solver" % "choco-solver" % "3.3.3",  // for constraint solving  (for test generation)
  "com.assembla.scala-incubator" %% "graph-core" % "1.10.1",  // for BrzTests and netlists
  "com.assembla.scala-incubator" %% "graph-constrained" % "1.10.1", // for BrzTests
  "org.scalatest" %% "scalatest" % "2.2.6" % "test", // for testing
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion, // for testing akka actors
  "com.github.scopt" %% "scopt" % "3.4.0",    // for parsing command line options
  "com.typesafe" % "config" % "1.3.0",  // for painless configuration
  "org.apache.logging.log4j" % "log4j-core" % "2.1",  // for logging compatible with asgcommon
  "org.apache.logging.log4j" % "log4j-api" % "2.1"    //for logging
)
