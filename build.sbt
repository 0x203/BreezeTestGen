name := "BreezeTestGen"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.mavenLocal  // for asgcommon
resolvers += Resolver.sonatypeRepo("public")  //for scopt

val akkaVersion = "2.4.3"

libraryDependencies ++= Seq(
  // the following can be found here: https://github.com/hpiasg/asgcommon
  "de.uni_potsdam.hpi.asg" % "asgcommon" % "1.0.0-SNAPSHOT",  // Breeze file parsing
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,  // for simulating handshake components
  "org.choco-solver" % "choco-solver" % "3.3.3",  // for constraint solving  (for test generation)
  "com.assembla.scala-incubator" %% "graph-core" % "1.10.1",  // for BrzTests and netlists
  "com.assembla.scala-incubator" %% "graph-constrained" % "1.10.1", // for BrzTests
  "org.scalatest" %% "scalatest" % "2.2.6" % "test", // for testing
  "com.github.scopt" %% "scopt" % "3.4.0",    // for parsing command line options
  "org.apache.logging.log4j" % "log4j-core" % "2.1",  // for logging compatible with asgcommon
  "org.apache.logging.log4j" % "log4j-api" % "2.1"    //for logging
)
