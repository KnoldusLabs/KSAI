import sbt._

object Dependencies {

  val logbackClassicVer = "1.2.3"
  val configVer = "1.3.1"

  val breeze = "org.scalanlp" %% "breeze" % "0.13.2"

  val breezeNative = "org.scalanlp" %% "breeze-natives" % "0.13.2"

  val breezeViz = "org.scalanlp" %% "breeze-viz" % "0.13.2"

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.4" % "test"

  val typesafeConfig = "com.typesafe" % "config" % configVer

  val akka = "com.typesafe.akka" %% "akka-actor" % "2.5.9"

  val akkaTest = "com.typesafe.akka" %% "akka-testkit" % "2.5.9" % Test


}
