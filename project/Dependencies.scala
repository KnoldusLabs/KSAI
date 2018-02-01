import sbt._

object Dependencies {

  val logbackClassicVer = "1.2.3"
  val akkaVer = "2.4.19"
  val configVer = "1.3.1"

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.5.4"

  val breeze = "org.scalanlp" %% "breeze" % "0.13.2"

  val breezeNative = "org.scalanlp" %% "breeze-natives" % "0.13.2"

  val breezeViz = "org.scalanlp" %% "breeze-viz" % "0.13.2"

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.4" % "test"

  val typesafeConfig = "com.typesafe" % "config" % configVer

}
