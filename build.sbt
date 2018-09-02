
import Dependencies._
import scoverage.ScoverageKeys._

name := "ksai"

version := "1.0"

scalaVersion := "2.12.6"

organization in ThisBuild := "ksai"

updateOptions := updateOptions.value.withCachedResolution(true)

concurrentRestrictions in Global += Tags.limit(Tags.Test, 2)


libraryDependencies ++= Seq(
  scalaTest,
  typesafeConfig,
  breeze,
  breezeNative,
  breezeViz,
  akka,
  akkaTest
)

useGpg := true