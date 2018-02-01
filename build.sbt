
import Dependencies._
import scoverage.ScoverageKeys._

name := "ksai"

version := "0.1"

scalaVersion := "2.12.4"

organization in ThisBuild := "ksai"

updateOptions := updateOptions.value.withCachedResolution(true)

concurrentRestrictions in Global += Tags.limit(Tags.Test, 2)


libraryDependencies ++= Seq(
  scalaTest,
  typesafeConfig,
  breeze,
  breezeNative,
  breezeViz
)