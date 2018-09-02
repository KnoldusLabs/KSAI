import com.typesafe.sbt.pgp.PgpKeys.{publishLocalSigned, publishSigned, useGpg}




ThisBuild / organization := "io.github.knolduslabs"
ThisBuild / organizationName := "Knoldus"
ThisBuild / organizationHomepage := Some(url("http://www.knoldus.com/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/KnoldusLabs/KSAI"),
    "scm:git@github.com:KnoldusLabs/KSAI.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "ksai",
    name  = "Pranjut",
    email = "pranjut.gogoi@gmail.com",
    url   = url("http://www.knoldus.com")
  )
)

ThisBuild / description := "KSAI is a machine learning library contains various algorithms such as classification, regression, clustering and many others."
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/KnoldusLabs/KSAI"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

/*
credentials += Credentials(
  "GnuPG Key ID",
  "gpg",
//  "8B40D21E2BF70C9F1E7EF445355915F766044FEA", // key identifier
  "B99CAADDEB422F634C4E719EA9C961AFB244BDBD",
  "ignored" // passwords are supplied by pinentry
)*/

//credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
credentials += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", "Gogoi", "Linkedlist@2")

//skip in publish := true