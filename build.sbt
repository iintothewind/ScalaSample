import org.ensime.EnsimeCoursierKeys._
import org.ensime.EnsimeKeys._

name := "ScalaSample"
version := "1.0"
scalaVersion := "2.12.4"

lazy val scalaSample = (project in file(".")).
  settings(
    name := "ScalaSample",
    version := "1.0",
    scalaVersion := "2.12.4",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-encoding", "UTF-8"
//      "-unchecked",
//      "-Xlint",
//      "-Ywarn-dead-code"
    )
  )

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
shellPrompt := { s => Project.extract(s).currentProject.id + "> " }

//ivyScala := ivyScala.value map {
//  _.copy(overrideScalaVersion = true)
//}

ensimeRepositoryUrls in ThisBuild += "http://maven.aliyun.com/nexus/content/groups/public"
ensimeIgnoreScalaMismatch in ThisBuild := true
ensimeJavaFlags in ThisBuild := Seq("-Xss512M", "-Xmx4G", "-XX:MaxMetaspaceSize=768M")
resolvers ++= {
  Seq(Resolver.sonatypeRepo("releases"))
}

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.7",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.7",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6" % Test,
  "com.google.code.findbugs" % "jsr305" % "3.0.1",
  "com.google.guava" % "guava" % "19.0",
  "junit" % "junit" % "4.12" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "org.assertj" % "assertj-core" % "3.3.0" % Test,
  "org.openjdk.jmh" % "jmh-core" % "1.17.4" % Test,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.17.4" % Test,
  "com.microsoft.sqlserver" % "mssql-jdbc" % "6.2.1.jre8"
)
