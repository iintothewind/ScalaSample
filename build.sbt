import org.ensime.EnsimeCoursierKeys._
import org.ensime.EnsimeKeys._
import sbt.util

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

//testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")
shellPrompt := { s => Project.extract(s).currentProject.id + "> " }

//ivyScala := ivyScala.value map {
//  _.copy(overrideScalaVersion = true)
//}
scalafmtConfig in ThisBuild := file(s"file://${Path.userHome.getAbsolutePath}/.sbt/1.0/.scalafmt.conf")
ensimeRepositoryUrls in ThisBuild += "http://maven.aliyun.com/nexus/content/groups/public"
ensimeIgnoreScalaMismatch in ThisBuild := true
ensimeJavaFlags in ThisBuild := Seq("-Xss512M", "-Xmx4G", "-XX:MaxMetaspaceSize=768M")
resolvers ++= Seq(
  Resolver.mavenLocal,
  MavenRepository("aliyun", "http://maven.aliyun.com/nexus/content/groups/public")
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.20",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.20",
  "com.google.guava" % "guava" % "24.0-jre",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
  "junit" % "junit" % "4.12" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test,
)
