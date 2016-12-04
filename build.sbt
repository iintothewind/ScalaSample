name := "ScalaSample"

version := "1.0"

scalaVersion := "2.12.0"

lazy val scalaSample = (project in file(".")).
  settings(
    name := "ScalaSample",
    version := "1.0",
    scalaVersion := "2.12.0",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-encoding","UTF-8",
      "-unchecked",
      "-Xlint",
      "-Ywarn-dead-code"
    )
  )

ivyScala := ivyScala.value map {
  _.copy(overrideScalaVersion = true)
}

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
  "org.assertj" % "assertj-core" % "3.3.0" % Test
)
