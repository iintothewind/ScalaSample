name := "ScalaSample"

version := "1.0"

scalaVersion := "2.11.8"

ivyScala := ivyScala.value map {
  _.copy(overrideScalaVersion = true)
}

resolvers ++= {
  Seq(Resolver.sonatypeRepo("releases"))
}

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.scalaz" %% "scalaz-core" % "7.2.5",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.5",
  "com.twitter" %% "util-collection" % "6.35.0" exclude("com.google.guava", "guava"),
  "com.google.code.findbugs" % "jsr305" % "3.0.1",
  "com.google.guava" % "guava" % "19.0",
  "junit" % "junit" % "4.12" % Test,
  "org.assertj" % "assertj-core" % "3.3.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3" % Test,
  "org.scala-lang" %% "scala-actors-migration" % "1.1.0" % Test
)