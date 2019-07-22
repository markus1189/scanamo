scalaVersion in ThisBuild := "2.13.0"
crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.8", scalaVersion.value)

val catsVersion = "2.0.0-M4"
val catsEffectVersion = "2.0.0-M4"
val scalazVersion = "7.2.28" // Bump as needed for io-effect compat
val scalazIOEffectVersion = "2.10.1"
val shimsVersion = "2.0-f7d011a"
val zioVersion = "1.0.0-RC10-1"
val scalacheckVersion = "1.14.0"
val scalatestVersion = "3.0.8"

lazy val stdOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-target:jvm-1.8"
)

lazy val std2xOptions = Seq(
  "-Xfatal-warnings",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-explaintypes",
  "-Yrangepos",
  "-Xfuture",
  "-Xlint:_,-type-parameter-shadow",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

def extraOptions(scalaVersion: String): Seq[String] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) =>
      Seq(
        "-Ymacro-annotations"
      )
    case Some((2, 12)) =>
      Seq(
        "-opt-warnings",
        "-Ywarn-extra-implicit",
        "-Ywarn-unused:_,imports",
        "-Ywarn-unused:imports",
        "-opt:l:inline",
        "-opt-inline-from:<source>"
      ) ++ std2xOptions
    case Some((2, 11)) =>
      Seq(
        "-Xexperimental",
        "-Ywarn-unused-import"
      ) ++ std2xOptions
    case _ => Seq.empty
  }

def macrosParadiseSettings(scalaVersion: String) =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, n)) if n >= 13 => Seq()
    case _                       => Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  }

lazy val commonSettings = Seq(
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
  scalacOptions := stdOptions ++ extraOptions(scalaVersion.value),
  // for simulacrum
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  // sbt-doctest leaves some unused values
  // see https://github.com/scala/bug/issues/10270
  scalacOptions in Test := {
    val mainScalacOptions = scalacOptions.value
    (if (CrossVersion.partialVersion(scalaVersion.value) == Some((2, 12)))
       mainScalacOptions.filter(!Seq("-Ywarn-value-discard", "-Xlint").contains(_)) :+ "-Xlint:-unused,_"
     else
       mainScalacOptions).filter(_ != "-Xfatal-warnings")
  },
  scalacOptions in (Compile, console) := (scalacOptions in Test).value,
  autoAPIMappings := true,
  apiURL := Some(url("http://www.scanamo.org/latest/api/")),
  dynamoDBLocalDownloadDir := file(".dynamodb-local"),
  dynamoDBLocalPort := 8042,
  Test / parallelExecution := false,
  libraryDependencies ++= macrosParadiseSettings(scalaVersion.value)
)

lazy val root = (project in file("."))
  .aggregate(formats, scanamo, testkit, alpakka, refined, scalaz, catsEffect, javaTime, joda, zio)
  .settings(
    commonSettings,
    publishingSettings,
    noPublishSettings,
    startDynamoDBLocal / aggregate := false,
    dynamoDBLocalTestCleanup / aggregate := false,
    stopDynamoDBLocal / aggregate := false
  )

addCommandAlias("tut", "docs/tut")
addCommandAlias("makeMicrosite", "docs/makeMicrosite")
addCommandAlias("publishMicrosite", "docs/publishMicrosite")

val awsDynamoDB = "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.504"

lazy val formats = (project in file("formats"))
  .settings(
    commonSettings,
    publishingSettings,
    name := "scanamo-formats"
  )
  .settings(
    libraryDependencies ++= Seq(
      awsDynamoDB,
      "com.chuusai"          %% "shapeless"  % "2.3.3",
      "com.github.mpilquist" %% "simulacrum" % "0.19.0",
      "org.typelevel"        %% "cats-core"  % catsVersion,
      "org.scalacheck"       %% "scalacheck" % scalacheckVersion % Test,
      "org.scalatest"        %% "scalatest"  % scalatestVersion % Test
    ),
    doctestMarkdownEnabled := true,
    doctestDecodeHtmlEntities := true,
    doctestTestFramework := DoctestTestFramework.ScalaTest
  )
  .dependsOn(testkit % "test->test")

lazy val refined = (project in file("refined"))
  .settings(
    commonSettings,
    publishingSettings,
    name := "scanamo-refined"
  )
  .settings(
    libraryDependencies ++= Seq(
      "eu.timepit"    %% "refined"   % "0.9.8",
      "org.scalatest" %% "scalatest" % scalatestVersion % Test
    )
  )
  .dependsOn(formats)

lazy val scanamo = (project in file("scanamo"))
  .settings(
    commonSettings,
    publishingSettings,
    name := "scanamo"
  )
  .settings(
    libraryDependencies ++= Seq(
      awsDynamoDB,
      "com.chuusai"          %% "shapeless"  % "2.3.3",
      "org.typelevel"        %% "cats-free"  % catsVersion,
      "com.github.mpilquist" %% "simulacrum" % "0.19.0",
      // Use Joda for custom conversion example
      "org.joda"       % "joda-convert" % "2.2.1"  % Provided,
      "joda-time"      % "joda-time"    % "2.10.3" % Test,
      "org.scalatest"  %% "scalatest"   % scalatestVersion  % Test,
      "org.scalacheck" %% "scalacheck"  % scalacheckVersion % Test
    )
  )
  .dependsOn(formats, testkit % "test->test")

lazy val testkit = (project in file("testkit"))
  .settings(
    commonSettings,
    publishingSettings,
    name := "scanamo-testkit",
    libraryDependencies ++= Seq(
      awsDynamoDB,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.1"
    )
  )

lazy val catsEffect = (project in file("cats"))
  .settings(
    name := "scanamo-cats-effect",
    commonSettings,
    publishingSettings,
    libraryDependencies ++= List(
      awsDynamoDB,
      "org.typelevel"  %% "cats-free"   % catsVersion,
      "org.typelevel"  %% "cats-core"   % catsVersion,
      "org.typelevel"  %% "cats-effect" % catsEffectVersion,
      "io.monix"       %% "monix"       % "3.0.0-RC2" % Provided,
      "co.fs2"         %% "fs2-core"    % "1.1.0-M1" % Provided,
      "io.monix"       %% "monix"       % "3.0.0-RC2" % Test,
      "co.fs2"         %% "fs2-core"    % "1.1.0-M1" % Test,
      "org.scalatest"  %% "scalatest"   % scalatestVersion % Test,
      "org.scalacheck" %% "scalacheck"  % scalacheckVersion % Test
    ),
    fork in Test := true,
    scalacOptions in (Compile, doc) += "-no-link-warnings"
  )
  .dependsOn(formats, scanamo, testkit % "test->test")

lazy val scalaz = (project in file("scalaz"))
  .settings(
    name := "scanamo-scalaz",
    commonSettings,
    publishingSettings,
    libraryDependencies ++= List(
      awsDynamoDB,
      "com.codecommit" %% "shims"           % shimsVersion,
      "org.scalaz"     %% "scalaz-core"     % scalazVersion,
      "org.scalaz"     %% "scalaz-ioeffect" % scalazIOEffectVersion,
      "org.scalatest"  %% "scalatest"       % scalatestVersion % Test,
      "org.scalacheck" %% "scalacheck"      % scalacheckVersion % Test
    ),
    fork in Test := true,
    scalacOptions in (Compile, doc) += "-no-link-warnings"
  )
  .dependsOn(formats, scanamo, testkit % "test->test")

lazy val zio = (project in file("scalaz-zio"))
  .settings(
    name := "scanamo-scalaz-zio",
    commonSettings,
    publishingSettings,
    libraryDependencies ++= List(
      awsDynamoDB,
      "org.typelevel"  %% "cats-core"        % catsVersion,
      "org.typelevel"  %% "cats-effect"      % catsEffectVersion,
      "dev.zio"        %% "zio"              % zioVersion,
      "dev.zio"        %% "zio-streams"      % zioVersion % Provided,
      "dev.zio"        %% "zio-interop-cats" % "2.0.0.0-RC1",
      "org.scalatest"  %% "scalatest"        % scalatestVersion % Test,
      "org.scalacheck" %% "scalacheck"       % scalacheckVersion % Test
    ),
    fork in Test := true,
    scalacOptions in (Compile, doc) += "-no-link-warnings"
  )
  .dependsOn(formats, scanamo, testkit % "test->test")

lazy val alpakka = (project in file("alpakka"))
  .settings(
    commonSettings,
    publishingSettings,
    name := "scanamo-alpakka"
  )
  .settings(
    libraryDependencies ++= Seq(
      awsDynamoDB,
      "org.typelevel"      %% "cats-free"                    % catsVersion,
      "com.lightbend.akka" %% "akka-stream-alpakka-dynamodb" % "1.1.0",
      "org.scalatest"      %% "scalatest"                    % scalatestVersion % Test,
      "org.scalacheck"     %% "scalacheck"                   % scalacheckVersion % Test
    ),
    fork in Test := true,
    // unidoc can work out links to other project, but scalac can't
    scalacOptions in (Compile, doc) += "-no-link-warnings"
  )
  .dependsOn(formats, scanamo, testkit % "test->test")

lazy val javaTime = (project in file("java-time"))
  .settings(
    commonSettings,
    publishingSettings,
    name := "scanamo-time"
  )
  .settings(
    libraryDependencies ++= List(
      "org.scalatest"  %% "scalatest"                   % scalatestVersion  % Test,
      "org.scalacheck" %% "scalacheck"                  % scalacheckVersion % Test,
      "com.47deg"      %% "scalacheck-toolbox-datetime" % "0.2.5"  % Test
    )
  )
  .dependsOn(formats)

lazy val joda = (project in file("joda"))
  .settings(
    commonSettings,
    publishingSettings,
    name := "scanamo-joda"
  )
  .settings(
    libraryDependencies ++= List(
      "org.joda"       % "joda-convert"                 % "2.2.1" % Provided,
      "joda-time"      % "joda-time"                    % "2.10.3",
      "org.scalatest"  %% "scalatest"                   % scalatestVersion % Test,
      "org.scalacheck" %% "scalacheck"                  % scalacheckVersion % Test,
      "com.47deg"      %% "scalacheck-toolbox-datetime" % "0.2.5" % Test
    )
  )
  .dependsOn(formats)

lazy val docs = (project in file("docs"))
  .settings(
    commonSettings,
    micrositeSettings,
    noPublishSettings,
    includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.yml",
    ghpagesNoJekyll := false,
    git.remoteRepo := "git@github.com:scanamo/scanamo.git",
    makeMicrosite := makeMicrosite.dependsOn(unidoc in Compile).value,
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
    siteSubdirName in ScalaUnidoc := "latest/api"
  )
  .enablePlugins(MicrositesPlugin, SiteScaladocPlugin, GhpagesPlugin, ScalaUnidocPlugin)
  .dependsOn(scanamo % "compile->test", alpakka % "compile", refined % "compile")

val publishingSettings = Seq(
  organization := "org.scanamo",
  homepage := Some(url("http://www.scanamo.org/")),
  licenses := Seq("Apache V2" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
  publishArtifact in Test := false,
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/scanamo/scanamo"),
      "scm:git:git@github.com:scanamo/scanamo.git"
    )
  ),
  developers := List(
    Developer("philwills", "Phil Wills", "", url("https://github.com/philwills")),
    Developer(
      "regiskuckaertz",
      "Regis Kuckaertz",
      "regis.kuckaertz@theguardian.com",
      url("https://github.com/regiskuckaertz")
    )
  )
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

val micrositeSettings = Seq(
  micrositeName := "Scanamo",
  micrositeDescription := "Scanamo: simpler DynamoDB access for Scala",
  micrositeAuthor := "Scanamo Contributors",
  micrositeGithubOwner := "scanamo",
  micrositeGithubRepo := "scanamo",
  micrositeBaseUrl := "",
  micrositeDocumentationUrl := "/latest/api",
  micrositeHighlightTheme := "color-brewer",
  micrositeGitterChannel := false,
  micrositePalette := Map(
    "brand-primary" -> "#951c55",
    "brand-secondary" -> "#005689",
    "brand-tertiary" -> "#00456e",
    "gray-dark" -> "#453E46",
    "gray" -> "#837F84",
    "gray-light" -> "#E3E2E3",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"
  )
)
