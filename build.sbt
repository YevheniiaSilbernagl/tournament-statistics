name := "tournament_statitics"

version := "1.0"

lazy val `tournament_statitics` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

routesGenerator := InjectedRoutesGenerator

libraryDependencies ++= Seq(jdbc, cache, ws, specs2 % Test,
  "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.22",
  "net.coobird" % "thumbnailator" % "0.4.8",
  "org.docx4j" % "docx4j" % "6.0.1",
  "org.postgresql" % "postgresql" % "9.4-1200-jdbc41",
  "com.discord4j" % "Discord4J" % "2.10.1",
  "com.googlecode.xmemcached" % "xmemcached" % "2.4.3"
)

unmanagedResourceDirectories in Test <+= baseDirectory(_ / "target/web/public/test")

resolvers ++= Seq(
  "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
  "jcenter" at "http://jcenter.bintray.com"
)