name := "tournament_statitics"

version := "1.0"

lazy val `tournament_statitics` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

routesGenerator := InjectedRoutesGenerator

libraryDependencies ++= Seq( jdbc , cache , ws   , specs2 % Test,
  "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.22",
  "net.coobird" % "thumbnailator" % "0.4.8",
  "org.docx4j" % "docx4j" % "6.0.1"
)

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"  