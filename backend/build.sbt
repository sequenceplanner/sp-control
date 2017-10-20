import SPSettings._

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence" % versions.akka,
  "org.iq80.leveldb"            % "leveldb"          % "0.7",
  "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "org.eclipse.milo" % "sdk-client" % "0.1.4",
  "com.codemettle.reactivemq" % "reactivemq_2.11" % "1.0.0",
  "org.apache.activemq" % "activemq-client" % "5.9.1",
  "com.github.tototoshi" %% "scala-csv" % "1.3.5",
  "joda-time" % "joda-time" % "2.9.9"



)
