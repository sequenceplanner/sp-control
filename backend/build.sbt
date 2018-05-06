import SPSettings._

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence" % versions.akka,
  "com.typesafe.akka" %% "akka-persistence-query" % versions.akka,
  "com.typesafe.akka" %% "akka-stream-kafka" % "0.19",
  "org.iq80.leveldb"            % "leveldb"          % "0.7",
  "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0"
)

