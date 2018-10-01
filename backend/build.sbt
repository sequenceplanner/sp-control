import SPSettings._

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence" % versions.akka,
  "com.typesafe.akka" %% "akka-persistence-query" % versions.akka,
  "com.typesafe.akka" %% "akka-stream" % versions.akka,
  "com.typesafe.akka" %% "akka-stream-testkit" % versions.akka % "test",
  "com.typesafe.akka" %% "akka-stream-kafka" % "0.22",
  "org.iq80.leveldb"            % "leveldb"          % "0.7",
  "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "org.eclipse.milo" % "sdk-client" % "0.1.4",


  // ros2
  "log4j" % "log4j" % "1.2.17",
  "org.slf4j" % "slf4j-api" % "1.7.21",
  "org.slf4j" % "slf4j-log4j12" % "1.7.21",
  "org.slf4j" % "slf4j-jdk14" % "1.7.21",
  "org.apache.commons" % "commons-lang3" % "3.7"

)
