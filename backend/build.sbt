import SPSettings._

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-persistence" % versions.akka,
  "com.typesafe.akka" %% "akka-persistence-query" % versions.akka,
  "com.typesafe.akka" %% "akka-stream-kafka" % "0.19",
  "org.iq80.leveldb"            % "leveldb"          % "0.7",
  "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "org.eclipse.milo" % "sdk-client" % "0.1.4",

  // "org.ros.rosjava_core" % "rosjava" % "0.3.5"
  // we cant do the above because rosjava_core includes a different
  // version of apache xmlrpc than the version we already have in sp.
  // instead I put a hacked version of both rosjava_core and
  // apache xmlrpc in lib/
  "org.ros.rosjava_messages" % "std_msgs" % "0.5.11",
  "org.ros.rosjava_messages" % "geometry_msgs" % "1.12.5",
  "org.ros.rosjava_messages" % "rosgraph_msgs" % "1.11.2",
  "org.ros.rosjava_messages" % "nav_msgs" % "1.12.5",

  // "org.ros.rosjava_messages" % "turtlesim" % "0.5.5",


  // these are needed by our own rosjava_core in lib/
  "dnsjava" % "dnsjava" % "2.1.1",
  "commons-logging" % "commons-logging" % "1.2",
  "org.apache.ws.commons.util" % "ws-commons-util" % "1.0.2",
  "org.apache.commons" % "com.springsource.org.apache.commons.codec" % "1.3.0",
  "org.apache.commons" % "com.springsource.org.apache.commons.httpclient" % "3.1.0",
  "org.apache.commons" % "com.springsource.org.apache.commons.io" % "1.4.0",
  "org.apache.commons" % "com.springsource.org.apache.commons.lang" % "2.4.0",
  "org.apache.commons" % "com.springsource.org.apache.commons.logging" % "1.1.1",
  "org.apache.commons" % "com.springsource.org.apache.commons.net" % "2.0.0"
)
