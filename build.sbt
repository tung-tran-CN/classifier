name := "classifier"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
    "org.apache.solr" % "solr-core" % "3.6.2",
    "org.apache.solr" % "solr-solrj" % "3.6.2",
    "org.slf4j" % "slf4j-api" % "1.5.11",
    "org.slf4j" % "slf4j-log4j12" % "1.5.11",
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

