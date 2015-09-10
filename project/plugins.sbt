import _root_.sbt.Keys._
import _root_.sbt.Resolver
import _root_.sbt._

resolvers ++=  Seq( Resolver.sonatypeRepo("releases"), Resolver.sonatypeRepo("snapshots") )

addSbtPlugin("com.reactific" % "sbt-project" % "0.1.0-SNAPSHOT" )
