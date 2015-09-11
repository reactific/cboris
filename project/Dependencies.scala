/**********************************************************************************************************************
  *                                                                                                                    *
  * Copyright (c) 2015, Reactific Software LLC. All Rights Reserved.                                                   *
  *                                                                                                                    *
  * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance     *
  * with the License. You may obtain a copy of the License at                                                          *
  *                                                                                                                    *
  *     http://www.apache.org/licenses/LICENSE-2.0                                                                     *
  *                                                                                                                    *
  * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed   *
  * on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for  *
  * the specific language governing permissions and limitations under the License.                                     *
  **********************************************************************************************************************/

import sbt.MavenRepository
import sbt.ModuleID
import sbt._

/** Build Dependencies
  * This trait can be mixed in to get all of Scrupals repository resolvers and dependent libraries.
  */
trait Dependencies
{
  val jcenter_repo            = "JCenter" at "http://jcenter.bintray.com/"

  val all_resolvers : Seq[MavenRepository] = Seq (
    jcenter_repo
  )

  object Ver {
    val akka = "2.3.13"
    val kamon = "0.4.0"
    val bootswatch = "3.3.1+2"
    val scala = "2.11.7"
  }

  // Akka Stuff
  val akka_actor              = "com.typesafe.akka"         %% "akka-actor"               % Ver.akka
  val akka_slf4j              = "com.typesafe.akka"         %% "akka-slf4j"               % Ver.akka
  val akka_http               = "com.typesafe.akka"         %% "akka-http-experimental"   % "1.0"

  // Fundamental Libraries
  val shapeless               = "com.chuusai"               %% "shapeless"                % "2.2.5"
  val scala_arm               = "com.jsuereth"              %% "scala-arm"                % "1.4"
  val scala_compiler          = "org.scala-lang"            % "scala-compiler"            % "2.11.7"
  val scala_xml               = "org.scala-lang.modules"    % "scala-xml"                 % "1.0.5"

  // TODO: Utilize Kamon Monitoring
  val kamon_core              = "io.kamon"                  %% "kamon-core"                % Ver.kamon
  val kamon_scala             = "io.kamon"                  %% "kamon-scala"               % Ver.kamon
  val kamon_akka              = "io.kamon"                  %% "kamon-akka"                % Ver.kamon
  val kamon_log_reporter      = "io.kamon"                  %% "kamon-log-reporter"        % Ver.kamon
  val kamon_system_metrics    = "io.kamon"                  %% "kamon-system_metrics"      % Ver.kamon
  val kamon_annotation        = "io.kamon"                  %% "kamon-annotation"          % Ver.kamon

  // Logging
  val grizzled_slf4j          = "org.clapper"               %% "grizzled-slf4j"           % "1.0.2"
  val logback_classic         = "ch.qos.logback"            %  "logback-classic"          % "1.1.3"

  val config                  = "com.typesafe"              %  "config"                   % "1.3.0"
  val hsp                     = "com.reactific"             %% "hotspot-profiler"         % "0.1.0-SNAPSHOT"
  // Test Libraries

  object Test {
    val akka_testkit     = "com.typesafe.akka"        %% "akka-testkit"             % Ver.akka        % "test"
    val akka_streams_testkit = "com.typesafe.akka"    %% "akka-stream-testkit-experimental" % "1.0" % "test"
  }

  val common_dependencies : Seq[ModuleID] = Seq(
    grizzled_slf4j, akka_slf4j, logback_classic, scala_compiler
  )

  val root_dependencies : Seq[ModuleID] = Seq(
    akka_actor, akka_http, shapeless,
    kamon_core, kamon_akka,
    Test.akka_streams_testkit, Test.akka_testkit
  ) ++ common_dependencies

}
