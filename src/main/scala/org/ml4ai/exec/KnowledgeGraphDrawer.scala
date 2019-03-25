package org.ml4ai.exec

import java.io.FileWriter
import java.nio.file.{Files, Paths}

import org.ml4ai.WHConfig
import org.ml4ai.inference.OpenIEKnowledgeGraph
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser, using}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success
import sys.process._

object KnowledgeGraphDrawer extends App {

  val instances = WikiHopParser.trainingInstances
  implicit val loader = new AnnotationsLoader(WHConfig.Files.annotationsFile)
  implicit val ec =  ExecutionContext.global

  // Check whether the output dir exists
  val graphDir = WHConfig.Files.graphvizDir

  val outputDir = Paths.get(graphDir)

  if(!Files.exists(outputDir)){
    Files.createDirectories(outputDir)
  }
  ////////


  val ops =instances.zipWithIndex map {
    case (instance, ix) =>
      Future{
        new OpenIEKnowledgeGraph(instance.supportDocs)
      } map {
        kg =>
          kg.toDot
      } andThen {
        case Success(dot) =>
          val dotPath = outputDir.resolve(Paths.get(s"kg$ix.dot"))
          val pngPath = outputDir.resolve(Paths.get(s"kg$ix.png"))

          using(new FileWriter(dotPath.toFile)) {
            w =>
              w.write(dot)
          }

          s"dot -Tpng $dotPath -o $pngPath" !

          println(s"Processed kg ${ix+1}")
      }
  }

  val all = Future.sequence(ops)

  Await.result(all, Duration.Inf)

//  val source = i.query.split(" ").last
//  val destination = i.answer.get
//
//
//  val p = kg.findPath(source, destination)
//  val dot = kg.toDot
//  println(dot)
}
