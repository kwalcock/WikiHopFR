package org.ml4ai.utils

import com.typesafe.config.ConfigFactory
import org.clulab.processors.bionlp.ner.KBEntry
import org.clulab.utils.Serializer
import org.ml4ai.inference._
import scalax.collection.Graph
import scalax.collection.edge.LUnDiEdge

import scala.util.{Failure, Success, Try}

object PathFinder extends App {

  abstract class Outcome
  case class Successful(paths: Iterable[Seq[Relation]]) extends Outcome
  case object NoPaths extends Outcome
  case class Unsuccessful(e:Throwable) extends Outcome

  val config  = ConfigFactory.load()

  val instances = WikiHopParser.trainingInstances

  implicit val loader: AnnotationsLoader = new AnnotationsLoader(config.getString("files.annotationsFile"))

  // For each of the trainig instances
  val results =
    (for(instance <- instances.par) yield {


      val kg = new CoocurrenceKnowledgeGraph(instance.supportDocs)
      val source = instance.query.split(" ").drop(1).mkString(" ")
      val destination = instance.answer.get

      val result = Try(kg.findPath(source, destination))

      val ret:Outcome = result match {
        case Success(paths) if paths.nonEmpty => Successful(paths)
        case Success(_) =>  NoPaths
        case Failure(exception) => Unsuccessful(exception)
      }

      instance.id -> ret

    }).toMap.seq

  Serializer.save(results, "coocurrence_results.ser")

  val x = results.values.count{
    case Successful(_) => true
    case _ => false
  }

  println(s"Successes: $x")


}
