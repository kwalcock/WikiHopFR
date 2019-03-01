package org.ml4ai.utils

import java.io.{BufferedOutputStream, PrintWriter, StringWriter}

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.processors.bionlp.ner.KBEntry
import org.clulab.utils.Serializer
import org.ml4ai.inference._
import scalax.collection.Graph
import scalax.collection.edge.LUnDiEdge

import scala.util.{Failure, Success, Try}

object PathFinder extends App with LazyLogging{

  abstract class Outcome

  case class Successful(paths: Iterable[Seq[Relation]]) extends Outcome

  case object NoPaths extends Outcome

  case class Unsuccessful(e: Throwable) extends Outcome

  val config = ConfigFactory.load()

  val instances = WikiHopParser.trainingInstances

  implicit val loader: AnnotationsLoader = new AnnotationsLoader(config.getString("files.annotationsFile"))

  // For each of the trainig instances
  val results =
    (for (instance <- instances.par) yield {


      val kg = new NamedEntityLinkKnowledgeGraph(instance.supportDocs)
      val source = instance.query.split(" ").drop(1).mkString(" ")
      val destination = instance.answer.get

      val result = Try(kg.findPath(source, destination)) recoverWith {
        case tr:java.util.NoSuchElementException =>
          val buf = new StringWriter()
          val writer = new PrintWriter(buf)
          tr.printStackTrace(writer)
          logger.error(buf.toString)
          Failure(tr)
      }

      val ret: Outcome = result match {
        case Success(paths) if paths.nonEmpty => Successful(paths)
        case Success(_) => NoPaths
        case Failure(exception) => Unsuccessful(exception)
      }

      instance.id -> ret

    }).toMap.seq

  Serializer.save(results, "cc_results2.ser")

  val x = results.values.count {
    case Successful(_) => true
    case _ => false
  }

  println(s"Successes: $x")
  println(getOutcomeCounts(results))
  println(getErrorCounts(results))

  def getOutcomeCounts(res: Map[String, Outcome]) = {
    var successes = 0
    var errors = 0
    var nopaths = 0
    for (o <- res.values) {
      o match {
        case Successful(_) => successes += 1;
        case NoPaths => nopaths += 1;
        case Unsuccessful(_) => errors += 1
      }
    }
    Map("Successes" -> successes, "NoPaths" -> nopaths, "Errors" -> errors)
  }

  def getErrorCounts(res: Map[String, Outcome]) = {
    res.values.collect{ case Unsuccessful(e) => e }.groupBy(_.getClass).map{ case (k, v) => k.toString -> v.size}
  }


}
