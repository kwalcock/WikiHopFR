package org.ml4ai.exec

import java.io.{PrintWriter, StringWriter}

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.clulab.utils.Serializer
import org.ml4ai.inference.{CoocurrenceKnowledgeGraph, NamedEntityLinkKnowledgeGraph, OpenIEKnowledgeGraph, VerboseRelation}
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser}

import scala.util.{Failure, Success, Try}

object PathFinder extends App with LazyLogging{

  abstract class Outcome

  case class Successful(paths: Iterable[Seq[VerboseRelation]]) extends Outcome

  case object NoPaths extends Outcome

  case class Unsuccessful(e: Throwable) extends Outcome

  val config = ConfigFactory.load()

  val instances = WikiHopParser.trainingInstances

  implicit val loader: AnnotationsLoader = new AnnotationsLoader(config.getString("files.annotationsFile"))

  // For each of the training instances
  val results =
    (for (instance <- instances.par) yield {


      //val kg = new CoocurrenceKnowledgeGraph(instance.supportDocs)
      val kg = config.getString("pathFinder.knowledgeGraphType") match {
        case "NamedEntityLink" => new NamedEntityLinkKnowledgeGraph(instance.supportDocs)
        case "Cooccurrence" => new CoocurrenceKnowledgeGraph(instance.supportDocs)
        case "OpenIE" => new OpenIEKnowledgeGraph(instance.supportDocs)
        case unknown => throw new UnsupportedOperationException(s"KnowledgeGraph type not implemented: $unknown")
      }
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
//        case Success(paths) if paths.nonEmpty =>
//          Successful{
//            // Keep unique paths only
//            paths.toSet.map {
//              path:Seq[Relation] =>
//                path map {
//                  relation =>
//                    VerboseRelation(
//                      kg.entityHashToText(relation.sourceHash),
//                      kg.entityHashToText(relation.destinationHash),
//                      relation.attributions
//                    )
//                }
//            }}

        case Success(paths) if paths.nonEmpty => Successful(paths.toSet)
        case Success(_) => NoPaths
        case Failure(exception) => Unsuccessful(exception)
      }

      instance.id -> ret

    }).toMap.seq


  Serializer.save(results, config.getString("pathFinder.outputFile"))

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
    var noPaths = 0
    for (o <- res.values) {
      o match {
        case Successful(_) => successes += 1;
        case NoPaths => noPaths += 1;
        case Unsuccessful(_) => errors += 1
      }
    }
    Map("Successes" -> successes, "NoPaths" -> noPaths, "Errors" -> errors)
  }

  def getErrorCounts(res: Map[String, Outcome]) = {
    res.values.collect{ case Unsuccessful(e) => e }.groupBy(_.getClass).map{ case (k, v) => k.toString -> v.size}
  }


}
