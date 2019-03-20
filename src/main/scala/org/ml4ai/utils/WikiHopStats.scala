package org.ml4ai.utils

import com.typesafe.config.ConfigFactory
import org.clulab.processors.RelationTriple

import scala.util.{Failure, Success, Try}

object WikiHopStats extends App {

  val config = ConfigFactory.load()
  val annotationsPath = config.getString("files.annotationsFile")

  val trainingInstances = WikiHopParser.testingInstances
  val loader = new AnnotationsLoader(annotationsPath)

  case class Row(instanceId:String, docHash:String, rawRels:Int, procRels:Int)

  def printStats(vals:Iterable[Int]){
    val min = vals.min
    val max = vals.max
    val mean = vals.sum / vals.size.toFloat

    println(s"Min: $min, Max:$max, Mean: $mean")
  }

  val frame =
    for{
      instance <- trainingInstances
      text <- instance.supportDocs
    } yield {
      val doc = loader.find(text)

      val extractions = doc.sentences.flatMap{
        s =>
          s.relations match {
            case Some(rels) => rels
            case None => Array.empty[RelationTriple]
          }
      }

      val numRaw = extractions.length
      val numProcessed = Try(RelationTripleUtils.pruneRelations(extractions).size)

      Row(instance.id, md5Hash(text), numRaw, numProcessed match { case Success(v) => v; case Failure(_) => 0})
    }


  // # of raw relations per document
  val rawRelsPerIns = frame.groupBy{
    t => t.instanceId
  }.mapValues(_.map(_.rawRels).sum)

  print("Number of raw relations per instance ")
  printStats(rawRelsPerIns.values)

  val procRelsPerIns = frame.groupBy{
    t => t.instanceId
  }.mapValues(_.map(_.procRels).sum)

  print("Number of processed relations per instance ")
  printStats(procRelsPerIns.values)

  // # of processed relations per document
  val rawRelsPerDoc = frame.groupBy{
    t => t.docHash
  }.mapValues(_.head.rawRels)

  print("Number of raw relations per document ")
  printStats(rawRelsPerDoc.values)

  val procRelsPerDoc = frame.groupBy{
    t => t.docHash
  }.mapValues(_.head.procRels)

  print("Number of processed relations per document ")
  printStats(procRelsPerDoc.values)

  // # of grouping errors
  val errors = frame.groupBy{
    t => t.docHash
  }.count{
    case (key, vals) =>
      val v = vals.head
      v.rawRels > 0 && v.procRels == 0
  }

  val numDocs = frame.map(_.docHash).toSet.size

  println(s"Number of documents with processing error: $errors out of $numDocs")


}
