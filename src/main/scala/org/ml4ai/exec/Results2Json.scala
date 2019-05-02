package org.ml4ai.exec

import java.io.PrintWriter

import org.clulab.utils.Serializer
import org.json4s.NoTypeHints
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.ml4ai.exec.PathFinder.{NoPaths, Outcome, Successful, Unsuccessful}
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser, md5Hash, using}

object Results2Json extends App {

  val inPath = WHConfig.PathFinder.outputFile

  val outPath = inPath.split("\\.").dropRight(1).mkString(".") + ".json"

  val data = Serializer.load[Map[String, Outcome]](inPath)

  implicit val formats = Serialization.formats(NoTypeHints)

  private val annotationsPath = WHConfig.Files.annotationsFile

  val loader = new AnnotationsLoader(annotationsPath, cache = false)

  val successfulKeys = data.collect{
    case (k, _:Successful) => k
  }.toSet

  val queries = WikiHopParser.trainingInstances.collect{
    case WikiHopInstance(id, query, answer, _, _) if successfulKeys contains id =>
      id -> (query.split(" ", 2).last, answer.get)
  }.toMap

  println(s"Data size: ${data.size}")

  using(new PrintWriter(outPath)){
    pw =>
      pw.print("[")

      data.toSeq.zipWithIndex.foreach{
        case ((k, v), ix) =>

          if(ix % 100 == 0)
            println(ix)

          if(ix > 0)
            pw.print(",")
          pw.print(write((k,  v match {
            case Successful(paths) => Map("Successful" -> (paths map {
              elems =>
                elems map {
                  e => Map("Source" -> e.source,
                    "Destination" -> e.destination,
                    "Attributions" -> e.attributions.map{
                      a =>
                        val doc = loader(a.document)
                        Map("Doc" -> a.document, "Sentence" -> a.sentenceIx, "SentenceText" -> doc.sentences(a.sentenceIx).getSentenceText)
                    })
                }
            }),
              "Start" -> queries(k)._1,
              "End" -> queries(k)._2
            )
            case NoPaths => "NoPath"
            case Unsuccessful(e) => Map("Error" -> e.toString)
          })))
      }

      pw.print("]")
  }

}
