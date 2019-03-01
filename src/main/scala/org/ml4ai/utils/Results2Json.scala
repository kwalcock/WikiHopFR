package org.ml4ai.utils

import java.io.PrintWriter

import org.clulab.utils.Serializer
import org.ml4ai.utils.PathFinder.{NoPaths, Outcome, Successful, Unsuccessful}
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write

object Results2Json extends App {

  val inPath = args(0)
  val outPath = inPath.split("\\.").dropRight(1).mkString(".") + ".json"

  val data = Serializer.load[Map[String, Outcome]](inPath)

  implicit val formats = Serialization.formats(NoTypeHints)

  using(new PrintWriter(outPath)){
    pw =>
      pw.print(
        write(
          data.mapValues{
            case Successful(paths) => "Successful"
            case NoPaths => "NoPath"
            case Unsuccessful(e) => e.toString
          }
        )
      )
  }

}
