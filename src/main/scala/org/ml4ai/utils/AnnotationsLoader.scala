package org.ml4ai.utils

import com.typesafe.config.ConfigFactory
import org.clulab.processors.Document
import org.clulab.serialization.DocumentSerializer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class AnnotationsLoader(path:String, cache:Boolean = false){


  private val serializer = new DocumentSerializer

  private val raw = loadAnnotations()

  private val unserializedAnnotations = new mutable.HashMap[String, Document]

  private def loadAnnotations():Map[String, String] = {

    using(Source.fromFile(path)) {
      src =>

        val buffer = new ListBuffer[String]
        val ret = (for (line <- src.getLines()) yield {
          if (line == "###") {
            val key = buffer.head
            val serialized = buffer.tail.mkString("\n")
            buffer.clear()
            Some(key -> serialized)
          }
          else {
            buffer append line
            None
          }
        }).collect {
          case Some(value) => value
        }.toMap

        ret
    }
  }

  def apply(text: String): Document =
    if(cache) {
      unserializedAnnotations.getOrElseUpdate(text, {
        val hash = md5Hash(text)
        val serialized = raw(hash)
        serializer.load(serialized)
      })
    }
    else serializer.load(raw(md5Hash(text)))

  def contains(text:String): Boolean =
    if(!unserializedAnnotations.contains(text))
      raw.contains(md5Hash(text))
    else
      false

  def rawSentences:Iterable[String] = raw.values

}

object AnnotationsLoader extends App {
  private val config = ConfigFactory.load()
  private val annotationsPath = config.getString("files.annotationsFile")

  val loader = new AnnotationsLoader(annotationsPath)

  val s = "The 2004 Summer Olympic Games, officially known as the Games of the XXVIII Olympiad and commonly known as Athens 2004, was a premier international multi-sport event held in Athens, Greece, from 13 to 29 August 2004 with the motto \"Welcome Home.\" 10,625 athletes competed, some 600 more than expected, accompanied by 5,501 team officials from 201 countries. There were 301 medal events in 28 different sports. Athens 2004 marked the first time since the 1996 Summer Olympics that all countries with a National Olympic Committee were in attendance. 2004 marked the return of the games to the city where they began."

  val d = loader(s)
  val d2 = loader(s)
  val x = 1
}