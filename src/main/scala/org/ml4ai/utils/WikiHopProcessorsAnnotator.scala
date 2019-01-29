package org.ml4ai.utils

import java.io.{File, FileWriter, PrintWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.serialization.DocumentSerializer

import scala.io.Source
import scala.util.{Failure, Try}
import scala.util.matching.Regex.Match

class WikiHopProcessorsAnnotator(config:Config, documents:Iterable[String]){


  private val annotationsPath = config.getString("files.annotationsFile")

  val file = new File(annotationsPath)
  if(!file.exists()){
    file.createNewFile()
  }

  private val annotationsStream = Source.fromFile(file)

  private val r = "^[0-9a-f]{32}$".r

  private val hashes = annotationsStream.getLines.flatMap(r.findAllMatchIn)

  private val alreadyAnnotated = hashes.map{
    case Match(hash)=> hash
  }.toSet

  annotationsStream.close()

  println(documents.size)
  println(alreadyAnnotated.size)

  val processor = new FastNLPProcessor(withRelationExtraction = true)
  val serializer = new DocumentSerializer()

  val writer = new PrintWriter(new FileWriter(file, true))

  val counter = new AtomicInteger(0)
  var progress = ""

  def save(txt:String, pw:PrintWriter):Unit = synchronized{
    pw.print(txt)
  }


  val annotatedDocuments = documents.par.foreach{
    d =>

      Try {
        val hash = md5Hash(d)
        if(!alreadyAnnotated.contains(hash)) {
          val doc = processor.annotate(d)
          val serialized = serializer.save(doc)
          val output = s"$hash\n$serialized###\n"

          save(output, writer)
        }
      } match {
        case Failure(e) =>
          //println(e)
          println(s"Problematic document: $d")
        case _ => ()
      }

      val current = counter.incrementAndGet()
      if(current % 100 == 0) {
        val deleter = Seq.fill(progress.length)('\r').mkString
        print(deleter)
        progress = s"Progress: $current out of ${documents.size}"
        print(progress)
      }
  }

  writer.close()
}

object WikiHopProcessorsAnnotator extends App {
  val config = ConfigFactory.load()
  val sentences = WikiHopParser.allDocuments

  val annotator = new WikiHopProcessorsAnnotator(config, sentences)
}
