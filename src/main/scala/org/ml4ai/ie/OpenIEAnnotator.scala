package org.ml4ai.ie

import java.io._
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.config.{Config, ConfigFactory}
import edu.stanford.nlp.ie.util.RelationTriple
import org.json4s.FileInput
import org.ml4ai.utils.{WikiHopParser, md5Hash, using}

import scala.collection.mutable
import scala.util.{Failure, Try}

class OpenIEAnnotator(config:Config, documents:Iterable[String]){


  private val annotationsPath = config.getString("files.openIEAnnotationsFile")

  val file = new File(annotationsPath)

  val writer = new ObjectOutputStream(new FileOutputStream(file))

  val engine = new OpenIEEngine

  val counter = new AtomicInteger(0)
  var progress = ""

  def save(item:(String, Iterable[(Double, String, String, String)]), oos:ObjectOutputStream):Unit = synchronized{
    oos.writeObject(item)
  }


  val annotatedDocuments = documents.take(10).par.foreach{
    d =>

      Try {
        val hash = md5Hash(d)

        val triples = engine.extractTriples(d)

        val output = (hash, triples)

        save(output, writer)

      } match {
        case Failure(e) =>
          println(s"Problematic document: $d\n${e.getMessage}")
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

object OpenIEAnnotator extends App {
  val config = ConfigFactory.load()
  val sentences = WikiHopParser.allDocuments

  val annotator = new OpenIEAnnotator(config, sentences)

  def loadAnnotations(path:String):Map[String, Iterable[(Double, String, String, String)]] = {
    using(new ObjectInputStream(new FileInputStream(path))){
      ois =>
        val ret = new mutable.HashMap[String,Iterable[(Double, String, String, String)]]
        var done = false
        while(!done){
          try{
            val item = ois.readObject().asInstanceOf[(String, Iterable[(Double, String, String, String)])]
            ret += (item._1 -> item._2)
          }
          catch {
            case _:Exception =>
              done = true
          }
        }
        ret.toMap
    }
  }

  //val x = loadAnnotations("openie_annotations.ser")
  //val y = 1
}
