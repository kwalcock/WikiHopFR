package org.ml4ai.ie

import java.io.FileWriter

import com.typesafe.config.ConfigFactory
import org.ml4ai.utils._
import org.ml4ai.utils.{WikiHopLoader, WikiHopParser}


object EntityEnumeratorApp extends App {
  val extractor = new IEEngine

  private val config = ConfigFactory.load()
  private val annotationsPath = config.getString("files.annotationsFile")

  val loader = new WikiHopLoader(annotationsPath)

  val documents = WikiHopParser.allDocuments

  println("Loading the annotated documents ...")
  val extractions =
    for(s <- documents.par) yield {
      val doc = loader(s)

      // extract mentions from annotated document
      val mentions = extractor.odinEngine.extractFrom(doc)//.sortBy(m => (m.sentence, m.getClass.getSimpleName))

      // display the mentions
      //displayMentions(mentions, d)

      mentions
    }

  val allEntities =
    extractions.flatMap{
      entities =>
        entities map {
          e =>
            (e.lemmas.get.mkString(" ").toLowerCase, e.label)
        }
    }

  private val entityListPath = config.getString("entityListFile")

  println("Writing out file")
  using(new FileWriter(entityListPath)){
    writer =>
      allEntities.seq foreach {
        case (text, label) =>
          writer.write(s"$text\t$label\n")
      }
  }
}
