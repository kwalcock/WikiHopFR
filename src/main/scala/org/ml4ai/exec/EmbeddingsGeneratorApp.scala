package org.ml4ai.exec

import java.io.{File, PrintWriter}

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.utils.AnnotationsLoader
import sys.process._

/**
  * Generates W2V embeddings using the WikiHop corpus
  */
object EmbeddingsGeneratorApp extends App with LazyLogging{

  // Load settings
  val config = ConfigFactory.load()

  val annotationsPath = config.getString("files.annotationsFile")
  val outputPath = config.getString("embeddings.embeddingsFile")
  val dimensions = config.getInt("embeddings.dimensions")
  val model = config.getString("embeddings.model")
  val w2vPath = config.getString("embeddings.binaryPath")
  val outputInBinaryFormat = config.getBoolean("embeddings.binaryMatrix")
  val numThreads = config.getInt("embeddings.threads")

  // Fetch the annotations
  val annotations = new AnnotationsLoader(annotationsPath)

  logger.info(s"About to generate word embeddings for text in $annotationsPath.")

  logger.info("Loading text ...")
  // Generate the lemmatized sentences
  val lemmatizedSentences =
    annotations.rawDocuments.par flatMap {
      txt =>
        val doc = annotations(txt)

        doc.sentences map {
          s =>
            s.lemmas.get.mkString(" ").toLowerCase
        }
    }

  logger.info(s"Loaded ${lemmatizedSentences.size} sentences.")
  // Store them as a temporary file
  val tempFile = File.createTempFile("lemmatized", ".txt")

  val writer = new PrintWriter(tempFile)
  writer.println(lemmatizedSentences mkString "\n")

  writer.close()

  logger.info("Calling word2vec ...")
  // Now call w2v
  val commandString = s"$w2vPath -train ${tempFile.getAbsolutePath} -output $outputPath -cbow ${if(model == "cbow") 1 else 0} -size $dimensions -window 8 -negative 25 -hs 0 -sample 1e-4 -threads $numThreads -binary ${if(outputInBinaryFormat) 1 else false} -iter 15"
  println(commandString)
  commandString !

  println
  tempFile.delete()
  logger.info(s"Finished training embeddings. Results stored in $outputPath")

}
