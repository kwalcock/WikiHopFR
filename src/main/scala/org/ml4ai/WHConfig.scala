package org.ml4ai

import com.typesafe.config.ConfigFactory

object WHConfig {
  private val config = ConfigFactory.load

  object Files {
    private val f = config.getConfig("files")

    val trainingPath: String = f.getString("trainingPath")
    val testingPath: String = f.getString("testingPath")
    val annotationsFile: String = f.getString("annotationsFile")
    val openIEAnnotationsFile: String = f.getString("openIEAnnotationsFile")
    val entityListFile: String = f.getString("entityListFile")
    val graphvizDir: String = f.getString("graphvizDir")
    val benchmarkOutput: String = f.getString("benchmarkOutput")
  }

  object PathFinder {
    private val f = config.getConfig("pathFinder")
    val knowledgeGraphType: String = f.getString("knowledgeGraphType")
    val outputFile: String = f.getString("outputFile")
  }

  object Lucene {
    val directoryIndex: String = config.getString("lucene.directoryIndex")
  }

  object Embeddings {
    private val f = config.getConfig("embeddings")

    val dimensions: Int = f.getInt("dimensions")
    val model: String = f.getString("model")
    val binaryMatrix: Boolean = f.getBoolean("binaryMatrix")
    val binaryPath: String = f.getString("binaryPath")
    val threads: Int = f.getInt("threads")
    val embeddingsFile: String = f.getString("embeddingsFile")
  }
}