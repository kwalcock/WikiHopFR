package org.ml4ai.ir

import java.io.File

import com.typesafe.config.ConfigFactory
import org.ml4ai.utils.{WikiHopParser, md5Hash}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.store.NIOFSDirectory
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.document.{Document, Field, StringField, TextField}
import org.apache.lucene.document.StringField
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.TopDocs


object IndexerApp extends App{

  def addToIndex(writer: IndexWriter, value: String):Unit = {
    val hash = md5Hash(value)

    try {
      val doc = new Document
      doc.add(new TextField("contents", value, Field.Store.YES))
      doc.add(new StringField("hash", hash, Field.Store.YES))

      writer.addDocument(doc)
    } catch {
      case exception: Exception =>
        println(s"Problem indexing: $value")
        println(exception.getMessage)
    }
  }


  val config = ConfigFactory.load()
  val indexDir = new File(config.getString("lucene.directoryIndex"))

  if(!indexDir.exists()){
    indexDir.mkdirs()
  }

  val documents = WikiHopParser.allDocuments


  val analyzer = new StandardAnalyzer
  val index = new NIOFSDirectory(indexDir.toPath)

  val ixWConfig = new IndexWriterConfig(analyzer)

  val w = new IndexWriter(index, ixWConfig)

  documents.zipWithIndex foreach {
    case (d, i) =>
      println(s"indexing $i")
      addToIndex(w, d)
  }

  w.commit()
  w.close()

}
