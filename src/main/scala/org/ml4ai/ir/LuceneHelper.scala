package org.ml4ai.ir

import java.io.File

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.NIOFSDirectory
import org.ml4ai.mdp.{Exploitation, Exploration, ExplorationDouble, RandomAction}
import org.sarsamora.actions.Action
import collection.JavaConverters._

/**
  * Utilities to interface with the lucene index
  */
object LuceneHelper {

  // State values
  private val config = ConfigFactory.load()
  private val indexDir = new File(config.getString("lucene.directoryIndex"))
  private val analyzer = new StandardAnalyzer
  private val index = new NIOFSDirectory(indexDir.toPath)
  private val reader = DirectoryReader.open(index)
  private val searcher = new IndexSearcher(reader)
  private val queryParser = new QueryParser("contents", analyzer)

  // Do the actual IR
  def retrieveDocumentNames(action: Action, instanceToFilter:Option[String] = None):Set[String] = {
    // Build the query
    val queryString = actionToQuery(action)
    val query = queryParser.parse(queryString)

    // Execute the query
    val maxHits = 100 // TODO: Change this to potentially unbounded
    val topDocs = searcher.search(query, maxHits)

    //Generate the result
    (for(hit <- topDocs.scoreDocs) yield {
      val doc = searcher.doc(hit.doc, Set("hash").asJava)
      doc.get("hash")
    }).toSet
  }

  /**
    * Creates a lucene query parameterized by the action
    * @param action That defines the desired query
    * @return Lucene query characterized by action
    */
  private def actionToQuery(action: Action):String = action match {
    case Exploration(a) => ""
    case ExplorationDouble(a, b) => ""
    case Exploitation(a, b) => ""
    case RandomAction => ""
  }


}
