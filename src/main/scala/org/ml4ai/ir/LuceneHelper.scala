package org.ml4ai.ir

import java.io.File

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.{DirectoryReader, Term}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queryparser.xml.builders.BooleanQueryBuilder
import org.apache.lucene.search._
import org.apache.lucene.store.NIOFSDirectory
import org.ml4ai.mdp.{Exploitation, Exploration, ExplorationDouble, RandomAction}
import org.sarsamora.actions.Action
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute

import collection.JavaConverters._
import scala.collection.mutable

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
  private implicit val searcher = new IndexSearcher(reader)

  // Do the actual IR
  def retrieveDocumentNames(action: Action, instanceToFilter:Option[Set[String]] = None, searcher:IndexSearcher = searcher):Set[String] = {
    // Build the query
    val query = actionToQuery(action)

    // Execute the query
    val maxHits = 100 // TODO: Change this to potentially unbounded
    val topDocs = searcher.search(query, maxHits)

    //Generate the result
    val result =
      (for(hit <- topDocs.scoreDocs) yield {
        val doc = searcher.doc(hit.doc, Set("hash").asJava)
        doc.get("hash")
      }).toSet

    instanceToFilter match {
      case Some(criteria) =>
        result intersect criteria
      case None =>
        result
    }
  }

  /**
    * Creates a lucene query parameterized by the action
    * @param action That defines the desired query
    * @return Lucene query characterized by action
    */
  private def actionToQuery(action: Action):Query = action match {
    case Exploration(a) => entityTermsDisjunction(a)
    case ExplorationDouble(a, b) =>
      val queryA = entityTermsDisjunction(a)
      val queryB = entityTermsDisjunction(b)

      val builder = new BooleanQuery.Builder
      builder.add(new BooleanClause(queryA, BooleanClause.Occur.SHOULD))
      builder.add(new BooleanClause(queryB, BooleanClause.Occur.SHOULD))

      builder.build()
    case Exploitation(a, b) =>
      val queryA = entityTermsDisjunction(a)
      val queryB = entityTermsDisjunction(b)

      val builder = new BooleanQuery.Builder
      builder.add(new BooleanClause(queryA, BooleanClause.Occur.MUST))
      builder.add(new BooleanClause(queryB, BooleanClause.Occur.MUST))

      builder.build()
    // TODO do this!!!
    case RandomAction => throw new UnsupportedOperationException("Still have to implement this!!")
  }


  private def entityTermsDisjunction(entityTerms:Set[String]):BooleanQuery = {

    val analizedTerms = analyzeTerms(entityTerms)
    val termQueries = analizedTerms map (t => new TermQuery(new Term("contents", t)))

    val queryBuilder = new BooleanQuery.Builder

    termQueries foreach {
      term =>
        queryBuilder.add(term, BooleanClause.Occur.MUST)
    }

    queryBuilder.build()
  }


  // Taken from https://lucene.apache.org/core/7_2_1/core/org/apache/lucene/analysis/package-summary.html
  private def analyzeTerms(entityTerms:Set[String]):Set[String] = entityTerms flatMap {
    term =>
      val tokenStream = analyzer.tokenStream("contents", term)
      val termAtt: CharTermAttribute = tokenStream.addAttribute(classOf[CharTermAttribute])
      val tokens = new mutable.HashSet[String]
      tokenStream.reset()
      while(tokenStream.incrementToken()){
        tokens += termAtt.toString
      }
      tokenStream.end()
      tokenStream.close()
      tokens.toSet
  }


}