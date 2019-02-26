package org.ml4ai.inference

import org.clulab.odin.{Mention, TextBoundMention}
import org.clulab.processors.{Document, Sentence}
import org.ml4ai.ie.OdinEngine
import org.ml4ai.utils.RelationTripleUtils.entityGroundingHash
import org.ml4ai.utils.{AnnotationsLoader, stopLemmas}

class EntityCoocurrenceKnowledgeGraph(documents:Iterable[(String,Document)]) extends KnowledgeGraph(documents) {

  /**
    * Convenience constructor
    * @param documents Md5 hashes for the documents to mine
    * @param loader Implicit AnnotationsLoader instance
    * @return Instance of KnowledgeGraph built from info from the specified documents
    */
  def this(documents:Iterable[String])(implicit loader:AnnotationsLoader) =
    this(documents map (h => (h, loader(h))))


  /**
    * Extracts relation instances from a document object.
    * The endpoints should be expressed as "lemma hashes" and each relation must carry its attributing element
    *
    * @param hash Wikipedia doc md5 hash string
    * @param doc  Processor's doc annotated instance
    * @return An iterable with triples which have the following signature: Source hash, destination has, attribution
    */
  override protected def extractRelationsFromDoc(hash: String, doc: Document): Iterable[(Int, Int, AttributingElement)] = {
    val entities = getOdinEntitiesFromDoc(doc)

    entities.groupBy(_.sentence).flatMap{
      case (sIx, es) =>
        // Compute the entity hashes
        val entityHashes = es map (e => groupedEntityHashes(e.lemmas.get.map(_.toLowerCase).filter(l => !stopLemmas.contains(l)).toSet))
        // Get all the pairs of entity hashes and compute their attribution
        for{
          a <- entityHashes
          b <- entityHashes
          if a != b && a != 0 && b != 0
        } yield (a, b, AttributingElement(None, sIx, hash))
    }

  }

  /**
    * Provides a precomputed map of all the entities' lemma hash and a text representation for all the entities present
    * in the document support set of this graph
    *
    * @return The hashes of the set of post-processed lemmas on the collection
    */
  override protected def buildEntityLemmaHashes: Map[Set[String], Int] = {
    // For each of the documents...
    documents.flatMap{
      case (id, d) =>
        // Extract the entities with the grammar and keep only entity mentions
        getOdinEntitiesFromDoc(d).map{
          entity =>
            val lemmas = entity.lemmas.get.map(_.toLowerCase).filter(!stopLemmas.contains(_))
            lemmas.toSet -> entityGroundingHash(lemmas)
        }
    }.toMap
  }

  protected def getOdinEntitiesFromDoc(d:Document):Iterable[TextBoundMention] = {
    // Odin extractor engine to get the entities
    OdinEngine.extractorEngine.extractFrom(d).collect{
      case tb:TextBoundMention if tb.labels.contains("Entity") => tb
    }
  }
}
