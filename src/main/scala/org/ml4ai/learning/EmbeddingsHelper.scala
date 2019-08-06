package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import edu.cmu.dynet.{Dim, Expression, FloatVector, LookupParameter, ParameterCollection, ParameterInit}
import org.clulab.embeddings.word2vec.Word2Vec
import org.ml4ai.WHConfig.Files.glovePath
import org.ml4ai.mdp.WikiHopEnvironment
import org.ml4ai.utils.WikiHopParser

class EmbeddingsHelper(pc:ParameterCollection) extends LazyLogging {

  /**
    * Loads the Glove embeddings for all the words that could be entities in an instance of FR
    * @return
    */
  private def loadGloveEntities:(Word2Vec, Iterable[String]) = {
    // Instantiate all the environments and pull the entities
    val instances = WikiHopParser.trainingInstances

    // Extract all the entities from all instances
    val allEntities = instances flatMap {
      instance =>
        // Include the end points of the search
        val (start, end) = (instance.query.split(" ").drop(1).toSet, instance.answer.get.split(" ").toSet)

        // Instantiate an environment to fetch the entities
        val env = new WikiHopEnvironment(instance.query, instance.answer.get, Some(instance.supportDocs.toSet))

        // Get the entities from the full knowledge graph
        val entities = env.entityDegrees.keySet.flatten

        // Return the union of the endpoints and the KG entities
        start union end union entities
    }

    val sanitizedTerms = allEntities.map(w => Word2Vec.sanitizeWord(w)).toSet

    // Sanitize them and use them as a filter to load the embeddings
    val embeddings = new Word2Vec(glovePath, Some(sanitizedTerms))

    val existingTerms = embeddings.matrix.keySet

    val missingTerms = sanitizedTerms diff existingTerms

    (embeddings, missingTerms)
  }

  // All the pretrained embeddings for the possible terms
  private val (originalEmbeddings, missingTerms) = loadGloveEntities

  private val pretrainedIndex = originalEmbeddings.matrix.keys.zipWithIndex.toMap
  private val missingIndex = missingTerms.zipWithIndex.toMap

  val embeddingsDim:Int = originalEmbeddings.dimensions

  // Make lookup parameters from the embeddings
  private val ev = FloatVector.Seq2FloatVector(originalEmbeddings.matrix.values.flatten.map(_.toFloat).toSeq)
  private val pretrainedEmbeddings = pc.addLookupParameters(originalEmbeddings.matrix.size, Dim(embeddingsDim), init = ParameterInit.fromVector(ev))
  private val missingEmbeddings = pc.addLookupParameters(missingIndex.size, Dim(embeddingsDim))

  def lookup(entity:Set[String]):Iterable[Expression] = entity map {
    term =>
      if(pretrainedIndex contains term) {
        // Fetch from the pretrained embeddings
        Expression.lookup(pretrainedEmbeddings, pretrainedIndex(Word2Vec.sanitizeWord(term)))
      } else {
        // Fetch from the missing embeddings
        Expression.lookup(missingEmbeddings, pretrainedIndex(Word2Vec.sanitizeWord(term)))
      }
  }

  def distance(entityA:Set[String], entityB:Set[String]) = {

  }
}
