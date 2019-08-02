package org.ml4ai

import org.clulab.embeddings.word2vec.Word2Vec
import org.ml4ai.WHConfig.Files
import org.ml4ai.WHConfig.Files._
import org.ml4ai.mdp.WikiHopEnvironment
import org.ml4ai.utils.WikiHopParser

package object learning {

  /**
    * Loads the Glove embeddings for all the words that could be entities in an instance of FR
    * @return
    */
  def loadGloveEntities:Word2Vec = {
    // Instantiate all the environments and pull the entities
    val instances = WikiHopParser.trainingInstances

    val allEntities = instances flatMap {
      instance =>
        val (start, end) = (instance.query.split(" ").drop(1).toSet, instance.answer.get.split(" ").toSet)

        val env = new WikiHopEnvironment(instance.query, instance.answer.get, Some(instance.supportDocs.toSet))

        val entities = env.entityDegrees.keySet.flatten

        start union end union entities
    }

    val e = allEntities.map(w => Word2Vec.sanitizeWord(w)).toSet

    new Word2Vec(glovePath, Some(e))

  }
}
