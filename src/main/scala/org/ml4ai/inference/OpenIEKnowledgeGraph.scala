package org.ml4ai.inference

import org.clulab.processors.{Document, Sentence}
import org.ml4ai.utils.{AnnotationsLoader, stopLemmas}

class OpenIEKnowledgeGraph(documents:Iterable[(String,Document)]) extends KnowledgeGraph(documents) {

  /**
    * Convenience constructor
    * @param documents Md5 hashes for the documents to mine
    * @param loader Implicit AnnotationsLoader instance
    * @return Instance of KnowledgeGraph built from info from the specified documents
    */
  def this(documents:Iterable[String])(implicit loader:AnnotationsLoader) =
    this(documents map (h => (h, loader(h))))

  /**
    * Implementation of the relation extraction from OpenIE relation triples
    * @param hash Wikipedia doc md5 hash string
    * @param doc Processor's doc annotated instance
    * @return An iterable with triples which have the following signature: Source hash, destination has, attribution
    */
  override protected def extractRelationsFromDoc(hash: String, doc: Document): Iterable[(Int, Int, AttributingElement)] = {
    doc.sentences.zipWithIndex.flatMap{
      case (sen, sIx) =>
        implicit val s:Sentence = sen
        s.relations match {
          case Some(rels) =>
            rels map {
              r =>
                val sHash = entityHashes(r.subjectLemmas.map(_.toLowerCase).filter(l => !stopLemmas.contains(l)).toSet)
                val dHash = entityHashes(r.objectLemmas.map(_.toLowerCase).filter(l => !stopLemmas.contains(l)).toSet)
                if(sHash != 0 && dHash != 0)
                  Some((sHash, dHash, AttributingElement(r, sIx, hash)))
                else
                  None
            } collect {
              case Some(t) => t
            }
          case None =>
            Seq.empty
        }
    }
  }
}
