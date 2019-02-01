package org.ml4ai.inference

import org.clulab.processors.{Document, RelationTriple, Sentence}
import org.ml4ai.utils.AnnotationsLoader
import org.ml4ai.utils.RelationTripleUtils.entityGroundingHash

class KnowledgeGraph(documents:Iterable[(String,Document)]) {

  def this(docHashes:Iterable[String])(implicit loader:AnnotationsLoader) =
    this(docHashes map (h => (h, loader(h))))

  // Entity hashes
  private lazy val entityHashes =
    documents.flatMap{
      d =>
        d._2.sentences.flatMap{
          implicit s =>
            s.relations match {
              case Some(rels) =>
                rels flatMap {
                  r =>
                    Seq(
                      r.subjectText -> entityGroundingHash(r.subjectLemmas),
                      r.objectText -> entityGroundingHash(r.objectLemmas),
                    )
                }
              case None => Seq.empty
            }
        }
    }.toMap

  // Knowledge relation instances
  private lazy val relations =
    documents.flatMap{
      d =>
        d._2.sentences.zipWithIndex.flatMap{
          case (sen, sIx) =>
            implicit val s:Sentence = sen
            s.relations match {
              case Some(rels) =>
                rels withFilter {
                  r => entityHashes(r.subjectText) != 0 && entityHashes(r.objectText) != 0
                } map {
                  r =>
                    val sHash = entityHashes(r.subjectText)
                    val dHash = entityHashes(r.objectText)
                    (sHash, dHash, AttributingElement(r, sIx, d._1))
                }
              case None =>
                Seq.empty
            }
        }
    }.groupBy(t => (t._1, t._2)).map{
      case (k, v) =>
        val sourceHash = k._1
        val destinationHash = k._2
        val attributions = v.map(_._3)
        Relation(sourceHash, destinationHash, attributions)
    }


  // Make hash of the nodes
  //val nodeHashes = elements.flatMap(e => Seq(e.))
  // Build graph

  def findPath(source:String, destination:String):Option[Seq[Relation]] =
    if (entityHashes.getOrElse(source, 0) == 0)
      throw new NotGroundableElementException(source)
    else if (entityHashes.getOrElse(destination, 0) == 0)
      throw new NotGroundableElementException(destination)
    else
      None

}
