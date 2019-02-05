package org.ml4ai.inference

import org.clulab.processors.{Document, Sentence}
import org.ml4ai.utils.AnnotationsLoader
import org.ml4ai.utils._
import org.ml4ai.utils.RelationTripleUtils.entityGroundingHash
import scalax.collection.Graph
import scalax.collection.edge.LBase.LEdgeImplicits
import scalax.collection.edge.Implicits._ // shortcuts

class KnowledgeGraph(documents:Iterable[(String,Document)]) {

  def this(documents:Iterable[String])(implicit loader:AnnotationsLoader) =
    this(documents map (h => (h, loader(h))))

  private lazy val entityHashes = {
    val allLemmas = entityLemmaBuckets.keySet

    (for{
      current <- allLemmas
      candidate <- allLemmas
      if all(current map candidate.contains)
    } yield {
      (current, candidate)
    }).groupBy(_._1)
      .mapValues{
        elems =>
          val representative = elems.map(_._2).toSeq.maxBy(_.size)
          entityLemmaBuckets(representative)._2
      } ++ Map(Set() -> 0)
  }
  private lazy val entityLemmaBuckets =
    documents.flatMap{
      d =>
        d._2.sentences.flatMap{
          implicit s =>
            s.relations match {
              case Some(rels) =>
                rels flatMap {
                  r =>
                    val subjectLemmas = r.subjectLemmas.map(_.toLowerCase).filter(!stopLemmas.contains(_))
                    val objectLemmas = r.objectLemmas.map(_.toLowerCase).filter(!stopLemmas.contains(_))


                    Seq(
                      subjectLemmas.toSet -> (r.subjectText, entityGroundingHash(subjectLemmas)),
                      objectLemmas.toSet -> (r.objectText, entityGroundingHash(objectLemmas))
                    )
                }
              case None => Seq.empty
            }
        }
    }.toMap

  def matchToEntities(text:String):Iterable[Set[String]] = {
    val lemmas = lemmatize(text) map (_.toLowerCase)
    val buckets = entityLemmaBuckets.keys

    buckets filter {
      bucket =>
        if(bucket.size > lemmas.size)
          all(lemmas map bucket.contains)
        else
          all(bucket map lemmas.contains)
    }
  }

  // Knowledge relation instances
  private lazy val relations:Iterable[Relation] =
    documents.flatMap{
      d =>
        d._2.sentences.zipWithIndex.flatMap{
          case (sen, sIx) =>
            implicit val s:Sentence = sen
            s.relations match {
              case Some(rels) =>
                rels map {
                  r =>
                    val sHash = entityHashes(r.subjectLemmas.map(_.toLowerCase).filter(l => !stopLemmas.contains(l)).toSet)
                    val dHash = entityHashes(r.objectLemmas.map(_.toLowerCase).filter(l => !stopLemmas.contains(l)).toSet)
                    if(sHash != 0 && dHash != 0)
                      Some((sHash, dHash, AttributingElement(r, sIx, d._1)))
                    else
                      None
                } collect {
                  case Some(t) => t
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


  private case class KBLabel(relation:Relation)
  private object MyImplicit extends LEdgeImplicits[KBLabel]; import MyImplicit._

  // Build graph
  private val graph = Graph.from(edges = relations map {
    r =>
      (r.sourceHash ~+> r.destinationHash)(KBLabel(r))
  })

  def findPath(source:String, destination:String):Iterable[Seq[Relation]] = {
    val sourceCandidates = matchToEntities(source)
    if(sourceCandidates.isEmpty)
      throw new NotGroundableElementException(source)

    val destinationCandidates = matchToEntities(destination)
    if(destinationCandidates.isEmpty)
      throw new NotGroundableElementException(destination)

    (for{
      src <- sourceCandidates
      dst <- destinationCandidates
    } yield {
      val s = graph get entityLemmaBuckets(src)._2
      val d = graph get entityLemmaBuckets(dst)._2
      s shortestPathTo d match {
        case Some(path) =>
          println(path.length)
          Some(path.edges.map(_.relation).toSeq)
        case None =>
          println("No path")
          None
      }
    }) collect {
      case Some(path) => path
    }
  }



}
