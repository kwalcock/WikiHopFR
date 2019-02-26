package org.ml4ai.inference

import org.clulab.processors.{Document, Sentence}
import org.ml4ai.utils.AnnotationsLoader
import org.ml4ai.utils._
import org.ml4ai.utils.RelationTripleUtils.entityGroundingHash
import scalax.collection.Graph
import scalax.collection.edge.LBase.{LEdge, LEdgeImplicits}
import scalax.collection.edge.Implicits._
import scalax.collection.edge.{LDiEdge, LUnDiEdge}
import scalax.collection.io.dot._

import scala.util.{Failure, Success, Try}

case class KBLabel(relation:Relation)

abstract class KnowledgeGraph(documents:Iterable[(String,Document)]) {

  protected lazy val entityLemmaBuckets: Map[Set[String], (String, Int)] =
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

  protected lazy val entityHashes: Map[Set[String], Int] = {
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
      } ++ Map(Set.empty[String] -> 0)
  }

  protected lazy val reversedEntityHashes: Map[Int, Set[String]] = entityHashes map { case (k, v) => v -> k }


  protected def matchToEntities(text:String):Iterable[Set[String]] = {
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

  /**
    * Extracts relation instances from a document object.
    * The endpoints should be expressed as "lemma hashes" and each relation must carry its attributing element
    * @param hash Wikipedia doc md5 hash string
    * @param doc Processor's doc annotated instance
    * @return An iterable with triples which have the following signature: Source hash, destination has, attribution
    */
  protected def extractRelationsFromDoc(hash:String, doc:Document):Iterable[(Int, Int, AttributingElement)]

  // Knowledge relation instances
  protected lazy val relations:Iterable[Relation] =
    documents.flatMap{
      d => extractRelationsFromDoc(d._1, d._2)

    }.groupBy(t => (t._1, t._2)).map{
      case (k, v) =>
        val sourceHash = k._1
        val destinationHash = k._2
        val attributions = v.map(_._3)
        Relation(sourceHash, destinationHash, attributions)
    }

  private object MyImplicit extends LEdgeImplicits[KBLabel]; import MyImplicit._

  // Build graph
  protected val graph: Graph[Int, LUnDiEdge] = Graph.from(edges = relations map {
    r =>
      (r.sourceHash ~+ r.destinationHash)(KBLabel(r))
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

  // Dot format viz code
  private val root = DotRootGraph(directed = true, id = Some(Id("WikiHop Instance")))

  private def edgeTransformer(loader:AnnotationsLoader)(innerEdge: Graph[Int,LUnDiEdge]#EdgeT): Option[(DotGraph,DotEdgeStmt)] =
    innerEdge.edge match {
      case LDiEdge(source, target, label) => label match {
        case KBLabel(r) =>
          val src = entityLemmaBuckets(reversedEntityHashes(r.sourceHash))._1
          val dst = entityLemmaBuckets(reversedEntityHashes(r.destinationHash))._1

          val label = r.attributions.map{
            attr =>
              Try {
                val doc = loader(attr.document)
                val sen = doc.sentences(attr.sentenceIx)
                attr.triple.relationText(sen) match {
                  case "" => "*EMPTY*"
                  case l => l
                }
              }
          }.collect{
            case Success(txt) => txt
            case Failure(_) => "*ATTRIBUTION ERROR*"
          }.toSet.mkString(", ")

          Some((root, DotEdgeStmt(NodeId(src), NodeId(dst), List(DotAttr(Id("label"), Id(label))))))

      }
    }

  def toDot(implicit loader:AnnotationsLoader):String = graph.toDot(dotRoot = root, edgeTransformer=edgeTransformer(loader))
  //////////////////
}
