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


  protected lazy val groupedEntityHashes: Map[Set[String], Int] = {
    val allLemmas = entityLemmaHashes.keySet

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
          entityLemmaHashes(representative)
      } ++ Map(Set.empty[String] -> 0)
  }

  protected lazy val reversedGroupEntityHashes: Map[Int, Set[String]] = groupedEntityHashes map { case (k, v) => v -> k }


  protected def matchToEntities(text:String):Iterable[Set[String]] = {
    val lemmas = lemmatize(text) map (_.toLowerCase)
    val buckets = entityLemmaHashes.keys

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


  /**
    * Provides a precomputed map of all the entities' lemma hash and a text representation for all the entities present
    * in the document support set of this graph
    * @return The hashes of the set of post-processed lemmas on the collection
    */
  protected def buildEntityLemmaHashes: Map[Set[String], Int]

  protected lazy val entityLemmaHashes = buildEntityLemmaHashes

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
      val s = graph get entityLemmaHashes(src)
      val d = graph get entityLemmaHashes(dst)
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
          val src = entityLemmaHashes(reversedGroupEntityHashes(r.sourceHash))
          val dst = entityLemmaHashes(reversedGroupEntityHashes(r.destinationHash))

          val label = r.attributions.map{
            attr =>
              Try {
                val doc = loader(attr.document)
                val sen = doc.sentences(attr.sentenceIx)
                attr.triple match {
                  case Some(rel) if rel.relationText(sen) == "" => "*EMPTY*"
                  case Some(rel) => rel.relationText(sen)
                  case None => ""
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
