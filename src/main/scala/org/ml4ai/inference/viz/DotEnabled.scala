package org.ml4ai.inference.viz

import org.ml4ai.inference.{KBLabel, KnowledgeGraph}
import org.ml4ai.utils.AnnotationsLoader
import scalax.collection.Graph
import scalax.collection.edge.{LDiEdge, LUnDiEdge}
import scalax.collection.io.dot.{DotAttr, DotEdgeStmt, DotGraph, DotRootGraph, Id, NodeId}
import scalax.collection.io.dot._

import scala.util.{Failure, Success, Try}

/**
  * Dot functionality. Factored out as a trait to un clutter the core implementation of KG
  */
trait DotEnabled {

  this:KnowledgeGraph =>

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
                  val doc = loader.find(attr.document)
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
