package org.ml4ai.utils

import com.typesafe.config.ConfigFactory
import org.clulab.processors.bionlp.ner.KBEntry
import org.ml4ai.inference.{KBLabel, KnowledgeGraph}
import scalax.collection.Graph
import scalax.collection.edge.LUnDiEdge

import scala.util.{Success, Try}

object PathFinder extends App {
  val config  = ConfigFactory.load()

  val instances = WikiHopParser.trainingInstances
  implicit val loader = new AnnotationsLoader(config.getString("files.annotationsFile"))

  // For each of the trainig instances
  val paths =
    for(instance <- instances.take(1000)) yield {
    val kg = new KnowledgeGraph(instance.supportDocs)
    val source = instance.query.split(" ").drop(1).mkString(" ")
    // get the underlying graph
    val graph:Graph[Int, LUnDiEdge] = kg.graph

    val rootCandidates = kg.matchToEntities(source)

    (for(root <- rootCandidates) yield Try{
      val nr  = graph get kg.entityLemmaBuckets(root)._2

      nr.withMaxDepth(4).pathUntil{
        n =>
          (nr shortestPathTo n).get.length > 2
      } match {
        case Some(path) =>
          Some(path.edges.map(_.label.asInstanceOf[KBLabel]).toSeq)
        case None =>
          None
      }
    }).collect{
      case Success(Some(p)) =>
        val sources = p map {_.relation.attributions.map(_.document).toSet}
        // Do they come from different sources?
        val docs = sources.flatten

        if (!any(docs map { d => all(sources map { s => s contains d }) })) {
          Some{
            p.map {
              e: KBLabel =>
                val source = kg.reverseEntityHashes(e.relation.sourceHash).mkString(" ")
                val destination = kg.reverseEntityHashes(e.relation.destinationHash).mkString(" ")
                val attributions = e.relation.attributions

                val evidence = attributions map {
                  a =>
                    val doc = loader(a.document)
                    val sen = doc.sentences(a.sentenceIx)
                    (md5Hash(a.document), sen.getSentenceText)
                } toSet

                (source, destination, evidence)
            }
          }
        }else
          None
    }.collect{
      case Some(a) => a
    }

    // make a walk to fetch all the entities that are 3 to 5 connections apart on different documents
  }

  for(ps <- paths; p <- ps){
    p foreach {
      case (s, d, e) =>
        println(s)
        println(d)
        e foreach {
          case (dh, s) =>
            println(s"$dh:\t$s")
        }
        println("===========================")
    }
    println("##########")
    println
  }
}
