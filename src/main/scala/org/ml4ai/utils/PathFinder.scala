package org.ml4ai.utils

import com.typesafe.config.ConfigFactory
import org.ml4ai.inference.KnowledgeGraph

object PathFinder extends App {
  val config  = ConfigFactory.load()

  val instances = WikiHopParser.trainingInstances
  implicit val loader = new AnnotationsLoader(config.getString("files.annotationsFile"))

  val i = instances.head

  val kg = new KnowledgeGraph(i.supportDocs)

  val source = i.query.split(" ").last
  val destination = i.answer.get


  val p = kg.findPath(source, destination)
  val x = 0
}
