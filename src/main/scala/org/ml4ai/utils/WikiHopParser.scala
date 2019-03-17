package org.ml4ai.utils

import com.typesafe.config.ConfigFactory
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.ml4ai.WikiHopInstance

import scala.io.Source

object WikiHopParser{

  private val config = ConfigFactory.load()

  private val trainingPath = config.getString("files.trainingPath")
  private val testingPath = config.getString("files.testingPath")

  private lazy val jsonTrain = using(Source.fromFile(trainingPath)){
    trainStream =>
      parse(trainStream.bufferedReader())
  }
  private lazy val jsonTest = using(Source.fromFile(testingPath)){
    testStream =>
      parse(testStream.bufferedReader())
  }


  private lazy val documentsTrain = for{ JObject(elem) <- jsonTrain
                          JField("supports", JArray(documents)) <- elem
                          JString(document) <- documents
                       } yield document

  private lazy val documentsTest = for{ JObject(elem) <- jsonTest
                          JField("supports", JArray(documents)) <- elem
                          JString(document) <- documents
                      } yield document


  lazy val allDocuments:Set[String] = Set.empty[String] ++ documentsTest ++ documentsTrain


  lazy val trainingInstances:Seq[WikiHopInstance] = {
    for{
      JObject(elem) <- jsonTrain
      JField("id", JString(id)) <- elem
      JField("query", JString(query)) <- elem
      JField("answer", JString(answer)) <- elem
      JField("candidates", JArray(candidates)) <- elem
      JField("supports", JArray(supportDocs)) <- elem
    } yield WikiHopInstance(id, query, Some(answer),
        candidates map { c => (c: @unchecked) match{ case JString(s) => s }},
        supportDocs map { c => (c: @unchecked) match{ case JString(s) => s }})
  }

  lazy private val trainingMap = trainingInstances.map(i => i.id -> i).toMap
  lazy private val testingMap = testingInstances.map(i => i.id -> i).toMap

  lazy val testingInstances:Seq[WikiHopInstance] = {
    for{
      JObject(elem) <- jsonTest
      JField("id", JString(id)) <- elem
      JField("query", JString(query)) <- elem
      JField("candidates", JArray(candidates)) <- elem
      JField("supports", JArray(supportDocs)) <- elem
    } yield WikiHopInstance(id, query, None,
      candidates map { c => (c: @unchecked) match{ case JString(s) => s }},
      supportDocs map { c => (c: @unchecked) match{ case JString(s) => s }})
  }

  def get(instanceId:String): WikiHopInstance = {
    trainingMap.getOrElse(
      instanceId,
      testingMap(instanceId)
    )
  }

}
