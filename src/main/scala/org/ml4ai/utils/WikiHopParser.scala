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


  private val trainStream = Source.fromFile(trainingPath)
  private val testStream = Source.fromFile(testingPath)



  private lazy val jsonTrain = parse(trainStream.bufferedReader())
  private lazy val jsonTest = parse(testStream.bufferedReader())


  private lazy val documentsTrain = for{ JObject(elem) <- jsonTrain
                          JField("supports", JArray(documents)) <- elem
                          JString(document) <- documents
                       } yield document

  private lazy val documentsTest = for{ JObject(elem) <- jsonTest
                          JField("supports", JArray(documents)) <- elem
                          JString(document) <- documents
                      } yield document



  lazy val allDocuments:Set[String] = {
    val s = Set.empty[String] ++ documentsTest ++ documentsTrain

    trainStream.close()
    testStream.close()
    s
  }


  lazy val trainingInstances:List[WikiHopInstance] = {
    for{
      JObject(elem) <- jsonTrain
      JField("id", JString(id)) <- elem
      JField("query", JString(query)) <- elem
      JField("answer", JString(answer)) <- elem
      JField("candidates", JArray(candidates)) <- elem
      JField("supports", JArray(supportDocs)) <- elem
    } yield WikiHopInstance(id, query, Some(answer), candidates map { case JString(s) => s}, supportDocs map { case JString(s) => s})
  }

  lazy val testingInstances:List[WikiHopInstance] = {
    for{
      JObject(elem) <- jsonTest
      JField("id", JString(id)) <- elem
      JField("query", JString(query)) <- elem
      JField("candidates", JArray(candidates)) <- elem
      JField("supports", JArray(supportDocs)) <- elem
    } yield WikiHopInstance(id, query, None, candidates map { case JString(s) => s}, supportDocs map { case JString(s) => s})
  }


}
