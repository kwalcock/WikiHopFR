package org.ml4ai.ie

import org.clulab.odin.EventMention
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.ml4ai.utils.AnnotationsLoader

object RuleTesterApp extends App {

  def displayEvent(e:EventMention) = {
    println(s"Text: ${e.text}")
    println(s"Trigger: ${e.trigger.text}")
    println("Arguments:")
    for((label, instances) <- e.arguments; instance <- instances){
      println(s" - $label: ${instance.text}\t${instance.label}")
    }
  }

  val text = "The 2004 Summer Olympic Games, officially known as the Games of the XXVIII Olympiad and commonly known as Athens 2004, was a premier international multi-sport event held in Athens, Greece, from 13 to 29 August 2004 with the motto \\\"Welcome Home.\\\" 10,625 athletes competed, some 600 more than expected, accompanied by 5,501 team officials from 201 countries. There were 301 medal events in 28 different sports. Athens 2004 marked the first time since the 1996 Summer Olympics that all countries with a National Olympic Committee were in attendance. 2004 marked the return of the games to the city where they began."

  val processor = new FastNLPProcessor

  val doc = processor.annotate(text)
  //val odinEngine = new OdinEngine


  val entries = OdinEngine.extractorEngine.extractFrom(doc)

  val events = entries.collect{
    case e:EventMention => e
  }


  val groupedEvents = events.groupBy(_.sentence)

  println
  doc.sentences.indices foreach {
    ix =>
      val (sentence, events) = (doc.sentences(ix), groupedEvents.getOrElse(ix, Seq.empty))
      println(sentence.getSentenceText)
      println("-"*20)
      events foreach displayEvent
      println
  }

//  println
//    .foreach {
//    case (sentence, evs) =>
//      println(sentence.getSentenceText)
//      println("-"*20)
//      events foreach displayEvent
//      println
//  }


  val x = 1
}
