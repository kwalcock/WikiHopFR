package org.ml4ai.ie

import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.util.PropertiesUtils
import java.util.Properties

import edu.stanford.nlp.ie.util.RelationTriple
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.naturalli.NaturalLogicAnnotations

import collection.JavaConverters._


class OpenIEEngine {
  private val props = PropertiesUtils.asProperties("annotators", "tokenize,ssplit,pos,lemma,depparse,natlog,openie")
  private lazy val pipeline = new StanfordCoreNLP(props)


  def extractTriples(document:String):Iterable[(Double, String, String, String)] = {
    val doc = new Annotation(document)
    pipeline.annotate(doc)


    val sentences = doc.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala
//    val triples = sentences.flatMap(_.get(classOf[NaturalLogicAnnotations.RelationTriplesAnnotation]).asScala).map{
//      t =>
//        (t.confidence, t.subjectGloss, t.relationGloss, t.objectGloss)
//    }

    val triples = for(s <- sentences;t <- s.get(classOf[NaturalLogicAnnotations.RelationTriplesAnnotation]).asScala) yield {
      val lemmas = s.get(classOf[CoreAnnotations.LemmaAnnotation])
      (t.confidence, t.subjectGloss, t.relationGloss, t.objectGloss)
    }

    triples
  }
}


object OpenIEEngine extends App{

  // Create the Stanford CoreNLP pipeline// Create the Stanford CoreNLP pipeline



  // Annotate an example document.// Annotate an example document.

  //var text: String = "Obama was born in Hawaii. He is our president."
  val text = "The 2004 Summer Olympic Games, officially known as the Games of the XXVIII Olympiad and commonly known as Athens 2004, was a premier international multi-sport event held in Athens, Greece, from 13 to 29 August 2004 with the motto \\\"Welcome Home.\\\" 10,625 athletes competed, some 600 more than expected, accompanied by 5,501 team officials from 201 countries. There were 301 medal events in 28 different sports. Athens 2004 marked the first time since the 1996 Summer Olympics that all countries with a National Olympic Committee were in attendance"

  val engine = new OpenIEEngine

  val relations = engine.extractTriples(text)

  // Print the triples
//  for (triple <- relations) {
//    println(triple.confidence + "\t" + triple.subjectLemmaGloss() + "\t" + triple.relationLemmaGloss + "\t" + triple.objectLemmaGloss)
//  }

}
