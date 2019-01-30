package org.ml4ai

import java.security.MessageDigest

import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.struct.Interval

import scala.annotation.tailrec
import scala.io.Source
import scala.language.reflectiveCalls

package object utils {

  def using[A <: { def close() }, B](resource:A)(func: A => B): B = {
    val returnValue:B = func(resource)
    resource.close()
    returnValue
  }

  def md5Hash(s:String):String = {
    val md5 = MessageDigest.getInstance("md5")
    md5.digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
  }

  lazy val stopWords:Set[String] = using(Source.fromURL(getClass.getResource("/stopwords.txt"))){
    s =>
      s.getLines.toSet
  }

  lazy val stopLemmas:Set[String] = {
    val proc = new ShallowNLPProcessor
    val doc  = proc.mkDocumentFromTokens(stopWords map (Seq(_)))
    proc.annotate(doc)
    doc.sentences.flatMap(_.lemmas.get).toSet
  }

}
