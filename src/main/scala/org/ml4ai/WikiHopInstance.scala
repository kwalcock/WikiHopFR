package org.ml4ai

case class WikiHopInstance(id:String,
                           query:String,
                           answer:Option[String],
                           candidates:Seq[String],
                           supportDocs:Seq[String])
