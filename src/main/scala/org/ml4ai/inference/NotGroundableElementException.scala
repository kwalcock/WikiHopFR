package org.ml4ai.inference

class NotGroundableElementException(val element:String) extends Exception {
  override def getMessage: String = s"$element can't be matched to any node in the knowledge graph"
}
