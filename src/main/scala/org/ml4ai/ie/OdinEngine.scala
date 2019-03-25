package org.ml4ai.ie

import org.clulab.odin._

import scala.io.Source

object OdinEngine {

  // read rules from general-rules.yml file in resources
  private val source = Source.fromURL(getClass.getResource("/grammars/master.yml"))
  private val rules = source.mkString
  source.close()

  // creates an extractor engine using the rules and the default actions
  val extractorEngine = ExtractorEngine(rules)
}
