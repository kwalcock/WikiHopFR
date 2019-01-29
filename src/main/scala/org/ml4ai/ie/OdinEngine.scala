package org.ml4ai.ie

import com.typesafe.config.ConfigFactory
import org.clulab.odin._
import org.ml4ai.utils.WikiHopLoader

import scala.io.Source

class OdinEngine {

  // read rules from general-rules.yml file in resources
  private val source = Source.fromURL(getClass.getResource("/grammars/master.yml"))
  private val rules = source.mkString
  source.close()

  // creates an extractor engine using the rules and the default actions
  val odinEngine = ExtractorEngine(rules)
}
