package org.ml4ai.inference

case class Relation(sourceHash:Int,
                    destinationHash:Int,
                    attributions:Iterable[AttributingElement])

case class VerboseRelation(source:Iterable[String],
                           destination:Iterable[String],
                           attributions:Iterable[AttributingElement])
