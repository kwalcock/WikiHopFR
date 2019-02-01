package org.ml4ai.inference

case class Relation(sourceHash:Int,
                    destinationHash:Int,
                    attributions:Iterable[AttributingElement])
