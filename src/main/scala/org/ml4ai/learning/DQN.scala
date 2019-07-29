package org.ml4ai.learning

import edu.cmu.dynet.{ComputationGraph, Dim, Expression, FloatVector, ParameterCollection}

class DQN(embeddingsDimension:Int) {

  private val featureVectorSize = embeddingsDimension + 4
  val params = new ParameterCollection()

  val pW = params.addParameters(Dim(5, embeddingsDimension))
  val pb = params.addParameters(Dim(5))
  val pX = params.addParameters(Dim(4, 5))
  val pc = params.addParameters(Dim(4))

  ComputationGraph.renew()

  def apply(states:Seq[(Int, Int)]) = {
    val W = Expression.parameter(pW)
    val b = Expression.parameter(pb)
    val X = Expression.parameter(pX)
    val c = Expression.parameter(pc)

    //    val input = FloatVector.Seq2FloatVector(states.flatMap{ case (a, b) => Seq(a.toFloat, b.toFloat)})
    //    val x = Expression.input(Dim(2, states.size), input)
    val x = Expression.concatenateToBatch(states map { s => Expression.input(Dim(2), FloatVector.Seq2FloatVector(Seq(s._1.toFloat, s._2.toFloat)))})

    X * Expression.tanh(W*x + b) + c
  }
}
