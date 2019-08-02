package org.ml4ai.learning

import edu.cmu.dynet.{ComputationGraph, Dim, Expression, FloatVector, ParameterCollection, Tensor}
import org.ml4ai.mdp.WikiHopState

class DQN(params:ParameterCollection, embeddingsHelper: EmbeddingsHelper) {

  private val embeddingsDimension = embeddingsHelper.embeddingsDim
  private val featureVectorSize = embeddingsDimension + 4

  private val pW = params.addParameters(Dim(5, embeddingsDimension))
  private val pb = params.addParameters(Dim(5))
  private val pX = params.addParameters(Dim(4, 5))
  private val pc = params.addParameters(Dim(4))

  ComputationGraph.renew()

  def apply(input:FloatVector):Expression = {
//    val W = Expression.parameter(pW)
//    val b = Expression.parameter(pb)
//    val X = Expression.parameter(pX)
//    val c = Expression.parameter(pc)
//
//    val input = FloatVector.Seq2FloatVector(states.flatMap{ case (a, b) => Seq(a.toFloat, b.toFloat)})
//    val x = Expression.input(Dim(2, states.size), input)
////    val x = Expression.concatenateToBatch(states map { s => Expression.input(Dim(2), FloatVector.Seq2FloatVector(Seq(s._1.toFloat, s._2.toFloat)))})
//
//    X * Expression.tanh(W*x + b) + c
    ???
  }

  def apply(inputs:Seq[FloatVector]):Expression = {
    // Concatenate all the state vectors into an input matrix
    ???
  }

  // TODO implement this
  def vectorize(state:WikiHopState, entityA:Set[String], entityB:Set[String]):FloatVector = ???
}
