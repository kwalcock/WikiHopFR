package org.ml4ai.learning

import edu.cmu.dynet.{ComputationGraph, Dim, Expression, FloatVector, ParameterCollection, Tensor}
import org.ml4ai.mdp.WikiHopState
import org.sarsamora.states.State

class DQN(params:ParameterCollection, embeddingsHelper: EmbeddingsHelper) {

  private val embeddingsDimension = embeddingsHelper.embeddingsDim
  private val featureVectorSize = 2*embeddingsDimension + 4 // TODO update this

  private val pW = params.addParameters(Dim(5, 2*embeddingsDimension))
  private val pb = params.addParameters(Dim(5))
  private val pX = params.addParameters(Dim(4, 5))
  private val pc = params.addParameters(Dim(4))

  ComputationGraph.renew()

  def apply(input:(WikiHopState, Set[String], Set[String])): Expression = this(Seq(input))

  def apply(input:Iterable[(WikiHopState, Set[String], Set[String])]):Expression = {

    val inputVectors = input map {
      case (state, entityA, entityB) =>
        val features = vectorizeState(state)
        val featureVector = Expression.input(Dim(features.size), features)
        val embA = embeddingsHelper.lookup(entityA)
        val embB = embeddingsHelper.lookup(entityB)

        Expression.concatenate(featureVector, aggregateEmbeddings(embA), aggregateEmbeddings(embB))
    }

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

  /**
    * Converts a state into a DyNET FloatVector
    * @param state instance
    * @return FloatVector to be used as instance
    */
  def vectorizeState(state:WikiHopState):FloatVector = {
    val features = Seq(
      state.iterationNum,
      state.numNodes,
      state.numEdges
    )

    FloatVector.Seq2FloatVector(features map (_.toFloat))
  }

  /**
    * Aggregates entity embeddings by certain criteria to yield a unique entity embedding
    */
  private def aggregateEmbeddings(embs:Iterable[Expression]) = {
    // TODO: Add more nuance here
    Expression.average(embs.toSeq)
  }
}
