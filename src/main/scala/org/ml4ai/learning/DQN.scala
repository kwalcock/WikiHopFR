package org.ml4ai.learning

import edu.cmu.dynet.{ComputationGraph, Dim, Expression, FloatVector, ParameterCollection, Tensor}
import org.ml4ai.mdp.{Exploitation, Exploration, ExplorationDouble, WikiHopState}
import org.sarsamora.actions.Action
import org.sarsamora.states.State

class DQN(params:ParameterCollection, embeddingsHelper: EmbeddingsHelper) {

  private val embeddingsDimension = embeddingsHelper.embeddingsDim
  private val featuresDimension = WikiHopState.numFeatures
  private val featureVectorSize = 2*embeddingsDimension + featuresDimension

  private val pW = params.addParameters(Dim(10, featureVectorSize))
  private val pb = params.addParameters(Dim(10))
  private val pX = params.addParameters(Dim(2, 10))
  private val pc = params.addParameters(Dim(2))

  ComputationGraph.renew()

  def apply(input:(WikiHopState, Set[String], Set[String])): Expression = this(Seq(input))

  def apply(input:Iterable[(WikiHopState, Set[String], Set[String])]):Expression = {

    val inputVectors = input.toSeq map {
      case (state, entityA, entityB) =>
        val features = vectorizeState(state)
        val featureVector = Expression.input(Dim(features.size), features)
        val embA = embeddingsHelper.lookup(entityA)
        val embB = embeddingsHelper.lookup(entityB)

        Expression.concatenate(featureVector, aggregateEmbeddings(embA), aggregateEmbeddings(embB))
    }

    val inputMatrix = Expression.concatenateCols(inputVectors:_*)

    val W = Expression.parameter(pW)
    val b = Expression.parameter(pb)
    val X = Expression.parameter(pX)
    val c = Expression.parameter(pc)

    X * Expression.tanh(W*inputMatrix + b) + c
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

object DQN {
  implicit def actionIndex(action:Action):Int = action match {
    case _:Exploration => 0
    case _:ExplorationDouble => 1
    case _:Exploitation => 2
    case _ => throw new IllegalStateException("Unsupported action type for RL")
  }
}