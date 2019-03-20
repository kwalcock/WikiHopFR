package org.ml4ai.agents
import org.ml4ai.mdp._
import org.sarsamora.actions.Action

import scala.collection.mutable

/**
  * Keeps track of certain metrics in a environment as the agent unfolds the MDP.
  * Try to keep the state as lean as possible to incur in minimal overhead of memory.
  * Try to keep the method implementations as simple as possible to incur in minimal time overhead.
  */
class StatsObserver extends AgentObserver {

  // State variables
  val actionDistribution = new mutable.HashMap[Int, Int].withDefaultValue(0)
  var iterations:Option[Int] = None
  var papersRead:Option[Int] = None

  /**
    * Called once before executing an action for the first time
    *
    * @param env
    */
  override def startedEpisode(env: WikiHopEnvironment): Unit = ()

  /**
    * Called it after executing an action and observing the reward
    *
    * @param action
    * @param reward
    * @param env
    */
  override def actionTaken(action: Action, reward: Double, env: WikiHopEnvironment): Unit = {
    import StatsObserver._
    // Increment action counters
    action match {
      case _:Exploration => actionDistribution(EXPLORATION) += 1
      case _:ExplorationDouble => actionDistribution(EXPLORATION_DOUBLE) += 1
      case _:Exploitation => actionDistribution(EXPLOITATION) += 1
      case RandomAction => actionDistribution(RANDOM) += 1
      case _ => ()
    }
  }

  /**
    * Called once after finishing the episode finishes
    *
    * @param env
    */
  override def endedEpisode(env: WikiHopEnvironment): Unit = {
    iterations = Some(env.iterations)
    papersRead = Some(env.consultedPapers.size)
  }
}

object StatsObserver {
  // Some Int constants to keep hashing efficient
  val EXPLORATION = 0
  val EXPLORATION_DOUBLE = 1
  val EXPLOITATION = 2
  val RANDOM = 3
}