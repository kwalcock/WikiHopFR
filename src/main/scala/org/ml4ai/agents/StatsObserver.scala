package org.ml4ai.agents
import org.ml4ai.mdp._
import org.sarsamora.actions.Action

import scala.collection.mutable

/**
  * Keeps track of certain metrics in a environment as the agent unfolds the MDP.
  * Try to keep the state as lean as possible to incur in minimal overhead of memory.
  * Try to keep the method implementations as simple as possible to incur in minimal time overhead.
  */
class StatsObserver extends AgentObserver with Serializable {

  // State variables
  val actionDistribution: mutable.Map[String, Int] = new mutable.HashMap[String, Int].withDefaultValue(0)
  val concreteActionDistribution: mutable.Map[String, Int] = new mutable.HashMap[String, Int].withDefaultValue(0)
  val documentsContribution: mutable.Map[String, Int] = new mutable.HashMap[String, Int].withDefaultValue(0)
  var errors :List[Throwable] = Nil
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
  override def actionTaken(action: Action, reward: Double, numDocsAdded:Int, env: WikiHopEnvironment): Unit = {
    import StatsObserver._
    // Increment action counters
    action match {
      case _:Exploration => actionDistribution(EXPLORATION) += 1
      case _:ExplorationDouble => actionDistribution(EXPLORATION_DOUBLE) += 1
      case _:Exploitation => actionDistribution(EXPLOITATION) += 1
      case _:Cascade => actionDistribution(CASCADE) += 1
      case RandomAction => actionDistribution(RANDOM) += 1
      case _ => ()
    }
  }

  /**
    * Similar to action taken, but returns a concrete action. I.e. Instead of Random Action,
    * returns the sampled action.
    *
    * @param action
    * @param reward
    * @param env
    */
  override def concreteActionTaken(action: Action, reward: Double, numDocsAdded:Int, env: WikiHopEnvironment): Unit = {
    import StatsObserver._
    // Increment action counters
    action match {
      case _:Exploration =>
        concreteActionDistribution(EXPLORATION) += 1
        documentsContribution(EXPLORATION) += numDocsAdded
      case _:ExplorationDouble =>
        concreteActionDistribution(EXPLORATION_DOUBLE) += 1
        documentsContribution(EXPLORATION_DOUBLE) += numDocsAdded
      case _:Exploitation =>
        concreteActionDistribution(EXPLOITATION) += 1
        documentsContribution(EXPLOITATION) += numDocsAdded
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

  /**
    * Do something to keep track of the exceptions and throwables for a post-mortem analysis
    *
    * @param throwable
    */
  override def registerError(throwable: Throwable): Unit = {
    // Just keep a buffer with the elements and let the analysis do whatever it needs with them offline
    errors = throwable::errors
  }

  /**
    * Called before actually taking the action
    *
    * @param action
    * @param env
    */
  override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = Unit
}

object StatsObserver {
  // Some Int constants to keep hashing efficient
  val EXPLORATION = "EXPLORATION"
  val EXPLORATION_DOUBLE = "EXPLORATION_DOUBLE"
  val EXPLOITATION = "EXPLOITATION"
  val RANDOM = "RANDOM"
  val CASCADE = "CASCADE"
}