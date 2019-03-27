package org.ml4ai.agents

import org.ml4ai.mdp.WikiHopEnvironment
import org.sarsamora.actions.Action

/**
  * Interface to implement an observer pattern for an agent unfolding an FR search
  */
trait AgentObserver {

  /**
    * Called once before executing an action for the first time
    * @param env
    */
  def startedEpisode(env:WikiHopEnvironment):Unit

  /**
    * Called it after executing an action and observing the reward
    * @param action
    * @param reward
    * @param env
    */
  def actionTaken(action:Action, reward:Double, env:WikiHopEnvironment):Unit

  /**
    * Called once after finishing the episode finishes
    * @param env
    */
  def endedEpisode(env:WikiHopEnvironment):Unit

  /**
    * Do something to keep track of the exceptions and throwables for a post-mortem analysis
    * @param throwable
    */
  def registerError(throwable: Throwable): Unit
}
