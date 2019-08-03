package org.ml4ai.agents.baseline
import org.ml4ai.mdp.{RandomAction, WikiHopEnvironment}
import org.sarsamora.actions.Action

import scala.util.Random

/**
  * Simplest random agent. Behaves randomly
  */
final class RandomActionAgent(implicit rng:Random) extends DeterministicAgent {
  /**
    * Always return the random action
    * @param environment disregarded in this particular implementation
    * @return Action selected from the environment associate to this agent
    */
  override def selectAction(environment: WikiHopEnvironment): Action = RandomAction
}
