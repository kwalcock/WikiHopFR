package org.ml4ai.agents

import org.ml4ai.WikiHopInstance
import org.ml4ai.mdp.WikiHopEnvironment
import org.sarsamora.actions.Action

/**
  * Base class to all of the agent implementations
  * @param instance of WikiHop to operate over
  */
abstract class BaseAgent(instance:WikiHopInstance) {

  // Generate the end points
  // TODO: Verify this is correct
  protected val source: String = instance.query.split(" ").last
  protected val destination: String = instance.answer match {
    case Some(ans) => ans
    case None =>
      throw new UnsupportedOperationException("For now, only training instances are supported")
  }

  // TODO: Tweak here if we do FR over all the corpus
  protected val documentUniverse: Set[String] = instance.supportDocs.toSet

  // Build the environment with the source and destination
  // This is public as the MDP handler needs it
  val environment: WikiHopEnvironment = new WikiHopEnvironment(source, destination)

  /**
    * Selects an action from those available from the environment
    * @return Action selected from the environment associate to this agent
    */
  def selectAction:Action

}
