package org.ml4ai.agents

import org.ml4ai.WikiHopInstance
import org.ml4ai.inference.VerboseRelation
import org.ml4ai.mdp.WikiHopEnvironment
import org.sarsamora.actions.Action
import org.ml4ai.utils.md5Hash

/**
  * Base class to all of the agent implementations
  */
abstract class BaseAgent {

  /**
    * Selects an action from those available from the environment within runEpoch
    * @return Action selected from the environment associate to this agent
    */
  protected def selectAction(environment: WikiHopEnvironment):Action

  /**
    * Runs the MDP over an environment. Must be overridden by the implementor
    * @return The output paths found by the agent
    */
  def runEpisode(environment: WikiHopEnvironment):Iterable[Seq[VerboseRelation]]


  /**
    * Convenience overload that generates an environment from a training instance
    * @param instance WikiHop instance to create an environment from
    * @return The output paths found by the agent
    */
  def runEpisode(instance:WikiHopInstance):Iterable[Seq[VerboseRelation]] = {
    // Generate the end points
    // TODO: Verify this is correct
    val source: String = instance.query.split(" ").last
    val destination: String = instance.answer match {
      case Some(ans) => ans
      case None =>
        throw new UnsupportedOperationException("For now, only training instances are supported")
    }

    val documentUniverse = Some(instance.supportDocs.map(md5Hash).toSet)

    // Build the environment with the source and destination
    // This is public as the MDP handler needs it
    val environment: WikiHopEnvironment = new WikiHopEnvironment(source, destination, documentUniverse)

    runEpisode(environment)
  }

}
