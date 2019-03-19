package org.ml4ai.agents.baseline

import org.ml4ai.agents.BaseAgent
import org.ml4ai.inference.VerboseRelation
import org.ml4ai.mdp.WikiHopEnvironment

/**
  * Base class for deterministic agents. Subclasses will chose actions with different criteria
  */
abstract class DeterministicAgent extends BaseAgent{

  /**
    * Tail recursive implementation of run episode
    * @param environment to interact with
    * @return The output paths found by the agent
    */
  override def runEpisode(environment: WikiHopEnvironment): Iterable[Seq[VerboseRelation]] = {
    // Base case in which the episode has finished and the outcome can be observed
    if(environment.finishedEpisode){
      environment.outcome
    }
    else{
      // Select an action
      val action = selectAction(environment)
      // Execute it
      environment.execute(action)
      // Tail recursion
      runEpisode(environment)
    }
  }
}
