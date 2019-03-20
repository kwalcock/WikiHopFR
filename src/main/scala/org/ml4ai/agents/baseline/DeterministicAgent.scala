package org.ml4ai.agents.baseline

import org.ml4ai.agents.{AgentObserver, BaseAgent}
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
  override def runEpisode(environment: WikiHopEnvironment, monitor:Option[AgentObserver]): Iterable[Seq[VerboseRelation]] = {
    // Base case in which the episode has finished and the outcome can be observed
    if(environment.finishedEpisode){
      for(m <- monitor)
        m.endedEpisode(environment)

      environment.outcome
    }
    else{
      // Select an action
      val action = selectAction(environment)
      // Execute it
      val reward = environment.execute(action)
      // Log it to the monitor
      for(m <- monitor)
        m.actionTaken(action, reward, environment)
      // Tail recursion
      runEpisode(environment, monitor)
    }
  }
}
