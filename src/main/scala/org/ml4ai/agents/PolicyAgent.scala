package org.ml4ai.agents
import org.ml4ai.agents.baseline.DeterministicAgent
import org.ml4ai.inference.VerboseRelation
import org.ml4ai.mdp.{RandomAction, WikiHopEnvironment}
import org.sarsamora.actions.Action

class PolicyAgent(policy:Any) extends DeterministicAgent {
  /**
    * Selects an action from those available from the environment within runEpoch
    *
    * @return Action selected from the environment associate to this agent
    */
  override protected def selectAction(environment: WikiHopEnvironment): Action = {
    RandomAction
  }

}
