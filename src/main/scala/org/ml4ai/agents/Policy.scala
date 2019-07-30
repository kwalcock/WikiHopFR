package org.ml4ai.agents

import org.ml4ai.mdp.WikiHopState
import org.sarsamora.actions.Action
import org.sarsamora.states.State

abstract class Policy  {
  def selectAction(state:WikiHopState, candidateEntities:Seq[Set[String]]):Action
}
