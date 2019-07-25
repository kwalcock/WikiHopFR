package org.ml4ai.agents

import org.sarsamora.actions.Action
import org.sarsamora.states.State

abstract class Policy  {
  def selectAction(state:State):Action
}
