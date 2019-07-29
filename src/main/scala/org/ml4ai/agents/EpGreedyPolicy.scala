package org.ml4ai.agents
import org.ml4ai.learning.DQN
import org.sarsamora.actions.Action
import org.sarsamora.states.State

class EpGreedyPolicy(decay:Iterator[Double], network:DQN) extends Policy {
  override def selectAction(state: State): Action = ??? // TODO Finish this
}
