package org.ml4ai.agents
import org.sarsamora.actions.Action
import org.sarsamora.states.State

class EpGreedyPolicy(decay:Iterator[Double], dqn:Any) extends Policy {
  override def selectAction(state: State): Action = ??? // TODO Finish this
}
