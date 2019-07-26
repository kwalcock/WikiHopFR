package org.ml4ai.learning

import org.ml4ai.mdp.WikiHopState
import org.sarsamora.actions.Action

case class Transition(state:WikiHopState, action:Action, reward:Float, nextState:WikiHopState)
