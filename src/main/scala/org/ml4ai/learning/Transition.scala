package org.ml4ai.learning

import org.ml4ai.mdp.WikiHopState
import org.sarsamora.actions.Action
import org.sarsamora.states.State

case class Transition(state:State, action:Action, reward:Float, nextState:State)
