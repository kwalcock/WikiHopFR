package org.ml4ai

import org.sarsamora.actions.Action

package object mdp {

  def actionBuilder(actionIx:Int, entityA:Set[String], entityB:Set[String]):Action = {
    if(actionIx == 0){
      assert(entityA == entityB, "Exploration must use the same endpoints")
      Exploration(entityA)
    }
    else{
      assert(entityA == entityB, "The action must use the different endpoints")
      actionIx match {
        case 1 =>
          ExplorationDouble(entityA, entityB)
        case 2 =>
          Exploitation(entityA, entityB)
        case ix:Int =>
          throw new NotImplementedError(s"Action with code $ix is not valid")
      }
    }

  }
}
