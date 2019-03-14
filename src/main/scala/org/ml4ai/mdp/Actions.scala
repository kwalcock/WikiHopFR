package org.ml4ai.mdp

import util.hashing.MurmurHash3
import org.sarsamora.actions.Action

case class Exploration(entityHash:Int) extends Action

case class ExplorationDouble(entityHashA:Int, entityHashB:Int) extends Action {

  // These methods make sure this action is commutable for its arguments
  override def canEqual(that: Any): Boolean = that.isInstanceOf[ExplorationDouble]

  override def equals(obj: Any): Boolean = obj match {
    case that:ExplorationDouble => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode(): Int = MurmurHash3.unorderedHash(Iterable(entityHashA, entityHashB))

}

case class Exploitation(entityHashA:Int, entityHashB:Int) extends Action

case object RandomAction extends Action
