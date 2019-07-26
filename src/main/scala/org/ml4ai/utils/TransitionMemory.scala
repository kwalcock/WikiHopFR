package org.ml4ai.utils

import scala.collection.mutable
import scala.util.Random

class TransitionMemory[A](val maxSize:Int = 100000) extends mutable.Queue[A]{

  def remember(transitions:Iterable[A]):Unit = {
    val remainingSlots = maxSize - this.size
    if(remainingSlots < transitions.size){
      val requiredSlots = transitions.size - remainingSlots
      for(_ <- 0 until requiredSlots)
        dequeue()
    }
    this ++= transitions
  }

  def sample(size:Int)(implicit rng:Random):Iterable[A] = {
    // This code does sample with replacement, however with a large enough memory it should be very unlikely to happen
    // and shouldn't be a big issue for training
    val indices = (0 until size) map ( _ => rng.nextInt(this.size))
    indices map this
  }
}
