package org.ml4ai

import org.ml4ai.utils.{TransitionMemory, any}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class TestTransitionMemory extends FlatSpec with Matchers{

  def buildSampleMemory: TransitionMemory[Int] = {
    val mem = new TransitionMemory[Int](maxSize = 3)
    mem ++= Seq(1,2,3)
    mem
  }

  "The Memory" should "never exceed its maxSize" in {
    val mem = buildSampleMemory
    mem remember Seq(10, 11)
    mem.size should be (3)
  }

  it should "behave FIFO" in {
    val mem = buildSampleMemory
    mem remember Seq(100, 5)

    mem should equal (Seq(3, 100, 5))
  }

  it should "not drop elements if there's enough room to remember more" in {
    val mem = new TransitionMemory[Int](maxSize = 5)
    mem remember Seq(1,2)
    mem remember Seq(45, 678)

    mem should equal (Seq(1,2,45,678))
  }

  it should "return random samples" in {
    /* The reasoning behind this test is the following:
        A random sample should be different from the prefix of the Queue.
        Theoretically, the prefix could be sampled, but getting the same sample multiple times is *very* unlikely.
        As long  as one of the samples is different from the prefix, we can consider this feature as working correctly.
     */
    implicit val rnd: Random = utils.buildRandom()
    val mem = new TransitionMemory[Int](maxSize = 5)
    mem remember (1 to 5)
    val prefix = mem.take(3)
    val subSamples = (0 until 3) map (_ => mem.sample(3))
    val equalities = subSamples map ( _ == prefix)

    any(equalities) should not be true
  }
}
