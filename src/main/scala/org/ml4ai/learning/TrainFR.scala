package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WikiHopInstance
import org.ml4ai.agents.{AgentObserver, PolicyAgent}
import org.ml4ai.mdp.{WikiHopEnvironment, WikiHopState}
import org.ml4ai.utils.{TransitionMemory, WikiHopParser}
import org.sarsamora.actions.Action
import org.sarsamora.states.State

object TrainFR extends App with LazyLogging{

  def selectSmall(instances: Seq[WikiHopInstance]) = instances.head // TODO: implement this correctly

  // Load the config
  // Load the data
  val instances = WikiHopParser.trainingInstances

  val episode = 1000
  val policy = None
  val memory = new TransitionMemory[Transition](maxSize = 100000)

  val instance = selectSmall(instances)

  val trainingObserver: AgentObserver = new AgentObserver {

    var state:Option[State] = None

    override def startedEpisode(env: WikiHopEnvironment): Unit = {}

    override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = {
      // Save the state observation before taking the action
      state = Some(env.observeState)
    }

    override def actionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {
      assert(state.isDefined, "The state should be defined at this point")
      val newState = env.observeState
      val transition = Transition(state.get, action, reward, newState)
      memory remember transition
      state = None
    }


    override def concreteActionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {}

    override def endedEpisode(env: WikiHopEnvironment): Unit = {}

    override def registerError(throwable: Throwable): Unit = {
      logger.error(throwable.getMessage)
    }
  }

  var successes = 0

  for(ep <- 1 to episode){
    logger.info(s"Epoch $ep")
    val agent = new PolicyAgent(policy)
    val outcome = agent.runEpisode(instance, Some(trainingObserver))

    val successful = outcome.nonEmpty
    if(successful)
      successes += 1

    if(ep % 100 == 0){
      val successRate = successes / 100.0
      logger.info(s"Success rate of $successRate for the last 100 episodes")
      successes = 0
    }

    // TODO store observations
    // TODO Update policy
  }

  // TODO: Do dataset split
  // Define the batch size
  // Collect the observations
  // Perform the update
  // Test for convergence
}
