package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import edu.cmu.dynet.{ComputationGraph, Expression, FloatVector, Initialize, ParameterCollection, RMSPropTrainer, Trainer}
import org.ml4ai.{WHConfig, WikiHopInstance}
import org.ml4ai.agents.{AgentObserver, EpGreedyPolicy, PolicyAgent}
import org.ml4ai.mdp.{Exploitation, Exploration, ExplorationDouble, WikiHopEnvironment, WikiHopState}
import org.ml4ai.utils.{TransitionMemory, WikiHopParser, rng}
import org.sarsamora.Decays
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.util.Random

object TrainFR extends App with LazyLogging{

  def selectSmall(instances: Seq[WikiHopInstance]) = instances.head // TODO: implement this correctly

  /**
    * Tests whether the parameters converged since the last update
    * @return
    */
  def converged = false // TODO: Implement this correctly

  /**
    * Updates the network with a minibatch
    * @param network
    */
  def updateParameters(network:DQN, trainer:Trainer)(implicit rng:Random):Unit = {
    // Sample a mini batch
    val miniBatch = memory.sample(1000)

    // TODO: Refactor this parameter
    val GAMMA = .9

    // Renew the computational graph
    ComputationGraph.renew()

    // Get the states from the batch and the entities associated to the action taken on that transition
    val states = miniBatch map { m => m.state}
    val selectedEntities = miniBatch map {
      tr =>
        tr.action match {
          case Exploration(single) => (single, single)
          case ExplorationDouble(entityA, entityB) => (entityA, entityB)
          case Exploitation(entityA, entityB) => (entityA, entityB)
          case _ => throw new NotImplementedException
        }
    }

    // Compute the state action values for all the actions of that (state, entities) argument
    val stateValues = network((states zip selectedEntities) map { case(s, (ea, eb)) => (s, ea, eb) })

    // Fetch the observed reward of that transition
    val rewards = miniBatch map { _.reward }

    // Fetch the resulting state of the transitions
    val nextStates = miniBatch map { m => m.nextState }
    val nextStateValues = // TODO figure out this correctly, this considers the state values of all the possible entity combinations, not just only of  the ones with the top entity
      max{
        network{
          nextStates.flatMap{
            ns =>
              val entityPairs =
                for {
                  ea <- ns.candidateEntities.get
                  eb <- ns.candidateEntities.get
                } yield (ea, eb)

              val ret = entityPairs.toSet.map{
                ep:(Set[String], Set[String]) =>
                  ep match {
                    case (ea, eb) => (ns, ea, eb)
                  }
              }
              ret
          }.take(100) // Keith: Please look here. Ideally there will not be a take statement here and network.apply,
                      // but I put it and teste it with multiple values with varying number of elements to reproduce the bug
        }.value()
      }

//    val updates = (rewards zip nextStateValues) map { case (r, q) => r + GAMMA*q}
//
//    val actions = miniBatch.map(_.action)
//
//    // Import this to make the code below more readable
//    import DQN.actionIndex
//
//    val targetStateValuesData =
//      // TODO Clean this, factor out the hard-coded num 2.
//      for(((action, tv), u) <- (actions zip stateValues.value().toSeq().grouped(2).toSeq).zip(updates) ) yield {
//        val ret = tv.toArray
//        ret(action) = u.toFloat // TODO: Select the correct action
//        ret
//      }
//
//    val targetStateValues = Expression.input(stateValues.dim(), FloatVector.Seq2FloatVector(targetStateValuesData.flatten.toSeq))
//
//    val loss = mseLoss(stateValues, targetStateValues)
//
//    //    ComputationGraph.forward(loss) // This might make the whole thing crash
//    ComputationGraph.backward(loss)
//
//    optimizer.update()
  }


  // Load the data
  val instances = WikiHopParser.trainingInstances

  val numEpisodes = WHConfig.Training.episodes
  val targetUpdate = WHConfig.Training.targetUpdate

  Initialize.initialize(Map("random-seed" -> 2522620396L, "dynet-mem" -> "2048"))
  val params = new ParameterCollection()
  implicit val eh: EmbeddingsHelper = new EmbeddingsHelper(params)
  val network = new DQN(params, eh)

  // Initialize the optimizer
  val optimizer = new RMSPropTrainer(params, learningRate = .01f, rho = .99f)

  val policy = new EpGreedyPolicy(Decays.exponentialDecay(WHConfig.Training.Epsilon.upperBound, WHConfig.Training.Epsilon.lowerBound, numEpisodes*10, 0).iterator, network)
  val memory = new TransitionMemory[Transition](maxSize = WHConfig.Training.transitionMemorySize)

  // TODO: Implement this
  val instance = selectSmall(instances)

  val trainingObserver: AgentObserver = new AgentObserver {

    var state:Option[WikiHopState] = None

    override def startedEpisode(env: WikiHopEnvironment): Unit = {}

    override def beforeTakingAction(action: Action, env: WikiHopEnvironment): Unit = {
      // Save the state observation before taking the action
      state = Some(env.observeState.asInstanceOf[WikiHopState])
    }

    override def actionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = ()


    override def concreteActionTaken(action: Action, reward: Float, numDocsAdded: Int, env: WikiHopEnvironment): Unit = {
      assert(state.isDefined, "The state should be defined at this point")
      val newState = env.observeState.asInstanceOf[WikiHopState]
      val transition = Transition(state.get, action, reward, newState)
      memory remember transition
      state = None
    }

    override def endedEpisode(env: WikiHopEnvironment): Unit = ()

    override def registerError(throwable: Throwable): Unit = {
      logger.error(throwable.getMessage)
    }
  }

  var successes = 0

  for(ep <- 1 to numEpisodes){
    if(!converged || ep < targetUpdate) {
      logger.info(s"Epoch $ep")
      val agent = new PolicyAgent(policy)
      val outcome = agent.runEpisode(instance, Some(trainingObserver))

      val successful = outcome.nonEmpty
      if (successful)
        successes += 1

      if (ep % targetUpdate == 0) {
        val successRate = successes / targetUpdate.toFloat
        logger.info(s"Success rate of $successRate for the last 100 episodes")
        successes = 0
        updateParameters(network, optimizer)
      }
    }
  }

  // Do dataset split
  // Define the batch size
  // Collect the observations
  // Perform the update
  // Test for convergence
}
