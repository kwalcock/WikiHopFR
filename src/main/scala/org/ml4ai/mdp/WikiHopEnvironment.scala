package org.ml4ai.mdp

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.WikiHopInstance
import org.ml4ai.inference.{CoocurrenceKnowledgeGraph, KnowledgeGraph}
import org.ml4ai.ir.LuceneHelper
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.states.State
import org.ml4ai.utils.rng

//import collection.Set
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class WikiHopEnvironment(start:String, end:String) extends Environment {

  private implicit val loader:AnnotationsLoader = WikiHopEnvironment.annotationsLoader

  // TODO add a factory pattern to change this without recompiling
  type KG = CoocurrenceKnowledgeGraph

  def this(wikiHopKey:String) {
    this(WikiHopEnvironment.getTrainingInstance(wikiHopKey).query.split(" ").last, WikiHopEnvironment.getTrainingInstance(wikiHopKey).answer.get)
  }


  // Control values
  val maxIterations = 10

  // State variables
  private var knowledgeGraph:Option[KnowledgeGraph] = None
  private var iterationNum:Int = 0
  private val exploredEntities = new mutable.HashSet[Set[String]]
  private val exploitedEntities = new mutable.HashSet[Set[String]]
  private val papersRead = new mutable.HashSet[String]



  private def exploitationEligible(e: Set[String]) = !(exploitedEntities contains e)
  private def explorationEligible(e: Set[String]) = !(exploredEntities contains e)



  override def possibleActions: Seq[Action] = {
    // Generate the possible actions to be taken given the current state of the environment
    val actions =
      knowledgeGraph match {
        // If this is the first call and there isn't a KG yet, generate the actions given those two nodes
        case None =>
          List(
            Exploration(start.split(" ").toSet),
            Exploration(end.split(" ").toSet),
            ExplorationDouble(start.split(" ").toSet, end.split(" ").toSet),
            Exploitation(start.split(" ").toSet, end.split(" ").toSet),
          )
        // Otherwise, procedurally generate the list of actions
        case Some(kg) =>
          val currentEntities = kg.entities

          val ret = new mutable.ListBuffer[Action]

          currentEntities foreach {
            e =>

              if(explorationEligible(e))
                ret += Exploration(e)

              if(exploitationEligible(e))
                ret += Exploitation(e, end.split(" ").toSet)

              for(i <- currentEntities if i != e && explorationEligible(i)){
                ret += ExplorationDouble(e, i)
              }
          }

          ret.toList
      }

    // Prepend the random action to the list of candidate actions
    RandomAction :: actions
  }

  /**
    * Computes the scalar reward of taking the current action
    * @param action taken
    * @return
    */
  private def rewardSignal(action: Action, newState:KG, fetchedPapers:Set[String]):Double = {
    // TODO make the reward function more nuanced
    val newPapers:Int = (fetchedPapers diff fetchedPapers).size
    val newRelations:Int =
      (newState.edges diff
        (knowledgeGraph match {
          case Some(kg) => kg.edges
          case None => Set.empty
        })
      ).size

    val livingReward = 0.5

    (newRelations / newPapers) - livingReward
  }

  override def execute(action: Action, persist: Boolean): Double = {
    // Increment the iteration counter
    iterationNum += 1
    // If the random action is selected, transform it to a concrete action randomly
    val finalAction = action match {
      case RandomAction => buildRandomAction
      case a:Action => a
    }
    val fetchedDocs = LuceneHelper.retrieveDocumentNames(finalAction)
    // Generate new KG from the documents
    val kg = new KG(fetchedDocs)
    val reward = rewardSignal(action, kg, fetchedDocs)

    // Update the knowledge graph
    knowledgeGraph = Some(kg)

    reward
  }


  private def buildRandomAction():Action = {
    val randomInt:Int = rng.nextInt(3)

    val a:Set[String] = ??? //sampleRandomEntity()
    randomInt match {
      case 0 => Exploration(a)
      case 1 =>
        val b:Set[String] = ??? //sampleRandomEntity()
        ExplorationDouble(a, b)
      case 2 =>
        Exploitation(a, end.split(" ").toSet)
      case _ =>
        throw new UnsupportedOperationException("Error in the random action generator. Unspecified action code.")
    }
  }

  override def observeState: State = WikiHopState(iterationNum)

  override def finishedEpisode: Boolean = {
    //TODO: Consider quiting if the state didn't change after an action.
    if(iterationNum >= maxIterations)
      true
    else{
      knowledgeGraph match {
        case Some(kg) =>
          val paths = kg.findPath(start, end)
          if(paths.nonEmpty)
            true
          else
            false
        case None => false
      }
    }
  }

}

object WikiHopEnvironment extends LazyLogging {

  private val config = ConfigFactory.load()

  private def getInstance(data:Iterable[WikiHopInstance], key:String):WikiHopInstance =
    Try(data.filter(_.id == key)) match {
      case Success(values) if values.size == 1 => values.head
      case Success(_) =>
        val msg = "More than one instance with tha same key"
        logger.error(msg)
        throw new UnsupportedOperationException(msg)
      case Failure(exception) =>
        logger.error(exception.getMessage)
        throw exception

    }

  def getTrainingInstance(key:String):WikiHopInstance = getInstance(WikiHopParser.trainingInstances, key)

  def getTestingInstance(key:String):WikiHopInstance = getInstance(WikiHopParser.testingInstances, key)

  lazy val annotationsLoader = new AnnotationsLoader(config.getString("files.annotationsFile"), cache = false)
}
