package org.ml4ai.utils

import org.clulab.processors.RelationTriple
import org.clulab.struct.Interval

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

object RelationTripleUtils {

  private def compareInterval(a:Interval, b:Interval):Boolean = {
    if(a.end < b.end) false
    else if(b.end < a.end) true
    else {
      if(a.start > b.start) false
      else true
    }
  }

  @tailrec
  private def collapseIntervalsHelper(results:List[Interval], current:Interval, reminder:Iterable[Interval]):List[Interval] = {
    if(reminder.isEmpty) current::results
    else{
      val candidate = reminder.head
      if(current contains candidate) collapseIntervalsHelper(results, current, reminder.tail)
      else collapseIntervalsHelper(current::results, candidate, reminder.tail)
    }
  }

  private def collapseIntervals(intervals:Iterable[Interval]):Array[Interval] = {
    if(intervals.size > 1) {
      val sortedIntervals = intervals.toSeq.sortWith(compareInterval)
      collapseIntervalsHelper(Nil, sortedIntervals.head, sortedIntervals.tail).toArray
    }
    else
      intervals.toArray
  }

  def pruneRelations(triples:Seq[RelationTriple]):Set[RelationTriple] = {
    val subjects = collapseIntervals(triples map (_.subjectInterval)).sortBy(_.length)
    val objects = collapseIntervals(triples map (_.objectInterval)).sortBy(_.length)
    val rels = collapseIntervals(triples map (_.relationInterval) collect { case Some(r) => r}).sortBy(_.length)

    triples.map{
      t =>
        val newSubject = subjects.dropWhile(!_.contains(t.subjectInterval)).head
        val newObject = objects.dropWhile(!_.contains(t.objectInterval)).head
        val newRel = t.relationInterval match {
          case Some(r) => Some(rels.dropWhile(!_.contains(r)).head)
          case None => None
        }

        RelationTriple(t.confidence, newSubject, newRel, newObject)
    }.toSet.filter{
      t =>
        if(t.subjectInterval.overlaps(t.objectInterval))
          false
        else if(t.relationInterval.isDefined && t.subjectInterval.overlaps(t.relationInterval.get))
          false
        else if(t.relationInterval.isDefined && t.objectInterval.overlaps(t.relationInterval.get))
          false
        else
          true
    }
  }

}
