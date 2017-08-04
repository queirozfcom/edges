package com.queirozf.helpers

import com.queirozf.models.{DirectedEdge, DistanceToNode, Edge, Node}

import scala.collection._

/**
  * Created by felipe on 31/07/17.
  */
object GraphHelper {

  /**
    * Given a list of lines of the form "4 5"
    * Return a list of DirectedEdges (i.e. one separate edge for each ordering)
    *
    * @param lines
    * @return
    */
  def buildDirectedEdges(lines: List[String]): List[DirectedEdge] = {
    lines
      .map(str => str.split(" "))
      .filter(parts => parts.length == 2) // drop malformed
      .flatMap(parts => List((parts(0), parts(1)), (parts(1), parts(0))))
      .map {
        case (from, to) => DirectedEdge(from, to)
      }
  }

  /**
    * Build an index with all edges pointing out of some node, so that we don't need to traverse
    * the whole node list everytime we need to get them.
    *
    * @param edges
    * @return
    */
  def buildOutgoingEdgeIndex(edges: List[DirectedEdge]) = edges.groupBy(edge => edge.fromLabel)


  /**
    * For a given node, return its distance with respect to all other nodes in the graph
    *
    * @param sourceNode
    * @param edgeIndex
    * @return
    */
  def getDistances(sourceNode: String, edgeIndex: Map[String, List[DirectedEdge]]): List[DistanceToNode] = {

    val visitedEdges = mutable.Set.empty[(DirectedEdge, Int)]

    def getDistancesIter(currentSourceNode: String,
                          currentDistances: List[DistanceToNode],
                          distanceValue: Int): List[DistanceToNode] = {

      edgeIndex.get(currentSourceNode) match {
        case Some(outgoingEdges) => {

          // an edge only counts as "visited" is it's been visited by a path with a size less than or equal to the current one.
          val unvisitedNewEdges = outgoingEdges.filterNot(outgoingEdge => visitedEdges.exists(visit => visit._1 == outgoingEdge && (visit._2 <= distanceValue)))

          val newDistances = currentDistances ++ unvisitedNewEdges.map(outgoingEdge => DistanceToNode(outgoingEdge.toLabel, distanceValue))

          unvisitedNewEdges.foreach(edge => visitedEdges.add((edge, distanceValue)))

          if (unvisitedNewEdges.nonEmpty) {
            // recursion step: visit the next nodes, pointed to by the current node
            val updatedDistances = newDistances ++ unvisitedNewEdges.flatMap { edge =>
              getDistancesIter(edge.toLabel, newDistances, distanceValue + 1)
            }
            pruneDistances(updatedDistances, currentSourceNode)

          } else newDistances // base case: all edges have been visited
        }
        case None => currentDistances // base case: the current node doesn't point to anybody else in the graph
      }
    }

    getDistancesIter(sourceNode, List.empty[DistanceToNode], distanceValue = 1)

  }

  /**
    * Remove all values that aren't the shortest distance to a node.
    *
    * @param allDistances
    * @param sourceNode
    * @return
    */
  private def pruneDistances(allDistances: List[DistanceToNode], sourceNode: String): List[DistanceToNode] = {
    allDistances
      .groupBy { case DistanceToNode(targetLabel, _) => targetLabel }
      .map { case (targetLabel, distances) => (targetLabel, distances.map(_.value).min) }
      .map { case (label, value) => DistanceToNode(label, value) }
      .toList
      .distinct
      .filterNot(distance => distance.targetLabel == sourceNode)
  }
}
