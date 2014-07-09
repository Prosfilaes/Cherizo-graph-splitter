// import akka.actor._
import LP._

package Solution {

final class Problem
   (val countPoints : Int, 
    // points are labeled 0 .. countPoints - 1, so countPoints >= 1
    val edgeTuples : List[Tuple2[Int, Int]], 
    val pageSize : Int) 
{
   val startTime = System.currentTimeMillis
   override def toString = "# Points: " + countPoints.toString + " # Edges: " +
      edgeTuples.length + " Page size: " + 
      pageSize.toString + " Edges: " + edgeTuples.toString
      
   private def tuplesToVector 
      (tuples : List[Tuple2[Int, Int]], edges: Vector[Set[Int]]) : Vector[Set[Int]] =
   {
      val head = tuples.head
      if (head._1 < 0 || head._1 >= countPoints || head._2 < 0 || head._2 >= countPoints) {
         throw new Exception ("tuple " + head.toString + " is out of range")
      }
      val newEdges = edges.updated (head._1, edges(head._1) + head._2).
         updated (head._2, edges(head._2) + head._1)
      if (tuples.tail.isEmpty) newEdges
      else tuplesToVector (tuples.tail, newEdges)
   }
   val edges = tuplesToVector (edgeTuples, 
      Vector[Set[Int]] ().padTo (countPoints, Set[Int]()))
   if (countPoints < 0) throw new Exception ("the number of points must be positive")
   if (countPoints % pageSize != 0) throw new Exception (
      "the page size must divide the number of points")
}

sealed abstract class Solution {
   val prob : Problem
   protected val pointList : Vector[Int]
   // This is cost, not value, because less is better
   val cost : Int
   
   override def toString: String = {
      if (pointList.length == 0) "Empty solution!"
      else {
         def listToString (l : Seq[String]) = {
            "(" + l.dropRight(1).map (_ + ", ").fold ("")(_ + _) + l.last + ")"
         }
         val pages = pointList.map (_.toString).sliding (prob.pageSize, prob.pageSize).toSeq
         listToString (pages.map (listToString(_)))
      }
   }

	// countCrossPageLinks counts the number of edges from one point on Page 
	// to points off page
   final protected def countCrossPageLinks (edges : Set[Int], page : Set[Int]) =
			(edges -- page).size 
      
   protected def currentPageCost (fullPages : Vector[Int]) = {
		val pageSets = fullPages.sliding (prob.pageSize, prob.pageSize).map (_.toSet)
		pageSets.
		   map (page => page.toIndexedSeq.map (point => countCrossPageLinks (prob.edges (point), page))).
		   flatten.sum
	}
   
   // XXX: Deeper is really only a valid option on PartialSolutions. So far we can
   // (A) Force every user to cast from Solution to PartialSolution everytime they use 
   //     it and get a PartialSolution back (ugly)
   // (B) Provide a dummy solution of returning self, which lets certain processes do mass
   //     deeper without checking everything and lets other buggy ones run unboundedly
   //     (Right now, all problems have an equal number of deepers from start to end,
   //     but there's a profitable optimization on the table to do certain forced deepers
   //     in certain cases.)
   // (C) Toss an exception for complete solutions. I think this is the worse of the three
   //     solutions; we are forced in all cases to check if it's a PartialSolution like (A)
   //     and yet we don't get compile-time support like (A). (B) avoids a lot of checks,
   //     but won't catch certain bugs.
   //
   // Going with (B), the lazy but flexible solution.
   def deeper : Solution = this
   def deeperList : IndexedSeq[Solution] = Vector(this)
   val isComplete : Boolean
   val numSetPoints : Int = prob.countPoints
   
   lazy val fullPages = pointList.take (numSetPoints - numSetPoints % prob.pageSize)
   
   lazy val pageSets = fullPages.sliding (prob.pageSize, prob.pageSize).
      map (_.toSet).toIndexedSeq
}

class CompleteSolution (val prob : Problem, val pointList : Vector[Int]) extends Solution {
	// The cost of this problem is the number of crosspage links
	override lazy val cost = currentPageCost (pointList)
	override val isComplete = true
	
	def canonize : CompleteSolution = {
	   new CompleteSolution (prob, pageSets.sortBy (_.min).map (_.toSeq.sorted).flatten.toVector)
	}

   def swap (i : Int, j : Int) = {
      val iPoint = pointList(i)
      val jPoint = pointList(j)
      new CompleteSolution (prob, pointList.updated(i, jPoint).updated(j, iPoint))
   }
	
	def tryAllSwaps : CompleteSolution = {
	   var count = 0
	   for (i <- Range (0, pointList.length - 1);
	      j <- Range (i, pointList.length))
	   {
	      count = count + 1
	      val testSwap = swap (i, j)
	      if (testSwap.cost < cost) {
	         //val time = (System.currentTimeMillis - prob.startTime) / 1000
   	      return testSwap.tryAllSwaps 
   	   }
	   }
	   this.canonize
	}
	
	// Verify pointList is valid
	if (pointList.length != prob.countPoints) {
	   throw new Exception ("pointList is the wrong length");
	}
	if (pointList.toSet != Range (0, prob.countPoints).toSet) {
	   throw new Exception ("pointList is not a permutation of the possible points")
	}
}

final class PartialSolution (val prob : Problem, val pointList : Vector[Int]) 
   extends Solution 
{   
   // To exclude isomorphic solutions, partial solutions will be of the form
   // (0 ...) (x {where x is the least element not already in a page} ...) (y {ditto})...
   // where elements are in order on each page
   // If a page has any elements on it, it will have at least the least element and
   // one more, unless it's the initial page that starts with just 0
   
   
   lazy val partialPage = {
      pointList.drop (numSetPoints - numSetPoints % prob.pageSize)
   }
   
   lazy val fastCost = {
		// tack on rest of elements as one large page
		val pageSetsPlus = pageSets :+ (Range (0, prob.countPoints).toSet -- fullPages.toSet)		
		val c = pageSetsPlus.
		   map (page => page.toIndexedSeq.map (point => countCrossPageLinks (prob.edges (point), page))).
		   flatten.sum
		c
   }
   
   lazy val lpCost = {   
      val costFromLP = LP.solve (this)
      if (costFromLP.isEmpty) {
         println ("LP returned None! For " + toString)
      }
      val numEdges : Int = prob.edgeTuples.length
      // the LP returns the number edges that stay on their page; we want the inverse,     
      costFromLP.map (x => (numEdges - x - 0.1).ceil * 2.0).getOrElse(0.0).toInt
   }
   
   lazy val cost = {
      fastCost.max (lpCost)
   }
   
   override val isComplete = false
   
   override val numSetPoints = pointList.length  
   
   def this(prob: Problem) = this (prob, Vector (0))
   
   protected def nextPointList : Option [Vector [Int]] = {
      val unusedSuccessors = 
         (Range (pointList.last + 1, prob.countPoints).toSet -- pointList.toSet).
            toIndexedSeq.sorted
      // if there's n spaces left in the page, then there needs to be n+1 elements
      // to fill them.
      val neededPoints = prob.pageSize - (numSetPoints % prob.pageSize)
      if (unusedSuccessors.size < neededPoints) {
         None
      }
      else if (numSetPoints % prob.pageSize == 1) {
         //We're looking at a bare start with an unchangable base
         None
      }
      else {
         // OPTIMIZE: if we have only n points left, we should fill it instead of forcing
         // the programmer to go several layers of deeper, but that complicates things
         Some (pointList.dropRight (1) :+ unusedSuccessors.min)
      }
   }
            
   def next = {
      nextPointList.map (new PartialSolution (prob, _))
   }
   
   override def deeper = {
      val unused = Range (0, prob.countPoints).toSet -- pointList.toSet
      if (numSetPoints == prob.countPoints - prob.pageSize) {
         new CompleteSolution (prob, pointList ++ unused.toIndexedSeq.sorted)
      }
      else if (numSetPoints % prob.pageSize == 0) {
         new PartialSolution (prob, pointList :+ unused.min :+ (unused - unused.min).min)
      }
      else {
         new PartialSolution (prob, pointList :+ unused.filter (_ < pointList.last).min)
      }
   }
   
   override def deeperList = {
      val unused = Range (0, prob.countPoints).toSet -- pointList.toSet
      if (numSetPoints == prob.countPoints - prob.pageSize) {
         Vector (new CompleteSolution (prob, pointList ++ unused.toIndexedSeq.sorted))
      }
      else if (numSetPoints % prob.pageSize == 0) {
         val newUnused = (unused - unused.min).toIndexedSeq.sorted.
            dropRight (prob.pageSize - 2)
         newUnused.map (newPoint => 
            new PartialSolution (prob, pointList :+ unused.min :+ newPoint))
     }
     else {
         val newUnused = unused.filter (_ > pointList.last).toIndexedSeq.sorted.
            dropRight (prob.pageSize - numSetPoints % prob.pageSize - 1)
         newUnused.map (newPoint => new PartialSolution (prob, pointList :+ newPoint))
     }
   }
   
}

object Solution {
   def quickQuickSol (prob: Problem) : CompleteSolution = {
      new CompleteSolution (prob, Range (0, prob.countPoints).toVector)
   }
   def quickSol1 (prob: Problem) : CompleteSolution = {
      def quickVector (pagesLeft : Int, unused : Set[Int]): Vector[Int] = {
         
         def fillPage (pgVec : Vector[Int]): Vector[Int] = {
            val connectedPoints = 
               (unused & pgVec.map (x => prob.edges(x)).flatten.toSet) -- pgVec.toSet
            if (connectedPoints.size == 0) {
               val least = (unused -- pgVec.toSet).min
               if (prob.pageSize - pgVec.size == 1) {
                  pgVec :+ least
               }
               else {
                  fillPage (pgVec :+ least)
               }
            }
            else if (connectedPoints.size >= prob.pageSize - pgVec.size) {
               pgVec ++ connectedPoints.take(prob.pageSize - pgVec.size)
            }
            else {
               fillPage (pgVec ++ connectedPoints)
            }
         }
         
         if (pagesLeft == 1) unused.toVector.sorted
         else {
            val page = fillPage (Vector (unused.min))
            page ++ quickVector (pagesLeft - 1, unused -- page.toSet)
         }
      }
       
      new CompleteSolution (prob, 
         quickVector (prob.countPoints / prob.pageSize, 
            Range(0, prob.countPoints).toSet))
   }
   
   def quickSol (prob: Problem) : CompleteSolution = {
      val sol1 = quickQuickSol (prob)
      val sol2 = quickSol1 (prob)
      if (sol1.cost < sol2.cost) sol1
      else sol2
  }

   def branchAndBound (prob: Problem) : CompleteSolution = {
      branchAndBound (Solution.quickSol (prob))
   }
   
   def branchAndBound (bestSolStart : CompleteSolution) : CompleteSolution = {
      val prob = bestSolStart.prob
      
      var bestSolution : CompleteSolution = bestSolStart
      var leastCost : Int = bestSolution.cost
      
      var count : Int = 0      
      def printPartial (partial: PartialSolution) : Unit = {
         val time = (System.currentTimeMillis - prob.startTime) / 1000
         println ("bnb " + count.toString + " (" + time.toString + "): " + 
               partial.toString + " / " + partial.cost.toString)
         count = count + 1
      }  
      
      def bnbRecur (partial: PartialSolution) : Unit = {
         printPartial (partial)         
         val (completeSols, partialSols) = partial.deeperList.partition (_.isComplete)
         completeSols.map {
            s => if (s.cost < leastCost) {
               leastCost = s.cost
               bestSolution =  s.asInstanceOf[CompleteSolution]
               println ("bnb curr best: " + bestSolution.toString + " / " + leastCost.toString)
            }
         }
            
         val orderedList = partialSols.map(_.asInstanceOf[PartialSolution]).sortBy (_.cost)
         for (s <- orderedList) {
            if (s.cost < leastCost) bnbRecur (s)
         }         
      }
      
      bnbRecur (new PartialSolution (prob))
      bestSolution
   }
   
   def simulatedAnnealing (prob: Problem, startThresh : Int, seed : Int) = {
      val drop = 1.1
      val step = 100 * 1000
      val r = new scala.util.Random (seed)
      
      def saStep (sol : CompleteSolution, currThresh : Double, currStep : Int) : CompleteSolution = {
         if (currStep == 0) sol
         else {
            val i = r.nextInt (prob.countPoints)
            val j = r.nextInt (prob.countPoints)
            if (i == j) saStep (sol, currThresh, currStep)
            else {
               val newSol = sol.swap (i, j)
               if (newSol.cost < sol.cost + currThresh.floor.toInt) {
                  saStep (newSol, currThresh / drop, currStep - 1)
               } 
               else {saStep (sol, currThresh / drop, currStep - 1)}
            }
         }         
      }
      
      saStep (quickSol(prob), startThresh.toDouble, step).canonize
   }
}

}
