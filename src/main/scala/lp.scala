import Khachiyan._

import Solution._

package LP {

  object LP {

    // One element cache; a 15-20% speedup on a sample problem
    var cachedProblem : Option[(Problem, LinearProgram)] = None

    def edge(x : Int, y : Int, countPoints : Int): Int = {
      require (x != y)
      if (x > y) edge (y, x, countPoints)
        else x * countPoints + y - ((x + 1) * (x + 2)) / 2 + 1
    }

    def buildLP (prob: Problem) : LinearProgram = {
      val countPoints : Int = prob.countPoints

      val countVariables = edge (countPoints - 2, countPoints - 1, countPoints) // the last edge
      val boolBounds = new FullBounds (0.0, 1.0)
      val variables = Vector.fill(countVariables)(boolBounds)

      def pageCountConstraintEquation (i : Int) =
        new Equation (Range (0, countPoints).filter (_ != i).map (j => (edge (i, j, countPoints), 1.0)).toVector)
      val pageCountConstraintBounds = new FixedBound (prob.pageSize.toDouble - 1.0)

      val pageCountConstraints = Range(0, countPoints).
         map (i => (pageCountConstraintEquation(i), pageCountConstraintBounds)).
         toVector

      val maxOneBound = new UpperBound (1.0)
      // e(p1, p2) & e(p2, p3) => e(p1, p3)
      // e(p1, p2) + e(p2, p3) + (1 - e(p1, p3)) < 3
      // e(p1, p2) + e(p2, p3) + -1 * e(p1, p3) < 2
      // e(p1, p2) + e(p2, p3) + -1 * e(p1, p3) <= 1.0
      // (Equiv. in integer world only; stricter but correct with doubles)
      var transitiveConstraintsList = List [(Equation, Bound)]()
      // Limiting it to e(p1, p2) and e(p2, p3) such that those edges matter
      // cuts down on a lot of relatively unimportant edges for some little cost
      // Enormous speed improvement
      for (p1 <- Range (0, countPoints)) {
        for (p2 <- Range (0, countPoints) if p1 < p2) {
          for (p3 <- Range (0, countPoints) if p2 < p3) {
            val edge1 = edge (p1, p2, countPoints)
            val edge2 = edge (p2, p3, countPoints)
            val edge3 = edge (p1, p3, countPoints)
            def newEqu (e1 : Int, e2 : Int, e3 : Int) = 
               (new Equation (Vector ((e1, 1.0), (e2, 1.0), (e3, -1.0))),
               maxOneBound)
               
            val probEdge1 = prob.edges (p1).contains (p2)
            val probEdge2 = prob.edges (p2).contains (p3)
            val probEdge3 = prob.edges (p1).contains (p3)
            if (probEdge1 && probEdge2) 
                transitiveConstraintsList ::= newEqu (edge1, edge2, edge3)
            if (probEdge2 && probEdge3)
                transitiveConstraintsList ::= newEqu (edge2, edge3, edge1)
            if (probEdge1 && probEdge3)
                transitiveConstraintsList ::= newEqu (edge1, edge3, edge2)
          }
        }
      }
      val transitiveConstraints = transitiveConstraintsList.toVector
      val edgeSet = prob.edgeTuples.map (t => edge(t._1,t._2, countPoints)).toSet
      val objective = new Equation (Range (1, countVariables + 1).map (
      i => (i, if (edgeSet.contains(i)) 1.0 else 0.0)).toVector)

        new LinearProgram (countVariables, variables,
      pageCountConstraints ++ transitiveConstraints,
      objective, 0.0, true)
    }

    def bibuildLP (prob: Problem) : LinearProgram = {
      val countPoints : Int = prob.countPoints

      val countVariables = edge (countPoints - 2, countPoints - 1, countPoints) // the last edge
      val boolBounds = new FullBounds (0.0, 1.0)
      val variables = Vector.fill(countVariables)(boolBounds)

      val pageCountConstraintEquation =
        new Equation (Range (1, countPoints).map (j => (edge (0, j, countPoints), 1.0)).toVector)
      val pageCountConstraintBounds = new FixedBound (prob.pageSize.toDouble - 1.0)

      val pageCountConstraints =
        Vector ((pageCountConstraintEquation, pageCountConstraintBounds))

      val maxOneBound = new UpperBound (1.0)
      // e(p1, p2) & e(p2, p3) => e(p1, p3)
      // e(p1, p2) + e(p2, p3) + (1 - e(p1, p3)) < 3
      // e(p1, p2) + e(p2, p3) + -1 * e(p1, p3) < 2
      // e(p1, p2) + e(p2, p3) + -1 * e(p1, p3) <= 1.0
      // (Equiv. in integer world only; stricter but correct with doubles)
      var transitiveConstraintsList = List [(Equation, Bound)]()
      for (p2 <- Range (1, countPoints)
      if prob.edges(0).contains (p2)) {
       for (p3 <- Range (1, countPoints)
       if p2 != p3  && prob.edges(p2).contains (p3)) {
         transitiveConstraintsList = (new Equation (Vector ((edge (0, p2, countPoints), 1.0),
          (edge (p2, p3, countPoints), 1.0),
          (edge (0, p3, countPoints), -1.0))),
          maxOneBound) :: transitiveConstraintsList
        }
      }
      for (p2 <- Range (1, countPoints)
      if ! prob.edges(0).contains (p2)) {
       for (p3 <- Range (1, countPoints)
       if p2 != p3  && prob.edges(p2).contains (p3)) {
         transitiveConstraintsList = (new Equation (Vector ((edge (0, p2, countPoints), -1.0),
          (edge (p2, p3, countPoints), 1.0),
          (edge (0, p3, countPoints), 1.0))),
          maxOneBound) :: transitiveConstraintsList
        }
      }
      
      val transitiveConstraints = transitiveConstraintsList.toVector
      val edgeSet = prob.edgeTuples.map (t => edge(t._1,t._2, countPoints)).toSet
      val objective = new Equation (Range (1, countVariables + 1).map (
        i => (i, if (edgeSet.contains(i)) 1.0 else 0.0)).toVector)
        
      new LinearProgram (countVariables, variables,
        pageCountConstraints ++ transitiveConstraints,
        objective, 0.0, true)
    }
    
    def solve2 (partSol : PartialSolution): Option[Double] = synchronized {
      require (partSol.prob.countPoints == partSol.prob.pageSize * 2)
      val biLP = bibuildLP (partSol.prob)
      val lp = buildLP (partSol.prob)
      
      println ("Running biLP.")
      val ret = GLPK_Solver.solve (biLP, GLPK_Solver.Messages_On)
      ret match {
        case Right (ans) =>  println (ans.objective)
        case Left ((errorMsg, id)) =>
          println ("LP error: (" +  id + ") " + errorMsg)
      }
      println ("Running straight LP")
      val ret2 = GLPK_Solver.solve (lp, GLPK_Solver.Messages_On)
      ret2 match {
        case Right (ans) =>  println (ans.objective)
        case Left ((errorMsg, id)) =>
          println ("LP error: (" +  id + ") " + errorMsg)
      }      
      ret match {
        case Right (ans) => Some (ans.objective)
        case Left ((errorMsg, id)) =>
          println ("LP error: (" +  id + ") " + errorMsg)
        None
      }
    }
      
    
    def solve (partSol : PartialSolution): Option[Double] = synchronized {
      // use eq because we don't want to compare all the little vectors
      // and the odds a problem would get passed us that is a different
      // pointer and the same deeply is massively unlikely, and would not
      // be a correctness problem.
      if (cachedProblem.isEmpty || !(cachedProblem.get._1 eq partSol.prob)) {
        val newLP = 
          //if (partSol.prob.countPoints == partSol.prob.pageSize * 2) {
          //  bibuildLP (partSol.prob)
          //} 
          //else { 
            buildLP (partSol.prob)
          //}
        cachedProblem = Some ((partSol.prob, newLP))
      }
      val lp = cachedProblem.get._2
      val countPoints = partSol.prob.countPoints

      // Add pages from the partial solution
      val pageSets = partSol.pageSets
      var lpWithPartialSol = lp

      if (pageSets.length > 0) {
        for (page <- pageSets) {
          for (p1 <- page) {
            for (p2 <- Range (0, countPoints) if p1 != p2) {
              val isOnSamePage = if (page.contains(p2)) 1.0 else 0.0
              lpWithPartialSol = lpWithPartialSol.setVariable (edge(p1, p2, countPoints), isOnSamePage)
            }
          }
        }
      }

      val partialPage = partSol.partialPage
      if (partialPage.length > 1) {
        for (p1 <- partialPage) {
          for (p2 <- Range (0, partialPage.max + 1) if p1 != p2) {
            if (!partialPage.contains (p2) || p1 < p2) {
              val isOnSamePage = if (partialPage.contains (p2)) 1.0 else 0.0
              lpWithPartialSol = lpWithPartialSol.setVariable (edge(p1, p2, countPoints), isOnSamePage)
            }
          }
        }
      }

      val ret = GLPK_Solver.solve (lpWithPartialSol)
      ret match {
        case Right (ans) => Some (ans.objective)
        case Left ((errorMsg, id)) =>
          println ("LP error: (" +  id + ") " + errorMsg)
        None
      }
    }
    
  }
}
