import org.gnu.glpk.GLPK;
import org.gnu.glpk.GLPKConstants;
import org.gnu.glpk.GlpkException;
import org.gnu.glpk.SWIGTYPE_p_double;
import org.gnu.glpk.SWIGTYPE_p_int;
import org.gnu.glpk.glp_prob;
import org.gnu.glpk.glp_smcp;

package Khachiyan {

sealed abstract class Bound {}
case object NoBounds extends Bound {}
case class LowerBound (val lower: Double) extends Bound {}
case class UpperBound (val upper: Double) extends Bound {}
case class FullBounds (val lower: Double, val upper: Double) extends Bound {}
case class FixedBound (val equals: Double) extends Bound {}

class Equation (val equation : IndexedSeq [(Int, Double)]) {
   lazy val maxVariable = equation.map (_._1).max

   require (equation.map (_._1).min >= 0)
   require (equation.map (_._1).toSet.size == equation.size)
}

// Note that GLPK is 1-based, so the variables are numbered 1 .. countVariables
// and the bounds for variable i are found in variables (i - 1), since arrays are
// 0 based
class LinearProgram
   (val countVariables : Int,
   val variables : IndexedSeq [Bound],
   val constraints : IndexedSeq [(Equation, Bound)],
   val objective: Equation,
   val constantObjective : Double,
   val maximizeObjective : Boolean) {

   def setVariable (variable : Int, newValue : Double) : LinearProgram = {
      require (countVariables >= variable)
      new LinearProgram (countVariables,
         variables.updated(variable - 1, new FixedBound (newValue)), constraints,
         objective, constantObjective, maximizeObjective)
   }

   require (constraints.map (x => x._1.maxVariable).max <= countVariables)
   require (variables.length == countVariables)
   require (objective.maxVariable <= countVariables)
}

object GLPK_Solver {

   private val printCplex = false
   val Messages_Off : Int = GLPKConstants.GLP_MSG_OFF
   val Messages_Err : Int = GLPKConstants.GLP_MSG_ERR
   val Messages_On : Int = GLPKConstants.GLP_MSG_ON
   val Messages_All : Int = GLPKConstants.GLP_MSG_ALL
   
   var c : Int = 0
   
   def solve (lp: LinearProgram, errorLevel : Int = Messages_Off):
      Either[(String, Int), LinearAnswer] = synchronized
   {
      val glpk_lp = GLPK.glp_create_prob()

      // Insert the variables (columns)
      GLPK.glp_add_cols(glpk_lp, lp.countVariables)
      for ((varBound, i) <- lp.variables.zipWithIndex) {
         varBound match {
            case NoBounds => ;
            case UpperBound (x) =>
               GLPK.glp_set_col_bnds(glpk_lp, i + 1, GLPKConstants.GLP_UP, x, x)
            case LowerBound (x) =>
               GLPK.glp_set_col_bnds(glpk_lp, i + 1, GLPKConstants.GLP_LO, x, x)
            case FullBounds (x, y) =>
               GLPK.glp_set_col_bnds(glpk_lp, i + 1, GLPKConstants.GLP_DB, x, y)
            case FixedBound (x) =>
               GLPK.glp_set_col_bnds(glpk_lp, i + 1, GLPKConstants.GLP_FX, x, x)
         }
      }

      // Insert the constraints (rows)
      GLPK.glp_add_rows(glpk_lp, lp.constraints.size);
      val maxArrayLength = lp.constraints.map (_._1.equation.size).max
      val indices = GLPK.new_intArray(maxArrayLength + 1);
      val values = GLPK.new_doubleArray(maxArrayLength + 1);
      for (((equ, equBound), currRow) <- lp.constraints.zipWithIndex) {
         equBound match {
            case NoBounds => ;
            case UpperBound (x) =>
               GLPK.glp_set_row_bnds(glpk_lp, currRow + 1, GLPKConstants.GLP_UP, x, x)
            case LowerBound (x) =>
               GLPK.glp_set_row_bnds(glpk_lp, currRow + 1, GLPKConstants.GLP_LO, x, x)
            case FullBounds (x, y) =>
               GLPK.glp_set_row_bnds(glpk_lp, currRow + 1, GLPKConstants.GLP_DB, x, y)
            case FixedBound (x) =>
               GLPK.glp_set_row_bnds(glpk_lp, currRow + 1, GLPKConstants.GLP_FX, x, x)
         }
         for (((equVar, equVal), i) <- equ.equation.zipWithIndex) {
            GLPK.intArray_setitem(indices, i + 1, equVar)
            GLPK.doubleArray_setitem(values, i + 1, equVal)
         }
         GLPK.glp_set_mat_row(glpk_lp, currRow + 1, equ.equation.length, indices, values)
      }
      GLPK.delete_intArray(indices)
      GLPK.delete_doubleArray(values)

      // Set the objective function
      if (lp.maximizeObjective) GLPK.glp_set_obj_dir(glpk_lp, GLPKConstants.GLP_MAX)
      else GLPK.glp_set_obj_dir(glpk_lp, GLPKConstants.GLP_MIN)


      GLPK.glp_set_obj_coef(glpk_lp, 0, lp.constantObjective) // Set constant value
      for ((equVar, equVal) <- lp.objective.equation) {
         GLPK.glp_set_obj_coef(glpk_lp, equVar, equVal)
      }

      // Solve model
      val parm = new glp_smcp()
      GLPK.glp_init_smcp(parm)
      parm.setMsg_lev(errorLevel)
      parm.setPresolve(GLPKConstants.GLP_ON)
      if (printCplex) {
        GLPK.glp_write_lp(glpk_lp, null, "test" + c + ".cplex")
        c = c + 1
      }
      val ret = GLPK.glp_simplex(glpk_lp, parm)
      if (ret != 0) {
         GLPK.glp_delete_prob (glpk_lp)
         ret match {
            case GLPKConstants.GLP_EBADB | GLPKConstants.GLP_ESING | GLPKConstants.GLP_ESING =>
               Left (("There is a problem with the initial basis. " +
                     "This message should be impossible.", ret))
            case GLPKConstants.GLP_EBOUND =>
               Left (("There is an invalid double bounds in the problem.", ret))
            case GLPKConstants.GLP_EFAIL =>
               Left (("The solver failed.", ret))
            case GLPKConstants.GLP_EOBJLL | GLPKConstants.GLP_EOBJUL =>
               Left(("The objective function can change without limit in the dual simplex.", ret))
            case GLPKConstants.GLP_EITLIM =>
               Left(("The iteration limit was exceeded.", ret))
            case GLPKConstants.GLP_ETMLIM =>
               Left(("The time limit was exceeded.", ret))
            case GLPKConstants.GLP_ENOPFS =>
               Left(("The problem has no primal feasible solutions.", ret))
            case GLPKConstants.GLP_ENODFS =>
               Left(("The problem has no dual feasible solutions.", ret))
            case _ =>
               Left(("An unknown problem has occurred.", ret))
         }
      } else {
         val objVal = GLPK.glp_get_obj_val (glpk_lp)
         val coeffs = new Array[Double](lp.countVariables)
         for (i <- Range (0, lp.countVariables)) {
            coeffs (i) = GLPK.glp_get_col_prim (glpk_lp, i + 1)
         }
         GLPK.glp_delete_prob(glpk_lp)
         Right (new LinearAnswer (coeffs.toVector, objVal))
      }
   }
}

class LinearAnswer (val variableValues : Vector [Double], val objective : Double);

}
