package net.verdagon.vale.astronomer

import net.verdagon.vale._
import net.verdagon.vale.scout.{INameS, IRuneS, ITemplataType, RuneNameS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.solver.{IIncompleteOrFailedSolve, ISolverStateForRule, IncompleteSolve, Solver}

case class GenericSolveError(range: RangeS, failedSolve: IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplataType, Unit]) {
  vpass()
}

object GenericSolver {
//  def getRunes(rule: IRulexSR): Array[IRuneS] = {
//    val sanityCheck =
//      rule match {
//        case PrototypeComponentsSR(_, resultRune, nameRune, paramsListRune, returnRune) => Array(resultRune, nameRune, paramsListRune, returnRune)
//        case PrototypeSR(_, resultRune, name, parameters, returnTypeRune) => Array(resultRune) ++ parameters ++ Array(returnTypeRune)
//        case PackSR(_, resultRune, members) => Array(resultRune) ++ members
//        // These act like Lookup for the generic solver
//        case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(resultRune)
//        case ManualSequenceSR(_, resultRune, elements) => Array(resultRune)
//        case CallSR(_, resultRune, templateRune, args) => Array(resultRune, templateRune)
//        case LookupSR(_, rune, literal) => Array(rune)
//        case RuneParentEnvLookupSR(_, rune) => Array(rune)
//        // These all act like Equals for the generic solver
//        case AugmentSR(_, resultRune, literal, innerRune) => Array(resultRune, innerRune)
//        case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => Array(resultRune, kindRune)
//        case CoerceToCoord(_, coordRune, kindRune) => Array(coordRune, kindRune)
//        case CoordIsaSR(_, sub, suuper) => Array(sub, suuper)
//        case KindIsaSR(_, sub, suuper) => Array(sub, suuper)
//        case EqualsSR(_, left, right) => Array(left, right)
//        // Inconsequential to generic solver
//        case OneOfSR(_, rune, literals) => Array()
//        case LiteralSR(_, rune, literal) => Array()
//        case KindComponentsSR(_, resultRune, mutabilityRune) => Array(resultRune, mutabilityRune)
//        // We might like these to conclude whether an interface, but lets see if we can get along without it
//        case IsConcreteSR(_, rune) => Array()
//        case IsInterfaceSR(_, rune) => Array()
//        case IsStructSR(_, rune) => Array()
//      }
//    val result = rule.runeUsages
//    vassert(result.map(_.rune) sameElements sanityCheck.map(_.rune))
//    result.map(_.rune)
//  }

}
