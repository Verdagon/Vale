package net.verdagon.vale.scout

import net.verdagon.vale._
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.solver.{IIncompleteOrFailedSolve, ISolverStateForRule, IncompleteSolve, Solver}

case class RuneTypeSolveError(range: RangeS, failedSolve: IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplataType, Unit]) {
  vpass()
}

object RuneTypeSolver {
  def getRunes(rule: IRulexSR): Array[IRuneS] = {
    val sanityCheck =
      rule match {
        case LookupSR(range, rune, literal) => Array(rune)
        case RuneParentEnvLookupSR(range, rune) => Array(rune)
        case EqualsSR(range, left, right) => Array(left, right)
        case CoordIsaSR(range, sub, suuper) => Array(sub, suuper)
        case KindIsaSR(range, sub, suuper) => Array(sub, suuper)
        case KindComponentsSR(range, resultRune, mutabilityRune) => Array(resultRune, mutabilityRune)
        case CoordComponentsSR(range, resultRune, ownershipRune, permissionRune, kindRune) => Array(resultRune, ownershipRune, permissionRune, kindRune)
        case PrototypeComponentsSR(range, resultRune, nameRune, paramsListRune, returnRune) => Array(resultRune, nameRune, paramsListRune, returnRune)
        case OneOfSR(range, rune, literals) => Array(rune)
        case IsConcreteSR(range, rune) => Array(rune)
        case IsInterfaceSR(range, rune) => Array(rune)
        case IsStructSR(range, rune) => Array(rune)
        case CoerceToCoord(range, coordRune, kindRune) => Array(coordRune, kindRune)
        case LiteralSR(range, rune, literal) => Array(rune)
        case AugmentSR(range, resultRune, literal, innerRune) => Array(resultRune, innerRune)
        case CallSR(range, resultRune, templateRune, args) => Array(resultRune, templateRune) ++ args
        case PrototypeSR(range, resultRune, name, parameters, returnTypeRune) => Array(resultRune) ++ parameters ++ Array(returnTypeRune)
        case PackSR(range, resultRune, members) => Array(resultRune) ++ members
        case RepeaterSequenceSR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune)
        case ManualSequenceSR(range, resultRune, elements) => Array(resultRune) ++ elements
//        case CoordListSR(range, resultRune, elements) => Array(resultRune) ++ elements
      }
    val result = rule.runeUsages
    vassert(result.map(_.rune) sameElements sanityCheck.map(_.rune))
    result.map(_.rune)
  }

  def getPuzzles(predicting: Boolean, rule: IRulexSR): Array[Array[IRuneS]] = {
    rule match {
      case EqualsSR(_, leftRune, rightRune) => Array(Array(leftRune.rune), Array(rightRune.rune))
      case LookupSR(_, rune, _) => {
        if (predicting) {
          // This Array() literally means nothing can solve this puzzle.
          // It needs to be passed in via plan/solve's initiallyKnownRunes parameter.
          Array()
        } else {
          // We need to know the type beforehand, because we don't know if we'll be coercing or not.
          Array(Array(rune.rune))
        }
      }
      case RuneParentEnvLookupSR(_, rune) => {
        if (predicting) {
          // This Array() literally means nothing can solve this puzzle.
          // It needs to be passed in via plan/solve's initiallyKnownRunes parameter.
          Array()
        } else {
          // We need to know the type beforehand, because we don't know if we'll be coercing or not.
          Array(Array(rune.rune))
        }
      }
      case CallSR(range, resultRune, templateRune, args) => {
        // We can't determine the template from the result and args because we might be coercing its
        // returned kind to a coord. So we need the template.
        // We can't determine the return type because we don't know whether we're coercing or not.
        Array(Array(resultRune.rune, templateRune.rune))
      }
      case PackSR(_, resultRune, members) => {
        // Packs are always lists of coords
        Array(Array())
//        if (members.nonEmpty) {
//          Array(Array(resultRune.rune), members.map(_.rune))
//        } else {
//          // If there are no members, we wouldn't really know what type this thing is
//          Array(Array(resultRune.rune))
//        }
      }
      case KindComponentsSR(_, resultRune, mutabilityRune) => Array(Array())
      case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => Array(Array())
      case PrototypeComponentsSR(_, resultRune, nameRune, paramsListRune, returnRune) => Array(Array())
      case OneOfSR(_, rune, literals) => Array(Array())
      case IsConcreteSR(_, rune) => Array(Array(rune.rune))
      case IsInterfaceSR(_, rune) => Array(Array())
      case IsStructSR(_, rune) => Array(Array())
      case CoerceToCoord(_, coordRune, kindRune) => Array(Array())
      case LiteralSR(_, rune, literal) => Array(Array())
      case AugmentSR(_, resultRune, literals, innerRune) => Array(Array())
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(Array(resultRune.rune))
      case ManualSequenceSR(_, resultRune, elements) => Array(Array(resultRune.rune))
//      case CoordListSR(_, resultRune, elements) => Array(Array())
    }
  }

  private def solveRule(
    state: Unit,
    env: INameS => ITemplataType,
    ruleIndex: Int,
    rule: IRulexSR,
    solverState: ISolverStateForRule[IRulexSR, IRuneS, ITemplataType]):
  Result[Map[IRuneS, ITemplataType], Unit] = {
    rule match {
      case KindComponentsSR(range, resultRune, mutabilityRune) => {
        Ok(Map(resultRune.rune -> KindTemplataType, mutabilityRune.rune -> MutabilityTemplataType))
      }
      case CallSR(range, resultRune, templateRune, argRunes) => {
        vassertSome(solverState.getConclusion(templateRune.rune)) match {
          case TemplateTemplataType(paramTypes, returnType) => {
            Ok(argRunes.map(_.rune).zip(paramTypes).toMap)
          }
          case other => vwat(other)
        }
      }
      case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => {
        Ok(
          Map(
            resultRune.rune -> CoordTemplataType,
            ownershipRune.rune -> OwnershipTemplataType,
            permissionRune.rune -> PermissionTemplataType,
            kindRune.rune -> KindTemplataType))
      }
      case PrototypeComponentsSR(_, resultRune, nameRune, paramListRune, returnRune) => {
        Ok(
          Map(
            resultRune.rune -> PrototypeTemplataType,
            nameRune.rune -> StringTemplataType,
            paramListRune.rune -> PackTemplataType(CoordTemplataType),
            returnRune.rune -> CoordTemplataType))
      }
      case OneOfSR(_, resultRune, literals) => {
        val types = literals.map(_.getType()).toSet
        if (types.size > 1) {
          vfail("OneOf rule's possibilities must all be the same type!")
        }
        Ok(Map(resultRune.rune -> types.head))
      }
      case EqualsSR(_, leftRune, rightRune) => {
        solverState.getConclusion(leftRune.rune) match {
          case None => Ok(Map(leftRune.rune -> vassertSome(solverState.getConclusion(rightRune.rune))))
          case Some(left) => Ok(Map(rightRune.rune -> left))
        }
      }
      case IsConcreteSR(_, rune) => {
        Ok(Map(rune.rune -> KindTemplataType))
      }
      case IsInterfaceSR(_, rune) => {
        Ok(Map(rune.rune -> KindTemplataType))
      }
      case IsStructSR(_, rune) => {
        Ok(Map(rune.rune -> KindTemplataType))
      }
      case CoerceToCoord(_, coordRune, kindRune) => {
        Ok(Map(kindRune.rune -> KindTemplataType, coordRune.rune -> CoordTemplataType))
      }
      case LiteralSR(_, rune, literal) => {
        Ok(Map(rune.rune -> literal.getType()))
      }
      case LookupSR(range, rune, name) => {
        (env(name), vassertSome(solverState.getConclusion(rune.rune))) match {
          case (KindTemplataType, CoordTemplataType) =>
          case (TemplateTemplataType(Vector(), KindTemplataType), CoordTemplataType) =>
          case (TemplateTemplataType(Vector(), result), expected) if result == expected =>
          case (from, to) if from == to =>
          case (from, to) => vfail((from, to))
        }
        Ok(Map())
      }
      case RuneParentEnvLookupSR(range, rune) => {
        (env(RuneNameS(rune.rune)), vassertSome(solverState.getConclusion(rune.rune))) match {
          case (KindTemplataType, CoordTemplataType) =>
          case (TemplateTemplataType(Vector(), KindTemplataType), CoordTemplataType) =>
          case (TemplateTemplataType(Vector(), result), expected) if result == expected =>
          case (from, to) if from == to =>
          case (from, to) => vfail((from, to))
        }
        Ok(Map())
      }
      case LookupSR(range, rune, name) => {
        Ok(Map(rune.rune -> KindTemplataType))
      }
      case AugmentSR(_, resultRune, literals, innerRune) => {
        Ok(Map(resultRune.rune -> CoordTemplataType, innerRune.rune -> CoordTemplataType))
      }
      case PackSR(_, resultRune, memberRunes) => {
        Ok(
          memberRunes.map(_.rune -> CoordTemplataType).toMap +
            (resultRune.rune -> PackTemplataType(CoordTemplataType)))
//        solverState.getConclusion(resultRune.rune) match {
//          case Some(PackTemplataType(elementType)) => {
//            Ok(memberRunes.map(memberRune => (memberRune.rune -> elementType)).toMap)
//          }
//          case Some(_) => {
//            vfail("Pack rule's result must be a pack!")
//          }
//          case None => {
//            val memberTypes =
//              memberRunes.map(memberRune => vassertSome(solverState.getConclusion(memberRune.rune)))
//            val distinctMemberTypes = memberTypes.distinct
//            val memberType =
//              if (distinctMemberTypes.size != 1) {
//                vfail("Pack's members must all be the same type! Instead, got: " + memberTypes.mkString(""))
//              } else {
//                distinctMemberTypes.head
//              }
//            Ok(Map(resultRune.rune -> PackTemplataType(memberType)))
//          }
//        }
      }
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
        Ok(
          Map(
            mutabilityRune.rune -> MutabilityTemplataType,
            variabilityRune.rune -> VariabilityTemplataType,
            sizeRune.rune -> IntegerTemplataType,
            elementRune.rune -> CoordTemplataType))
      }
      case ManualSequenceSR(_, resultRune, elements) => vimpl()
//      case CoordListSR(_, resultRune, elements) => vimpl()
    }
  }

  def solve(
    env: INameS => ITemplataType,
    range: RangeS,
    predicting: Boolean,
    rules: IndexedSeq[IRulexSR],
    // Some runes don't appear in the rules, for example if they are in the identifying runes,
    // but not in any of the members or rules.
    additionalRunes: Iterable[IRuneS],
    expectCompleteSolve: Boolean,
    unpreprocessedInitiallyKnownRunes: Map[IRuneS, ITemplataType]):
  Result[Map[IRuneS, ITemplataType], RuneTypeSolveError] = {
    val initiallyKnownRunes =
      unpreprocessedInitiallyKnownRunes ++
        (if (predicting) {
          Map()
        } else {
          // Calculate what types we can beforehand, see KVCIE.
          rules.flatMap({
            case LookupSR(_, rune, name) => {
              env(name) match {
                // We don't know whether we'll interpret this kind as a coord.
                case KindTemplataType => List()
                case TemplateTemplataType(Vector(), KindTemplataType) => List()
                // If it's not a kind, then we'll use it as it is.
                case other => List(rune.rune -> other)
              }
            }
            case _ => List()
          }).toMap
        })
    val solverState =
      Solver.makeInitialSolverState(
        rules, getRunes, (rule: IRulexSR) => getPuzzles(predicting, rule), initiallyKnownRunes)
    val conclusions =
      Solver.solve[IRulexSR, IRuneS, INameS => ITemplataType, Unit, ITemplataType, Unit](
        Unit,
        env,
        solverState,
        solveRule
      ) match {
        case Ok(c) => c.toMap
        case Err(e) => vfail(e)
      }
    val allRunes = solverState.getAllRunes() ++ additionalRunes
    val unsolvedRunes = allRunes -- conclusions.keySet
    if (expectCompleteSolve && unsolvedRunes.nonEmpty) {
      Err(RuneTypeSolveError(range, IncompleteSolve(conclusions, solverState.getUnsolvedRules(), unsolvedRunes)))
    } else {
      Ok(conclusions)
    }
  }
}
