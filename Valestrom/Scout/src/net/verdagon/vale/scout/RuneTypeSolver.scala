package net.verdagon.vale.scout

import net.verdagon.vale._
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.solver.{ISolverStateForRule, Planner}
import net.verdagon.vale.templar.types._

case class RuneTypeSolveError(unknownRunes: Iterable[IRuneS])
object RuneTypeSolver {
  def getRunes(rule: IRulexSR): Array[IRuneS] = {
    val sanityCheck =
      rule match {
        case LookupSR(range, rune, literal) => Array(rune)
        case KindLookupSR(range, rune, literal) => Array(rune)
        case EqualsSR(range, left, right) => Array(left, right)
        case IsaSR(range, sub, suuper) => Array(sub, suuper)
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
        case CoordListSR(range, resultRune, elements) => Array(resultRune) ++ elements
      }
    val result = rule.runeUsages
    vassert(result.map(_.rune) sameElements sanityCheck.map(_.rune))
    result.map(_.rune)
  }

  def getPuzzles(predicting: Boolean, rule: IRulexSR): Array[Array[IRuneS]] = {
    rule match {
      case EqualsSR(_, leftRune, rightRune) => Array(Array(leftRune.rune), Array(rightRune.rune))
      case LookupSR(_, _, _) => {
        if (predicting) {
          // This Array() literally means nothing can solve this puzzle.
          // It needs to be passed in via plan/solve's initiallyKnownRunes parameter.
          Array()
        } else {
          // This means we can solve this puzzle and dont need anything to do it.
          Array(Array())
        }
      }
      case KindLookupSR(_, _, _) => Array(Array())
      case CallSR(range, resultRune, templateRune, args) => {
        Array(
          Array(resultRune.rune, templateRune.rune),
          Array(templateRune.rune) ++ args.map(_.rune))
          // We can't determine the template from the result and args because we might be coercing its
          // returned kind to a coord.
          // Array(resultRune.rune) ++ args.map(_.rune))
      }
      case PackSR(_, resultRune, members) => {
        if (members.nonEmpty) {
          Array(Array(resultRune.rune), members.map(_.rune))
        } else {
          // If there are no members, we wouldn't really know what type this thing is
          Array(Array(resultRune.rune))
        }
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
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(Array())
      case ManualSequenceSR(_, resultRune, elements) => Array(Array())
      case CoordListSR(_, resultRune, elements) => Array(Array())
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
        solverState.getConclusion(templateRune.rune) match {
          case None => {
            // We can't determine the template from the result and args because we might be coercing its
            // returned kind to a coord.
            //val result =
            //  vassertSome(getConclusion(resultRune.rune)) match {
            //    case KindTemplataType => CoordTemplataType
            //    case other => other
            //  }
            //val args = argRunes.map(argRune => vassertSome(getConclusion(argRune.rune))).toVector
            //concludeRune(templateRune.rune, TemplateTemplataType(args, result))
            vwat()
          }
          case Some(templateType) => {
            templateType match {
              case TemplateTemplataType(paramTypes, returnType) => {
                val effectiveReturnType =
                  returnType match {
                    case KindTemplataType => CoordTemplataType
                    case other => other
                  }
                Ok(argRunes.map(_.rune).zip(paramTypes).toMap ++ Map(resultRune.rune -> effectiveReturnType))
              }
              case _ => vimpl(); Err(())
            }
          }
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
        val actualType =
          env(name) match {
            case KindTemplataType => CoordTemplataType
            case TemplateTemplataType(Vector(), KindTemplataType) => CoordTemplataType
            case TemplateTemplataType(Vector(), CoordTemplataType) => CoordTemplataType
            case other => other
          }
        Ok(Map(rune.rune -> actualType))
      }
      case KindLookupSR(range, rune, name) => {
        Ok(Map(rune.rune -> KindTemplataType))
      }
      case AugmentSR(_, resultRune, literals, innerRune) => {
        Ok(Map(resultRune.rune -> CoordTemplataType, innerRune.rune -> CoordTemplataType))
      }
      case PackSR(_, resultRune, memberRunes) => {
        solverState.getConclusion(resultRune.rune) match {
          case Some(PackTemplataType(elementType)) => {
            Ok(memberRunes.map(memberRune => (memberRune.rune -> elementType)).toMap)
          }
          case Some(_) => {
            vfail("Pack rule's result must be a pack!")
          }
          case None => {
            val memberTypes =
              memberRunes.map(memberRune => vassertSome(solverState.getConclusion(memberRune.rune)))
            val distinctMemberTypes = memberTypes.distinct
            val memberType =
              if (distinctMemberTypes.size != 1) {
                vfail("Pack's members must all be the same type! Instead, got: " + memberTypes.mkString(""))
              } else {
                distinctMemberTypes.head
              }
            Ok(Map(resultRune.rune -> PackTemplataType(memberType)))
          }
        }
      }
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
        Ok(
          Map(
            resultRune.rune -> CoordTemplataType,
            mutabilityRune.rune -> MutabilityTemplataType,
            variabilityRune.rune -> VariabilityTemplataType,
            sizeRune.rune -> IntegerTemplataType,
            elementRune.rune -> CoordTemplataType))
      }
      case ManualSequenceSR(_, resultRune, elements) => vimpl()
      case CoordListSR(_, resultRune, elements) => vimpl()
    }
  }

  def solve(
    env: INameS => ITemplataType,
    predicting: Boolean,
    rules: IndexedSeq[IRulexSR],
    // Some runes don't appear in the rules, for example if they are in the identifying runes,
    // but not in any of the members or rules.
    additionalRunes: Iterable[IRuneS],
    expectCompleteSolve: Boolean,
    initiallyKnownRunes: Map[IRuneS, ITemplataType]):
  Result[Map[IRuneS, ITemplataType], RuneTypeSolveError] = {
    val solverState =
      Planner.makeInitialSolverState(
        rules, getRunes, (rule: IRulexSR) => getPuzzles(predicting, rule), initiallyKnownRunes)
    val conclusions =
      Planner.solve[IRulexSR, IRuneS, INameS => ITemplataType, Unit, ITemplataType, Unit](
        Unit,
        env,
        solverState,
        solveRule
      ) match {
        case Ok(c) => c.toMap
        case Err(e) => vfail(e)
      }
    if (expectCompleteSolve && (conclusions.keySet != solverState.getAllRunes())) {
      Err(RuneTypeSolveError(solverState.getAllRunes() -- conclusions.keySet))
    } else {
      Ok(conclusions)
    }
  }
}
