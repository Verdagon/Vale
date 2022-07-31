package dev.vale.postparsing

import dev.vale.{Err, Interner, Ok, RangeS, Result, vassert, vassertSome, vfail, vpass, vwat}
import dev.vale.postparsing.rules._
import dev.vale.solver.{IIncompleteOrFailedSolve, ISolveRule, ISolverError, ISolverState, IStepState, IncompleteSolve, Solver, SolverConflict}
import dev.vale._
import dev.vale.postparsing.rules._

import scala.collection.immutable.Map

case class RuneTypeSolveError(range: RangeS, failedSolve: IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplataType, IRuneTypeRuleError]) {
  vpass()
}

sealed trait IRuneTypeRuleError
case class LookupDidntMatchExpectedType(range: RangeS, expectedType: ITemplataType, actualType: ITemplataType) extends IRuneTypeRuleError

class RuneTypeSolver(interner: Interner) {
  def getRunes(rule: IRulexSR): Array[IRuneS] = {
    val sanityCheck: Array[RuneUsage] =
      rule match {
        case LookupSR(range, rune, literal) => Array(rune)
        case RuneParentEnvLookupSR(range, rune) => Array(rune)
        case EqualsSR(range, left, right) => Array(left, right)
        case CoordIsaSR(range, sub, suuper) => Array(sub, suuper)
        case KindIsaSR(range, sub, suuper) => Array(sub, suuper)
        case KindComponentsSR(range, resultRune, mutabilityRune) => Array(resultRune, mutabilityRune)
        case CoordComponentsSR(range, resultRune, ownershipRune, kindRune) => Array(resultRune, ownershipRune, kindRune)
        case PrototypeComponentsSR(range, resultRune, paramsRune, returnRune) => Array(resultRune, paramsRune, returnRune)
        case ResolveSR(range, resultRune, name, paramsListRune, returnRune) => Array(resultRune, paramsListRune, returnRune)
        case CallSiteFuncSR(range, resultRune, name, paramsListRune, returnRune) => Array(resultRune, paramsListRune, returnRune)
        case DefinitionFuncSR(range, resultRune, name, paramsListRune, returnRune) => Array(resultRune, paramsListRune, returnRune)
        case OneOfSR(range, rune, literals) => Array(rune)
        case IsConcreteSR(range, rune) => Array(rune)
        case IsInterfaceSR(range, rune) => Array(rune)
        case IsStructSR(range, rune) => Array(rune)
        case CoerceToCoordSR(range, coordRune, kindRune) => Array(coordRune, kindRune)
        case LiteralSR(range, rune, literal) => Array(rune)
        case AugmentSR(range, resultRune, ownership, innerRune) => Array(resultRune, innerRune)
        case CallSR(range, resultRune, templateRune, args) => Array(resultRune, templateRune) ++ args
//        case PrototypeSR(range, resultRune, name, parameters, returnTypeRune) => Array(resultRune, returnTypeRune) ++ parameters
        case PackSR(range, resultRune, members) => Array(resultRune) ++ members
        case StaticSizedArraySR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune)
        case RuntimeSizedArraySR(range, resultRune, mutabilityRune, elementRune) => Array(resultRune, mutabilityRune, elementRune)
//        case ManualSequenceSR(range, resultRune, elements) => Array(resultRune) ++ elements
        case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => Array(resultRune, coordListRune)
//        case CoordListSR(range, resultRune, elements) => Array(resultRune) ++ elements
      }
    val result = rule.runeUsages
    vassert(result.map(_.rune) sameElements sanityCheck.map(_.rune))
    result.map(_.rune)
  }

  def getPuzzles(predicting: Boolean, rule: IRulexSR): Array[Array[IRuneS]] = {
    rule match {
      case EqualsSR(range, leftRune, rightRune) => Array(Array(leftRune.rune), Array(rightRune.rune))
      case LookupSR(range, rune, _) => {
        if (predicting) {
          // This Array() literally means nothing can solve this puzzle.
          // It needs to be passed in via plan/solve's initiallyKnownRunes parameter.
          Array()
        } else {
          // We need to know the type beforehand, because we don't know if we'll be coercing or not.
          Array(Array(rune.rune))
        }
      }
      case RuneParentEnvLookupSR(range, rune) => {
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
      case PackSR(range, resultRune, members) => {
        // Packs are always lists of coords
        Array(Array())
      }
      case CoordIsaSR(range, subRune, superRune) => Array(Array())
      case KindComponentsSR(range, resultRune, mutabilityRune) => Array(Array())
      case CoordComponentsSR(range, resultRune, ownershipRune, kindRune) => Array(Array())
      case PrototypeComponentsSR(range, resultRune, paramsRune, returnRune) => Array(Array())
      case ResolveSR(range, resultRune, nameRune, paramsListRune, returnRune) => Array(Array())
      case CallSiteFuncSR(range, resultRune, nameRune, paramsListRune, returnRune) => Array(Array())
      case DefinitionFuncSR(range, resultRune, name, paramsListRune, returnRune) => Array(Array())
      case OneOfSR(range, rune, literals) => Array(Array())
      case IsConcreteSR(range, rune) => Array(Array(rune.rune))
      case IsInterfaceSR(range, rune) => Array(Array())
      case IsStructSR(range, rune) => Array(Array())
      case CoerceToCoordSR(range, coordRune, kindRune) => Array(Array())
      case LiteralSR(range, rune, literal) => Array(Array())
      case AugmentSR(range, resultRune, ownership, innerRune) => Array(Array())
      case StaticSizedArraySR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(Array(resultRune.rune))
      case RuntimeSizedArraySR(range, resultRune, mutabilityRune, elementRune) => Array(Array(resultRune.rune))
//      case ManualSequenceSR(range, resultRune, elements) => Array(Array(resultRune.rune))
      case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => Array(Array())
        // solverState.addPuzzle(ruleIndex, Array(senderRune, receiverRune))
//      case CoordListSR(range, resultRune, elements) => Array(Array())
    }
  }

  private def solveRule(
    state: Unit,
    env: IImpreciseNameS => ITemplataType,
    ruleIndex: Int,
    rule: IRulexSR,
    stepState: IStepState[IRulexSR, IRuneS, ITemplataType]):
  Result[Unit, ISolverError[IRuneS, ITemplataType, IRuneTypeRuleError]] = {
    rule match {
      case KindComponentsSR(range, resultRune, mutabilityRune) => {
        stepState.concludeRune(range, resultRune.rune, KindTemplataType())
        stepState.concludeRune(range, mutabilityRune.rune, MutabilityTemplataType())
        Ok(())
      }
      case CoordComponentsSR(range, resultRune, ownershipRune, kindRune) => {
        stepState.concludeRune(range, resultRune.rune, CoordTemplataType())
        stepState.concludeRune(range, ownershipRune.rune, OwnershipTemplataType())
        stepState.concludeRune(range, kindRune.rune, KindTemplataType())
        Ok(())
      }
      case PrototypeComponentsSR(range, resultRune, paramsRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, PrototypeTemplataType())
        stepState.concludeRune(range, paramsRune.rune, PackTemplataType(CoordTemplataType()))
        stepState.concludeRune(range, returnRune.rune, CoordTemplataType())
        Ok(())
      }
      case CallSR(range, resultRune, templateRune, argRunes) => {
        vassertSome(stepState.getConclusion(templateRune.rune)) match {
          case TemplateTemplataType(paramTypes, returnType) => {
            argRunes.map(_.rune).zip(paramTypes).foreach({ case (argRune, paramType) =>
              stepState.concludeRune(range, argRune, paramType)
            })
            Ok(())
          }
          case other => vwat(other)
        }
      }
      case ResolveSR(range, resultRune, name, paramListRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, PrototypeTemplataType())
        stepState.concludeRune(range, paramListRune.rune, PackTemplataType(CoordTemplataType()))
        stepState.concludeRune(range, returnRune.rune, CoordTemplataType())
        Ok(())
      }
      case CallSiteFuncSR(range, resultRune, name, paramListRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, PrototypeTemplataType())
        stepState.concludeRune(range, paramListRune.rune, PackTemplataType(CoordTemplataType()))
        stepState.concludeRune(range, returnRune.rune, CoordTemplataType())
        Ok(())
      }
      case DefinitionFuncSR(range, resultRune, name, paramListRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, PrototypeTemplataType())
        stepState.concludeRune(range, paramListRune.rune, PackTemplataType(CoordTemplataType()))
        stepState.concludeRune(range, returnRune.rune, CoordTemplataType())
        Ok(())
      }
      case CoordIsaSR(range, subRune, superRune) => {
        stepState.concludeRune(range, subRune.rune, CoordTemplataType())
        stepState.concludeRune(range, superRune.rune, CoordTemplataType())
        Ok(())
      }
      case OneOfSR(range, resultRune, literals) => {
        val types = literals.map(_.getType()).toSet
        if (types.size > 1) {
          vfail("OneOf rule's possibilities must all be the same type!")
        }
        stepState.concludeRune(range, resultRune.rune, types.head)
        Ok(())
      }
      case EqualsSR(range, leftRune, rightRune) => {
        stepState.getConclusion(leftRune.rune) match {
          case None => {
            stepState.concludeRune(range, leftRune.rune, vassertSome(stepState.getConclusion(rightRune.rune)))
            Ok(())
          }
          case Some(left) => {
            stepState.concludeRune(range, rightRune.rune, left)
            Ok(())
          }
        }
      }
      case IsConcreteSR(range, rune) => {
        stepState.concludeRune(range, rune.rune, KindTemplataType())
        Ok(())
      }
      case IsInterfaceSR(range, rune) => {
        stepState.concludeRune(range, rune.rune, KindTemplataType())
        Ok(())
      }
      case IsStructSR(range, rune) => {
        stepState.concludeRune(range, rune.rune, KindTemplataType())
        Ok(())
      }
      case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => {
        stepState.concludeRune(range, resultRune.rune, MutabilityTemplataType())
        stepState.concludeRune(range, coordListRune.rune, PackTemplataType(CoordTemplataType()))
        Ok(())
      }
      case CoerceToCoordSR(range, coordRune, kindRune) => {
        stepState.concludeRune(range, kindRune.rune, KindTemplataType())
        stepState.concludeRune(range, coordRune.rune, CoordTemplataType())
        Ok(())
      }
      case LiteralSR(range, rune, literal) => {
        stepState.concludeRune(range, rune.rune, literal.getType())
        Ok(())
      }
      case LookupSR(range, rune, name) => {
        (env(name), vassertSome(stepState.getConclusion(rune.rune))) match {
          case (KindTemplataType(), CoordTemplataType()) =>
          case (TemplateTemplataType(Vector(), KindTemplataType()), CoordTemplataType()) =>
          case (TemplateTemplataType(Vector(), result), expected) if result == expected =>
          case (from, to) if from == to =>
          case (from, to) => {
            return Err(SolverConflict(rune.rune, to, from))
          }
        }
        Ok(())
      }
      case RuneParentEnvLookupSR(range, rune) => {
        (env(interner.intern(RuneNameS(rune.rune))), vassertSome(stepState.getConclusion(rune.rune))) match {
          case (KindTemplataType(), CoordTemplataType()) =>
          case (TemplateTemplataType(Vector(), KindTemplataType()), CoordTemplataType()) =>
          case (TemplateTemplataType(Vector(), result), expected) if result == expected =>
          case (from, to) if from == to =>
          case (from, to) => {
            return Err(SolverConflict(rune.rune, to, from))
          }
        }
        Ok(())
      }
      case LookupSR(range, rune, name) => {
        stepState.concludeRune(range, rune.rune, KindTemplataType())
        Ok(())
      }
      case AugmentSR(range, resultRune, ownership, innerRune) => {
        stepState.concludeRune(range, resultRune.rune, CoordTemplataType())
        stepState.concludeRune(range, innerRune.rune, CoordTemplataType())
        Ok(())
      }
      case PackSR(range, resultRune, memberRunes) => {
        memberRunes.foreach(x => stepState.concludeRune(range, x.rune, CoordTemplataType()))
        stepState.concludeRune(range, resultRune.rune, PackTemplataType(CoordTemplataType()))
        Ok(())
      }
      case StaticSizedArraySR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
        stepState.concludeRune(range, mutabilityRune.rune, MutabilityTemplataType())
        stepState.concludeRune(range, variabilityRune.rune, VariabilityTemplataType())
        stepState.concludeRune(range, sizeRune.rune, IntegerTemplataType())
        stepState.concludeRune(range, elementRune.rune, CoordTemplataType())
        Ok(())
      }
      case RuntimeSizedArraySR(range, resultRune, mutabilityRune, elementRune) => {
        stepState.concludeRune(range, mutabilityRune.rune, MutabilityTemplataType())
        stepState.concludeRune(range, elementRune.rune, CoordTemplataType())
        Ok(())
      }
    }
  }

  def solve(
    sanityCheck: Boolean,
    useOptimizedSolver: Boolean,
    env: IImpreciseNameS => ITemplataType,
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
            case LookupSR(range, rune, name) => {
              env(name) match {
                // We don't know whether we'll interpret this kind as a coord.
                case KindTemplataType() => List()
                case TemplateTemplataType(Vector(), KindTemplataType()) => List()
                // If it's not a kind, then we'll use it as it is.
                case other => List(rune.rune -> other)
              }
            }
            case _ => List()
          }).toMap
        })
    val solver =
      new Solver[IRulexSR, IRuneS, IImpreciseNameS => ITemplataType, Unit, ITemplataType, IRuneTypeRuleError](
        sanityCheck, useOptimizedSolver, interner)
    val solverState =
      solver.makeInitialSolverState(
        rules, getRunes, (rule: IRulexSR) => getPuzzles(predicting, rule), initiallyKnownRunes)
    val (steps, conclusions) =
      solver.solve(
        (rule: IRulexSR) => getPuzzles(predicting, rule),
        Unit,
        env,
        solverState,
        new ISolveRule[IRulexSR, IRuneS, IImpreciseNameS => ITemplataType, Unit, ITemplataType, IRuneTypeRuleError] {
          override def sanityCheckConclusion(env: IImpreciseNameS => ITemplataType, state: Unit, rune: IRuneS, conclusion: ITemplataType): Unit = {}

          override def complexSolve(state: Unit, env: IImpreciseNameS => ITemplataType, solverState: ISolverState[IRulexSR, IRuneS, ITemplataType], stepState: IStepState[IRulexSR, IRuneS, ITemplataType]): Result[Unit, ISolverError[IRuneS, ITemplataType, IRuneTypeRuleError]] = {
            Ok(())
          }

          override def solve(state: Unit, env: IImpreciseNameS => ITemplataType, solverState: ISolverState[IRulexSR, IRuneS, ITemplataType], ruleIndex: Int, rule: IRulexSR, stepState: IStepState[IRulexSR, IRuneS, ITemplataType]): Result[Unit, ISolverError[IRuneS, ITemplataType, IRuneTypeRuleError]] = {
            solveRule(state, env, ruleIndex, rule, stepState)
          }
        }) match {
        case Ok((steps, conclusionsStream)) => (steps, conclusionsStream.toMap)
        case Err(e) => return Err(RuneTypeSolveError(range, e))
      }
    val allRunes = solverState.getAllRunes().map(solverState.getUserRune) ++ additionalRunes
    val unsolvedRunes = allRunes -- conclusions.keySet
    if (expectCompleteSolve && unsolvedRunes.nonEmpty) {
      Err(
        RuneTypeSolveError(
          range,
          IncompleteSolve(
            steps,
            solverState.getUnsolvedRules(),
            unsolvedRunes,
            conclusions)))
    } else {
      Ok(conclusions)
    }
  }
}
