package dev.vale.postparsing

import dev.vale.postparsing.rules._
import dev.vale.solver.{IIncompleteOrFailedSolve, ISolveRule, ISolverError, ISolverState, IStepState, IncompleteSolve, Solver}
import dev.vale.{Err, Ok, RangeS, Result, vassert, vimpl, vpass}
import dev.vale._
import dev.vale.postparsing.rules._

import scala.collection.immutable.Map

case class IdentifiabilitySolveError(range: RangeS, failedSolve: IIncompleteOrFailedSolve[IRulexSR, IRuneS, Boolean, IIdentifiabilityRuleError]) {
  vpass()
}

sealed trait IIdentifiabilityRuleError

object IdentifiabilitySolver {
  def getRunes(rule: IRulexSR): Array[IRuneS] = {
    val sanityCheck =
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
        case CallSiteFuncSR(range, prototypeRune, name, paramsListRune, returnRune) => Array(prototypeRune, paramsListRune, returnRune)
        case DefinitionFuncSR(range, resultRune, name, paramsListRune, returnRune) => Array(resultRune, paramsListRune, returnRune)
        case OneOfSR(range, rune, literals) => Array(rune)
        case IsConcreteSR(range, rune) => Array(rune)
        case IsInterfaceSR(range, rune) => Array(rune)
        case IsStructSR(range, rune) => Array(rune)
        case CoerceToCoordSR(range, coordRune, kindRune) => Array(coordRune, kindRune)
        case LiteralSR(range, rune, literal) => Array(rune)
        case AugmentSR(range, resultRune, ownership, innerRune) => Array(resultRune, innerRune)
        case CallSR(range, resultRune, templateRune, args) => Array(resultRune, templateRune) ++ args
//        case PrototypeSR(range, resultRune, name, parameters, returnTypeRune) => Array(resultRune) ++ parameters ++ Array(returnTypeRune)
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

  def getPuzzles(rule: IRulexSR): Array[Array[IRuneS]] = {
    rule match {
      case EqualsSR(range, leftRune, rightRune) => Array(Array(leftRune.rune), Array(rightRune.rune))
      case LookupSR(range, rune, _) => Array(Array())
      case RuneParentEnvLookupSR(range, rune) => {
        // This Array() literally means nothing can solve this puzzle.
        // It needs to be passed in via identifying rune.
        Array()
      }
      case CallSR(range, resultRune, templateRune, args) => {
        // We can't determine the template from the result and args because we might be coercing its
        // returned kind to a coord. So we need the template.
        // We can't determine the return type because we don't know whether we're coercing or not.
        Array(Array(resultRune.rune, templateRune.rune), Array(templateRune.rune) ++ args.map(_.rune))
      }
      case PackSR(range, resultRune, members) => {
        // Packs are always lists of coords
        Array(Array(resultRune.rune), members.map(_.rune))
      }
      case CoordIsaSR(range, subRune, superRune) => Array(Array())
      case KindComponentsSR(range, resultRune, mutabilityRune) => Array(Array())
      case CoordComponentsSR(range, resultRune, ownershipRune, kindRune) => Array(Array())
      case PrototypeComponentsSR(range, resultRune, ownershipRune, kindRune) => Array(Array())
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
      case StaticSizedArraySR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(Array(resultRune.rune), Array(mutabilityRune.rune, variabilityRune.rune, sizeRune.rune, elementRune.rune))
      case RuntimeSizedArraySR(range, resultRune, mutabilityRune, elementRune) => Array(Array(resultRune.rune), Array(mutabilityRune.rune, elementRune.rune))
//      case ManualSequenceSR(range, resultRune, elements) => Array(Array(resultRune.rune))
      case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => Array(Array())
        // solverState.addPuzzle(ruleIndex, Array(senderRune, receiverRune))
//      case CoordListSR(range, resultRune, elements) => Array(Array())
    }
  }

  private def solveRule(
    state: Unit,
    env: Unit,
    ruleIndex: Int,
    rule: IRulexSR,
    stepState: IStepState[IRulexSR, IRuneS, Boolean]):
  Result[Unit, ISolverError[IRuneS, Boolean, IIdentifiabilityRuleError]] = {
    rule match {
      case KindComponentsSR(range, resultRune, mutabilityRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, mutabilityRune.rune, true)
        Ok(())
      }
      case CoordComponentsSR(range, resultRune, ownershipRune, kindRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, ownershipRune.rune, true)
        stepState.concludeRune(range, kindRune.rune, true)
        Ok(())
      }
      case PrototypeComponentsSR(range, resultRune, paramsRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, paramsRune.rune, true)
        stepState.concludeRune(range, returnRune.rune, true)
        Ok(())
      }
      case CallSR(range, resultRune, templateRune, argRunes) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, templateRune.rune, true)
        argRunes.map(_.rune).foreach({ case argRune =>
          stepState.concludeRune(range, argRune, true)
        })
        Ok(())
      }
      case ResolveSR(range, resultRune, name, paramListRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, paramListRune.rune, true)
        stepState.concludeRune(range, returnRune.rune, true)
        Ok(())
      }
      case CallSiteFuncSR(range, resultRune, name, paramListRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, paramListRune.rune, true)
        stepState.concludeRune(range, returnRune.rune, true)
        Ok(())
      }
      case DefinitionFuncSR(range, resultRune, name, paramsListRune, returnRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, paramsListRune.rune, true)
        stepState.concludeRune(range, returnRune.rune, true)
        Ok(())
      }
      case CoordIsaSR(range, subRune, superRune) => {
        stepState.concludeRune(range, subRune.rune, true)
        stepState.concludeRune(range, superRune.rune, true)
        Ok(())
      }
      case OneOfSR(range, resultRune, literals) => {
        stepState.concludeRune(range, resultRune.rune, true)
        Ok(())
      }
      case EqualsSR(range, leftRune, rightRune) => {
        stepState.concludeRune(range, leftRune.rune, true)
        stepState.concludeRune(range, rightRune.rune, true)
        Ok(())
      }
      case IsConcreteSR(range, rune) => {
        stepState.concludeRune(range, rune.rune, true)
        Ok(())
      }
      case IsInterfaceSR(range, rune) => {
        stepState.concludeRune(range, rune.rune, true)
        Ok(())
      }
      case IsStructSR(range, rune) => {
        stepState.concludeRune(range, rune.rune, true)
        Ok(())
      }
      case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, coordListRune.rune, true)
        Ok(())
      }
      case CoerceToCoordSR(range, coordRune, kindRune) => {
        stepState.concludeRune(range, kindRune.rune, true)
        stepState.concludeRune(range, coordRune.rune, true)
        Ok(())
      }
      case LiteralSR(range, rune, literal) => {
        stepState.concludeRune(range, rune.rune, true)
        Ok(())
      }
      case LookupSR(range, rune, name) => {
        stepState.concludeRune(range, rune.rune, true)
        Ok(())
      }
      case RuneParentEnvLookupSR(range, rune) => {
        vimpl()
//        (env(RuneNameS(rune.rune)), vassertSome(stepState.getConclusion(rune.rune))) match {
//          case (true, true) =>
//          case (TemplateTemplataType(Vector(), true), true) =>
//          case (TemplateTemplataType(Vector(), result), expected) if result == expected =>
//          case (from, to) if from == to =>
//          case (from, to) => {
//            return Err(SolverConflict(rune.rune, to, from))
//          }
//        }
        Ok(())
      }
      case LookupSR(range, rune, name) => {
        stepState.concludeRune(range, rune.rune, true)
        Ok(())
      }
      case AugmentSR(range, resultRune, ownership, innerRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, innerRune.rune, true)
        Ok(())
      }
      case PackSR(range, resultRune, memberRunes) => {
        memberRunes.foreach(x => stepState.concludeRune(range, x.rune, true))
        stepState.concludeRune(range, resultRune.rune, true)
        Ok(())
      }
      case StaticSizedArraySR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, mutabilityRune.rune, true)
        stepState.concludeRune(range, variabilityRune.rune, true)
        stepState.concludeRune(range, sizeRune.rune, true)
        stepState.concludeRune(range, elementRune.rune, true)
        Ok(())
      }
      case RuntimeSizedArraySR(range, resultRune, mutabilityRune, elementRune) => {
        stepState.concludeRune(range, resultRune.rune, true)
        stepState.concludeRune(range, mutabilityRune.rune, true)
        stepState.concludeRune(range, elementRune.rune, true)
        Ok(())
      }
    }
  }

  def solve(
    sanityCheck: Boolean,
    useOptimizedSolver: Boolean,
    interner: Interner,
    range: RangeS,
    rules: IndexedSeq[IRulexSR],
    identifyingRunes: Iterable[IRuneS]):
  Result[Map[IRuneS, Boolean], IdentifiabilitySolveError] = {
    val initiallyKnownRunes = identifyingRunes.map(r => (r, true)).toMap
    val solver =
      new Solver[IRulexSR, IRuneS, Unit, Unit, Boolean, IIdentifiabilityRuleError](
        sanityCheck, useOptimizedSolver, interner)
    val solverState =
      solver
        .makeInitialSolverState(
          rules, getRunes, (rule: IRulexSR) => getPuzzles(rule), initiallyKnownRunes)
    val (steps, conclusions) =
      solver.solve(
        (rule: IRulexSR) => getPuzzles(rule),
        Unit,
        Unit,
        solverState,
        new ISolveRule[IRulexSR, IRuneS, Unit, Unit, Boolean, IIdentifiabilityRuleError] {
          override def complexSolve(state: Unit, env: Unit, solverState: ISolverState[IRulexSR, IRuneS, Boolean], stepState: IStepState[IRulexSR, IRuneS, Boolean]): Result[Unit, ISolverError[IRuneS, Boolean, IIdentifiabilityRuleError]] = {
            Ok(())
          }

          override def solve(state: Unit, env: Unit, solverState: ISolverState[IRulexSR, IRuneS, Boolean], ruleIndex: Int, rule: IRulexSR, stepState: IStepState[IRulexSR, IRuneS, Boolean]): Result[Unit, ISolverError[IRuneS, Boolean, IIdentifiabilityRuleError]] = {
            solveRule(state, env, ruleIndex, rule, stepState)
          }
        }) match {
        case Ok((steps, conclusionsStream)) => (steps.toVector, conclusionsStream.toMap)
        case Err(e) => return Err(IdentifiabilitySolveError(range, e))
      }
    val allRunes = solverState.getAllRunes().map(solverState.getUserRune)
    val unsolvedRunes = allRunes -- conclusions.keySet
    if (unsolvedRunes.nonEmpty) {
      Err(
        IdentifiabilitySolveError(
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
