package net.verdagon.vale.astronomer.ruletyper

import net.verdagon.vale.astronomer.{INameA, _}
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.{Err, Ok, Result, vassert, vcurious, vfail, vimpl}

import scala.collection.immutable.List
import scala.collection.mutable
import scala.reflect.NameTransformer

trait IRuleTyperEvaluatorDelegate[Env, State, Conclusion, ErrType] {
  def solve(state: State, env: Env, range: RangeS, rule: IRulexAR, runes: Map[Int, Conclusion]): Result[Map[Int, Conclusion], ErrType]
}

// Given enough user specified template params and param inputs, we should be able to
// infer everything.
// This class's purpose is to take those things, and see if it can figure out as many
// inferences as possible.

class RuleTyperEvaluator[Env, State, Conclusion, ErrType](
  delegate: IRuleTyperEvaluatorDelegate[Env, State, Conclusion, ErrType]) {

  def solve(
    state: State,
    env: Env,
    originalSolverState: RuneWorldSolverState,
    invocationRange: RangeS
  ): Result[Array[Option[Conclusion]], ErrType] = {
    val runeWorld = originalSolverState.runeWorld
    val solverState = originalSolverState.deepClone()

    val conclusions: Array[Option[Conclusion]] = runeWorld.runeToPuzzles.map(_ => None)

    while (solverState.numUnknownsToNumPuzzles(0) > 0) {
      vassert(solverState.numUnknownsToPuzzle(0)(0) >= 0)

      val numSolvableRules = solverState.numUnknownsToNumPuzzles(0)

      val solvingPuzzle = solverState.numUnknownsToPuzzle(0)(numSolvableRules - 1)
      vassert(solvingPuzzle >= 0)
      vassert(solverState.puzzleToIndexInNumUnknowns(solvingPuzzle) == numSolvableRules - 1)


      val solvedPuzzleRunes = runeWorld.puzzleToRunes(solvingPuzzle)
      val puzzleRuneToTemplata =
        solvedPuzzleRunes.map(rune => {
          (rune -> conclusions(rune).get)
        }).toMap

      val solvingRule = runeWorld.puzzleToRule(solvingPuzzle)

      val newlySolvedRuneToTemplata =
        delegate.solve(state, env, invocationRange, runeWorld.rules(solvingRule), puzzleRuneToTemplata) match {
          case Ok(x) => x
          case Err(e) => return Err(e)
        }

      newlySolvedRuneToTemplata.foreach({ case (newlySolvedRune, templata) =>
        conclusions(newlySolvedRune) = Some(templata)
        markRuneSolved(solverState, conclusions, newlySolvedRune)
      })

      markRuleSolved(solverState, solvingRule)

//      runeWorld.ruleToPuzzles(solvingRule).foreach(puzzleForThisSolvingRule => {
//        markPuzzleSolved(solverState, puzzleForThisSolvingRule)
//      })
    }

    Ok(conclusions)
  }

  def markRuneSolved(
      solverState: RuneWorldSolverState,
      conclusions: Array[Option[Conclusion]],
      rune: Int) = {
    val runeWorld = solverState.runeWorld
    val puzzlesWithNewlySolvedRune = runeWorld.runeToPuzzles(rune)

    puzzlesWithNewlySolvedRune.foreach(puzzle => {
      val puzzleRunes = runeWorld.puzzleToRunes(puzzle)
      vassert(puzzleRunes.contains(rune))

      val oldNumUnknownRunes = solverState.puzzleToNumUnknownRunes(puzzle)
      val newNumUnknownRunes = oldNumUnknownRunes - 1
      // == newNumUnknownRunes because we already registered it as a conclusion
      vassert(puzzleRunes.count(conclusions(_).isEmpty) == newNumUnknownRunes)
      solverState.puzzleToNumUnknownRunes(puzzle) = newNumUnknownRunes

      val puzzleUnknownRunes = solverState.puzzleToUnknownRunes(puzzle)

      // Should be O(5), no rule has more than 5 unknowns
      val indexOfUnknownRune = puzzleUnknownRunes.indexOf(rune)
      vassert(indexOfUnknownRune >= 0)
      // Swap the last thing into this one's place
      puzzleUnknownRunes(indexOfUnknownRune) = puzzleUnknownRunes(newNumUnknownRunes)
      // This is unnecessary, but might make debugging easier
      puzzleUnknownRunes(newNumUnknownRunes) = -1

      vassert(
        puzzleUnknownRunes.slice(0, newNumUnknownRunes).distinct.sorted sameElements
          puzzleRunes.filter(conclusions(_).isEmpty).distinct.sorted)

      val oldNumUnknownsBucket = solverState.numUnknownsToPuzzle(oldNumUnknownRunes)

      val oldNumUnknownsBucketOldSize = solverState.numUnknownsToNumPuzzles(oldNumUnknownRunes)
      vassert(oldNumUnknownsBucketOldSize == oldNumUnknownsBucket.count(_ >= 0))
      val oldNumUnknownsBucketNewSize = oldNumUnknownsBucketOldSize - 1
      solverState.numUnknownsToNumPuzzles(oldNumUnknownRunes) = oldNumUnknownsBucketNewSize

      val indexOfPuzzleInOldNumUnknownsBucket = solverState.puzzleToIndexInNumUnknowns(puzzle)
      vassert(indexOfPuzzleInOldNumUnknownsBucket == oldNumUnknownsBucket.indexOf(puzzle))

      // Swap the last thing into this one's place
      oldNumUnknownsBucket(indexOfPuzzleInOldNumUnknownsBucket) = oldNumUnknownsBucket(oldNumUnknownsBucketNewSize)
      // This is unnecessary, but might make debugging easier
      oldNumUnknownsBucket(oldNumUnknownsBucketNewSize) = -1

      val newNumUnknownsBucketOldSize = solverState.numUnknownsToNumPuzzles(newNumUnknownRunes)
      val newNumUnknownsBucketNewSize = newNumUnknownsBucketOldSize + 1
      solverState.numUnknownsToNumPuzzles(newNumUnknownRunes) = newNumUnknownsBucketNewSize

      val newNumUnknownsBucket = solverState.numUnknownsToPuzzle(newNumUnknownRunes)
      vassert(newNumUnknownsBucket(newNumUnknownsBucketOldSize) == -1)
      val indexOfPuzzleInNewNumUnknownsBucket = newNumUnknownsBucketOldSize
      newNumUnknownsBucket(indexOfPuzzleInNewNumUnknownsBucket) = puzzle

      solverState.puzzleToIndexInNumUnknowns(puzzle) = indexOfPuzzleInNewNumUnknownsBucket
    })
  }

  def markRuleSolved(
    solverState: RuneWorldSolverState,
    rule: Int) = {
    val puzzlesForRule = solverState.runeWorld.ruleToPuzzles(rule)
    puzzlesForRule.foreach(puzzle => {
      val numUnknowns = solverState.puzzleToNumUnknownRunes(puzzle)
      vassert(numUnknowns == 0)
      solverState.puzzleToNumUnknownRunes(puzzle) = -1
      val indexInNumUnknowns = solverState.puzzleToIndexInNumUnknowns(puzzle)
      solverState.puzzleToIndexInNumUnknowns(puzzle) = -1

      val oldNumPuzzlesInNumUnknownsBucket = solverState.numUnknownsToNumPuzzles(0)
      val lastSlotInNumUnknownsBucket = oldNumPuzzlesInNumUnknownsBucket - 1
      // Swap the last one into this spot
      val newPuzzleForThisSpot = solverState.numUnknownsToPuzzle(0)(lastSlotInNumUnknownsBucket)
      solverState.numUnknownsToPuzzle(0)(indexInNumUnknowns) = newPuzzleForThisSpot
      // Clear the last slot to -1
      solverState.numUnknownsToPuzzle(0)(lastSlotInNumUnknownsBucket) = -1

      // We just moved something in the numUnknownsToPuzzle, so we have to update that thing's knowledge of
      // where it is in the list.
      solverState.puzzleToIndexInNumUnknowns(newPuzzleForThisSpot) = indexInNumUnknowns

      // Reduce the number of puzzles in that bucket by 1
      val newNumPuzzlesInNumUnknownsBucket = oldNumPuzzlesInNumUnknownsBucket - 1
      solverState.numUnknownsToNumPuzzles(0) = newNumPuzzlesInNumUnknownsBucket
    })
  }

//
//  def solve(
//    state: State,
//    env: Env,
//    rules: Vector[IRulexSR],
//    invocationRange: RangeS,
//    paramAtoms: Vector[AtomSP],
//    maybeNeededRunes: Option[Set[IRuneA]]
//  ): (Conclusions, IRuleTyperSolveResult[Vector[IRulexAR]]) = {
//    // First, we feed into the system the things the user already specified.
//
//    // This used to be a parameter for some reason... could move it back if needed.
//    val conclusions = ConclusionsBox(Conclusions(Map()))
//
//    // Now we'll try solving a bunch, just to see if there's any contradictions,
//    // and if so bail out early.
//    solveUntilSettled(state, env, invocationRange, rules, conclusions) match {
//        case (isc @ RuleTyperSolveFailure(_, _, _, _)) => return (conclusions.conclusions, RuleTyperSolveFailure(conclusions, invocationRange, "Failed during pre-solving!", Vector(isc)))
//        case (RuleTyperSolveSuccess(_)) =>
//      }
//
//    // Now we have template args the user specified, and we know there's no contradictions yet.
//
//    // Next, we'll feed in the arguments that they used in the call.
//
//    paramAtoms.zipWithIndex.foreach({
//        case ((paramAtom, paramIndex)) => {
//          makeMatcher().matchAgainstAtomSP(state, env, conclusions, paramAtom) match {
//            case (isc @ RuleTyperMatchConflict(_, _, _, _)) => return (conclusions.conclusions, RuleTyperSolveFailure(conclusions, invocationRange, "Failed solving types for param " + paramIndex, Vector(isc)))
//            case (RuleTyperMatchSuccess(_)) =>
//          }
//        }
//      })
//
//    val listOfMaybeRuleTypes =
//      solveUntilSettled(state, env, invocationRange, rules, conclusions) match {
//        case (isc @ RuleTyperSolveFailure(_, _, _, _)) => return (conclusions.conclusions, RuleTyperSolveFailure(conclusions, invocationRange, "Failed to solve!", Vector(isc)))
//        case (RuleTyperSolveSuccess(rt)) => (rt)
//      }
//
//    // No need to do one last match, because we just did an entire iteration where nothing changed.
//
//    val knowns = listOfMaybeRuleTypes.collect({ case Some(x) => x })
//    if (knowns.size != listOfMaybeRuleTypes.size) {
//      val unknownIndices =
//        listOfMaybeRuleTypes.zipWithIndex.filter(_._1.isEmpty).map(_._2)
//      return (
//        conclusions.conclusions,
//        RuleTyperSolveFailure(
//          conclusions,
//          invocationRange,
//          "Couldn't figure out types of all rules! Couldn't figure out rules at indices: " + unknownIndices,
//          Vector.empty))
//    }
//
//    val unfiguredOutRunes = maybeNeededRunes.getOrElse(Set()) -- conclusions.typeByRune.keySet
//    if (unfiguredOutRunes.nonEmpty) {
//      return (
//        conclusions.conclusions,
//        RuleTyperSolveFailure(
//          conclusions,
//          invocationRange,
//          "Couldn't figure out types of all runes! Couldn't figure out: " + unfiguredOutRunes,
//          Vector.empty))
//    }
//
//    (conclusions.conclusions, RuleTyperSolveSuccess(knowns))
//  }
//
//  private def solveUntilSettled(
//    state: State,
//    env: Env,
//    invocationRange: RangeS,
//    rules: Vector[IRulexSR],
//    conclusions: ConclusionsBox,
//  ): (IRuleTyperSolveResult[Vector[Option[IRulexAR]]]) = {
//    val results =
//      rules.map(rule => {
//        evaluateRule(state, env, conclusions, rule) match {
//          case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperSolveFailure(conclusions, invocationRange, "", Vector(iec)))
//          case RuleTyperEvaluateUnknown() => None
//          case (RuleTyperEvaluateSuccess(result)) => Some(result)
//        }
//      })
//
//    if (conclusions != conclusions) {
//      // Things have not settled, we made some sort of progress in this last iteration.
//      // Keep going.
//      solveUntilSettled(state, env, invocationRange, rules, conclusions)
//    } else {
//      (RuleTyperSolveSuccess(results))
//    }
//  }
//
//  def evaluateRule(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    rule: IRulexSR,
//  ): (IRuleTyperEvaluateResult[IRulexAR]) = {
//    rule match {
//      case r @ IsaSR(_, _, _) => evaluateIsaRule(state, env, conclusions, r)
//      case r @ EqualsSR(_, _, _) => evaluateEqualsRule(state, env, conclusions, r)
//      case r @ OrSR(_, _) => evaluateOrRule(state, env, conclusions, r)
//      case r @ ComponentsSR(_, _, _) => evaluateComponentsRule(state, env, conclusions, r)
//      case r @ TypedSR(_, _, _) => evaluateTypedRule(state, env, conclusions, r)
//      case templexS => {
//        evaluateTemplex(state, env, conclusions, templexS) match {
//          case (rtec @ RuleTyperEvaluateConflict(_, _, _, _)) => (RuleTyperEvaluateConflict(conclusions.conclusions, templexS.range, "", Some(rtec)))
//          case RuleTyperEvaluateUnknown() => RuleTyperEvaluateUnknown()
//          case (RuleTyperEvaluateSuccess(templexT)) => {
//            (RuleTyperEvaluateSuccess(TemplexAR(templexT)))
//          }
//        }
//      }
//      case r @ CallSR(_, _, _) => evaluateRuleCall(state, env, conclusions, r)
//      case other => vfail(other.toString)
//    }
//  }
//
//  def evaluateRules(
//    state: State,
//    env: Env,
//    invocationRange: RangeS,
//    conclusions: ConclusionsBox,
//    rules: Vector[IRulexSR],
//  ): (IRuleTyperEvaluateResult[Vector[IRulexAR]]) = {
//    val initialResult: IRuleTyperEvaluateResult[Vector[IRulexAR]] =
//      RuleTyperEvaluateSuccess(Vector.empty)
//    rules.zipWithIndex.foldLeft((initialResult))({
//      case (RuleTyperEvaluateUnknown(), (rule, index)) => {
//        evaluateRule(state, env, conclusions, rule) match {
//          case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => {
//            return (RuleTyperEvaluateConflict(conclusions.conclusions, rule.range, "Failed evaluating rule index " + index, Some(iec)))
//          }
//          case RuleTyperEvaluateUnknown() => {
//            RuleTyperEvaluateUnknown()
//          }
//          case (RuleTyperEvaluateSuccess(result)) => {
//            // Throw it away; since one is unknown theyre all unknown
//            val _ = result
//            RuleTyperEvaluateUnknown()
//          }
//        }
//      }
//      case ((RuleTyperEvaluateSuccess(previousResults)), (rule, index)) => {
//        evaluateRule(state, env, conclusions, rule) match {
//          case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => {
//            return (RuleTyperEvaluateConflict(conclusions.conclusions, invocationRange, "Failed evaluating rule index " + index, Some(iec)))
//          }
//          case RuleTyperEvaluateUnknown() => {
//            RuleTyperEvaluateUnknown()
//          }
//          case (RuleTyperEvaluateSuccess(result)) => {
//            (RuleTyperEvaluateSuccess(previousResults :+ result))
//          }
//        }
//      }
//    })
//  }
//
//  def evaluateRuleCall(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    ruleCall: CallSR,
//  ): (IRuleTyperEvaluateResult[CallAR]) = {
//    val CallSR(range, name, argumentRules) = ruleCall
//
//    name match {
////      case "ownership" => {
////        val Vector(CoordTemplata(coord)) = argTemplatas
////        (RuleTyperEvaluateSuccess(OwnershipTemplata(coord.ownership)))
////      }
////      case "mutability" => {
////        val Vector(KindTemplata(kind)) = argTemplatas
////        val mutability = delegate.getMutability(kind)
////        (RuleTyperEvaluateSuccess(MutabilityTemplata(mutability)))
////      }
//      case "toRef" => {
//        if (argumentRules.size != 1) {
//          return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "toRef expects 1 argument, but received " + argumentRules.size, None))
//        }
//        val Vector(kindRule) = argumentRules
//        makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, KindTemplataType, kindRule) match {
//          case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in toRef argument!", Some(rtmc)))
//          case (RuleTyperMatchSuccess(kindRuleT)) => {
//            val ruleT = CallAR(range, name, Vector(kindRuleT), CoordTemplataType)
//            (RuleTyperEvaluateSuccess(ruleT))
//          }
//        }
//      }
//      case "passThroughIfInterface" => {
//        if (argumentRules.size != 1) {
//          return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "passThroughIfInterface expects 1 argument, but received " + argumentRules.size, None))
//        }
//        val Vector(kindRule) = argumentRules
//        makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, KindTemplataType, kindRule) match {
//          case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in toRef argument!", Some(rtmc)))
//          case (RuleTyperMatchSuccess(kindRuleT)) => {
//            val ruleT = CallAR(range, name, Vector(kindRuleT), KindTemplataType)
//            (RuleTyperEvaluateSuccess(ruleT))
//          }
//        }
//      }
//      case "resolveExactSignature" => {
//        if (argumentRules.size != 2) {
//          return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "resolveExactSignature expects 2 argument, but received " + argumentRules.size, None))
//        }
//        val Vector(nameRule, argsRule) = argumentRules
//        val nameRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, StringTemplataType, nameRule) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in toRef argument!", Some(rtmc))
//            case (RuleTyperMatchSuccess(nameRuleT)) => nameRuleT
//          }
//        val argsListRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, PackTemplataType(CoordTemplataType), argsRule) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in toRef argument!", Some(rtmc))
//            case (RuleTyperMatchSuccess(nameRuleT)) => nameRuleT
//          }
//        val ruleT = CallAR(range, "resolveExactSignature", Vector(nameRuleT, argsListRuleT), PrototypeTemplataType)
//        RuleTyperEvaluateSuccess(ruleT)
//      }
//      case _ => throw CompileErrorExceptionA(RangedInternalErrorA(range, "Unknown function \"" + name + "\"!"));
//    }
//  }
//
//  def evaluateTemplex(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    ruleTemplex: IRulexSR,
//  ): (IRuleTyperEvaluateResult[IRulexAR]) = {
//    ruleTemplex match {
//      case IntSR(range, value) => (RuleTyperEvaluateSuccess(IntAR(range, value)))
//      case BoolSR(range, value) => (RuleTyperEvaluateSuccess(BoolAR(range, value)))
//      case MutabilitySR(range, value) => (RuleTyperEvaluateSuccess(MutabilityAR(range, value)))
//      case PermissionSR(range, value) => (RuleTyperEvaluateSuccess(PermissionAR(range, value)))
//      case LocationSR(range, value) => (RuleTyperEvaluateSuccess(LocationAR(range, value)))
//      case OwnershipSR(range, value) => (RuleTyperEvaluateSuccess(OwnershipAR(range, value)))
//      case VariabilitySR(range, value) => (RuleTyperEvaluateSuccess(VariabilityAR(range, value)))
//      case NameSR(range, nameS) => {
//        delegate.lookupType(state, env, range, nameS) match {
//          case (KindTemplataType) => {
//            // The thing identified by `name` is a kind, but we don't know whether we're trying to access it
//            // as a kind, or trying to access it like a coord.
//            // Kinds from the outside are ambiguous until we know from context whether we're trying to use
//            // them like a kind or a coord.
//            RuleTyperEvaluateUnknown()
//          }
//          case (otherType) => {
//            val nameA = Astronomer.translateImpreciseName(nameS)
//            (RuleTyperEvaluateSuccess(NameAR(range, nameA, otherType)))
//          }
//        }
//      }
//      case RuneSR(range, runeS) => {
//        val runeA = Astronomer.translateRune(runeS)
//        conclusions.typeByRune.get(runeA) match {
//          case Some(tyype) => (RuleTyperEvaluateSuccess(RuneAR(range, runeA, tyype)))
//          case None => RuleTyperEvaluateUnknown()
//        }
//      }
//      case InterpretedSR(range, ownership, permission, innerCoordTemplexS) => {
//        makeMatcher().matchTypeAgainstTemplexS(state, env, conclusions, CoordTemplataType, innerCoordTemplexS) match {
//          case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in inner coord part!", Some(rtmc)))
//          case (RuleTyperMatchUnknown()) => {
//            RuleTyperEvaluateUnknown()
//          }
//          case (RuleTyperMatchSuccess(kindTemplexT)) => {
//            val templexT = InterpretedAT(range, ownership, permission, kindTemplexT)
//            (RuleTyperEvaluateSuccess(templexT))
//          }
//        }
//      }
//      case CallSR(range, templateRule, paramRules) => {
//        val maybeTemplateT =
//          evaluateTemplex(state, env, conclusions, templateRule) match {
//            case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "bogglewogget", Some(iec)))
//            case RuleTyperEvaluateUnknown() => (None)
//            case (RuleTyperEvaluateSuccess(templexT)) => {
//              templexT.resultType match {
//                case TemplateTemplataType(_, _) =>
//                case _ => {
//                  return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Trying to call something that's not a template! Is actually: " + templexT.resultType, None))
//                }
//              }
//              (Some(templexT))
//            }
//          }
//
//        maybeTemplateT match {
//          case None => {
//            // We don't know the template type, so we can't know the resulting type and can't assemble
//            // the CallAR... but evaluating the arguments anyway might yield clues as to the types of
//            // the runes, so evaluate them anyway.
//
//            paramRules.zipWithIndex.foreach({
//                case ((paramRule, paramIndex)) => {
//                  evaluateTemplex(state, env, conclusions, paramRule) match {
//                    case (imc @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict while evaluating param #" + paramIndex + "! " + paramRule, Some(imc)))
//                    case RuleTyperEvaluateUnknown() =>
//                    case (RuleTyperEvaluateSuccess(paramRuleT)) => {
//                      // Throw it away; without knowing the template type, even with the argument types
//                      // we can't know the return type.
//                      val _ = paramRuleT
//                    }
//                  }
//                }
//              })
//            RuleTyperEvaluateUnknown()
//          }
//          case Some(templateT) => {
//            val TemplateTemplataType(paramTypes, returnType) = templateT.resultType
//            val maybeRulesT =
//              paramTypes.zip(paramRules).zipWithIndex.map({
//                case (((paramType, paramRule), paramIndex)) => {
//                  makeMatcher().matchTypeAgainstTemplexS(state, env, conclusions, paramType, paramRule) match {
//                    case (imc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict while matching param #" + paramIndex + "! Was matching " + paramType + " and " + paramRule, Some(imc)))
//                    case (RuleTyperMatchUnknown()) => None
//                    case (RuleTyperMatchSuccess(paramTemplexT)) => Some(paramTemplexT)
//                  }
//                }
//              })
//
//            if (maybeRulesT.contains(None)) {
//              RuleTyperEvaluateUnknown()
//            } else {
//              returnType match {
//                case KindTemplataType => {
//                  // Return unknown, because we don't know if it should actually be a kind, or a coord.
//                  // Only the matcher can figure this out.
//                  RuleTyperEvaluateUnknown()
//                }
//                case _ => {
//                  (RuleTyperEvaluateSuccess(CallAT(range, templateT, maybeRulesT.flatten, returnType)))
//                }
//              }
//            }
//          }
//        }
//      }
//      case PrototypeSR(range, _, _, _) => {
//        throw CompileErrorExceptionA(RangedInternalErrorA(range, "Unimplemented"))
//      }
//      case PackSR(range, _) => {
////        evaluateTemplexes(env, conclusions, memberTemplexes) match {
////          case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => {
////            return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed to evaluate CallST arguments", Some(iec)))
////          }
////          case RuleTyperEvaluateUnknown() => {
////            RuleTyperEvaluateUnknown()
////          }
////          case (RuleTyperEvaluateSuccess(memberTemplatas)) => {
////            val memberCoords = memberTemplatas.collect({ case CoordTemplata(coord) => coord })
////            if (memberCoords.size != memberTemplatas.size) {
////              vfail("Packs can only take coords!")
////            }
////
////            val (packKind, _) = delegate.getPackKind(env, memberCoords)
////            (RuleTyperEvaluateSuccess(KindTemplata(packKind)))
////          }
////        }
//        vfail()
//      }
//      case RepeaterSequenceSR(range, mutabilityTemplexS, variabilityTemplexS, sizeTemplexS, elementTemplexS) => {
//        // It's futile to try and get the templexTs for size and element, since we don't know whether this
//        // thing will end up as a kind or coord (only matching can know that) but hey, let's match into
//        // them anyway, they might provide some nice intel for our conclusions.
//
//          makeMatcher().matchTypeAgainstTemplexS(state, env, conclusions, MutabilityTemplataType, mutabilityTemplexS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in mutability part!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) =>
//            case (RuleTyperMatchSuccess(_)) =>
//          }
//          makeMatcher().matchTypeAgainstTemplexS(state, env, conclusions, VariabilityTemplataType, variabilityTemplexS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in variability part!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) =>
//            case (RuleTyperMatchSuccess(_)) =>
//          }
//          makeMatcher().matchTypeAgainstTemplexS(state, env, conclusions, IntegerTemplataType, sizeTemplexS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in element part!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) =>
//            case (RuleTyperMatchSuccess(_)) =>
//          }
//          makeMatcher().matchTypeAgainstTemplexS(state, env, conclusions, CoordTemplataType, elementTemplexS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in element part!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) =>
//            case (RuleTyperMatchSuccess(_)) =>
//          }
//
//        // We don't know whether this thing is expected to be a kind or a coord, only matching can figure that out.
//        // Return unknown.
//        RuleTyperEvaluateUnknown()
//      }
//      case ManualSequenceSR(range, elements) => {
//        elements.foreach(element => {
//          makeMatcher().matchTypeAgainstTemplexS(state, env, conclusions, CoordTemplataType, element) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Conflict in element part!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) => None
//            case (RuleTyperMatchSuccess(templexA)) => Some(templexA)
//          }
//        })
//
//        // We don't know whether this thing is expected to be a kind or a coord, only matching can figure that out.
//        // Return unknown.
//        RuleTyperEvaluateUnknown()
//      }
//    }
//  }
//
//  def evaluateTypedRule(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    rule: TypedSR,
//  ): (IRuleTyperEvaluateResult[TemplexAR]) = {
//    val TypedSR(range, runeS, typeSR) = rule
//    val runeA = Astronomer.translateRune(runeS)
//
//    val templataType =
//      typeSR match {
//        case CoordTypeSR => CoordTemplataType
//        case IntTypeSR => IntegerTemplataType
//        case KindTypeSR => KindTemplataType
//        case MutabilityTypeSR => MutabilityTemplataType
//        case VariabilityTypeSR => VariabilityTemplataType
//        case OwnershipTypeSR => OwnershipTemplataType
//        case PermissionTypeSR => PermissionTemplataType
//        case PrototypeTypeSR => PrototypeTemplataType
//      }
//
//    conclusions.typeByRune.get(runeA) match {
//      case None =>
//      case Some(typeFromConclusions) => {
//        if (typeFromConclusions != templataType) {
//          return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Typed rule failed: expected rune " + runeA + " to be " + templataType + " but previously concluded " + typeFromConclusions, None))
//        }
//      }
//    }
//
//    makeMatcher().matchTypeAgainstTypedSR(state, env, conclusions, templataType, rule) match {
//      case (imc @ RuleTyperMatchConflict(_, _, _, _)) => (RuleTyperEvaluateConflict(conclusions.conclusions, range, "", Some(imc)))
//      case (RuleTyperMatchUnknown()) => RuleTyperEvaluateUnknown()
//      case (RuleTyperMatchSuccess(ruleT)) => (RuleTyperEvaluateSuccess(ruleT))
//    }
//  }
//
//  def evaluateIsaRule(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    rule: IsaSR,
//  ): (IRuleTyperEvaluateResult[IsaAR]) = {
//    val IsaSR(range, leftRuleS, rightRuleS) = rule
//
//    val maybeLeftRuleT =
//      makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, KindTemplataType, leftRuleS) match {
//        case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed matching isa's left rule", Some(rtmc)))
//        case (RuleTyperMatchUnknown()) => (None)
//        case (RuleTyperMatchSuccess(leftRuleT)) => (Some(leftRuleT))
//      }
//
//    val maybeRightRuleT =
//      makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, KindTemplataType, rightRuleS) match {
//        case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed matching isa's right rule", Some(rtmc)))
//        case (RuleTyperMatchUnknown()) => (None)
//        case (RuleTyperMatchSuccess(rightRuleT)) => (Some(rightRuleT))
//      }
//
//    (maybeLeftRuleT, maybeRightRuleT) match {
//      case (Some(leftRuleT), Some(rightRuleT)) => (RuleTyperEvaluateSuccess(IsaAR(range, leftRuleT, rightRuleT)))
//      case (_, _) => RuleTyperEvaluateUnknown()
//    }
//  }
//
//  def evaluateEqualsRule(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    rule: EqualsSR,
//  ): (IRuleTyperEvaluateResult[EqualsAR]) = {
//    val EqualsSR(range, leftRuleS, rightRuleS) = rule
//
//    val maybeLeftRuleT =
//      evaluateRule(state, env, conclusions, leftRuleS) match {
//        case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed evaluating left rule!", Some(iec)))
//        case RuleTyperEvaluateUnknown() => (None)
//        case (RuleTyperEvaluateSuccess(leftRuleT)) => (Some(leftRuleT))
//      }
//
//    val maybeRightRuleT =
//      evaluateRule(state, env, conclusions, rightRuleS) match {
//        case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed evaluating right rule!", Some(iec)))
//        case RuleTyperEvaluateUnknown() => (None)
//        case (RuleTyperEvaluateSuccess(rightRuleT)) => (Some(rightRuleT))
//      }
//
//    (maybeLeftRuleT, maybeRightRuleT) match {
//      case (Some(leftRuleT), Some(rightRuleT)) => {
//
//        if (leftRuleT.resultType != rightRuleT.resultType) {
//          return RuleTyperEvaluateConflict(
//            conclusions.conclusions,
//            range,
//            "Left rule type (" + leftRuleT.resultType + ") doesn't match right rule type (" + rightRuleT.resultType + ")", None)
//        } else {
//          (RuleTyperEvaluateSuccess(EqualsAR(range, leftRuleT, rightRuleT)))
//        }
//      }
//      case (Some(leftRuleT), None) => {
//        // We know the left, but don't know the right. Use the type from the left
//        // to try and figure out the thing on the right.
//        makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, leftRuleT.resultType, rightRuleS) match {
//          case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed matching right rule with type from left (" + leftRuleT.resultType + ")", Some(rtmc)))
//          case (RuleTyperMatchUnknown()) => RuleTyperEvaluateUnknown()
//          case (RuleTyperMatchSuccess(rightRuleT)) => {
//            (RuleTyperEvaluateSuccess(EqualsAR(range, leftRuleT, rightRuleT)))
//          }
//        }
//      }
//      case (None, Some(rightRuleT)) => {
//        // We know the left, but don't know the right. Use the type from the left
//        // to try and figure out the thing on the right.
//        makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, rightRuleT.resultType, leftRuleS) match {
//          case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed matching left rule with type from right (" + rightRuleT.resultType + ")", Some(rtmc)))
//          case (RuleTyperMatchUnknown()) => RuleTyperEvaluateUnknown()
//          case (RuleTyperMatchSuccess(leftRuleT)) => {
//            (RuleTyperEvaluateSuccess(EqualsAR(range, leftRuleT, rightRuleT)))
//          }
//        }
//      }
//      case (None, None) => {
//        RuleTyperEvaluateUnknown()
//      }
//    }
//  }
//
//  def evaluateOrRule(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    zrule: OrSR
//  ): (IRuleTyperEvaluateResult[OrAR]) = {
//    val OrSR(range, alternatives) = zrule
//    val listOfMaybeAlternativeT =
//      alternatives.zipWithIndex.foldLeft((Vector[Option[IRulexAR]]()))({
//        case ((Vector()), (alternative, alternativeIndex)) => {
//          evaluateRule(state, env, conclusions, alternative) match {
//            case (rtec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed to evaluate alternative index " + alternativeIndex, Some(rtec)))
//            case (RuleTyperEvaluateSuccess(alternativeRuleT)) => {
//              (Vector(Some(alternativeRuleT)))
//            }
//            case RuleTyperEvaluateUnknown() => {
//              (Vector(None))
//            }
//          }
//        }
//        case ((previousMaybeAlternativesT), (alternative, alternativeIndex)) => {
//          val maybeKnownType = previousMaybeAlternativesT.flatten.headOption.map(_.resultType)
//          maybeKnownType match {
//            case None => {
//              evaluateRule(state, env, conclusions, alternative) match {
//                case (rtec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed to evaluate alternative index " + alternativeIndex, Some(rtec)))
//                case RuleTyperEvaluateUnknown() => {
//                  (previousMaybeAlternativesT :+ None)
//                }
//                case (RuleTyperEvaluateSuccess(alternativeRuleT)) => {
//                  (previousMaybeAlternativesT :+ Some(alternativeRuleT))
//                }
//              }
//            }
//            case Some(knownType) => {
//              makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, knownType, alternative) match {
//                case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed to evaluate alternative index " + alternativeIndex, Some(rtmc)))
//                case (RuleTyperMatchUnknown()) => {
//                  (previousMaybeAlternativesT :+ None)
//                }
//                case (RuleTyperMatchSuccess(alternativeRuleT)) => {
//                  (previousMaybeAlternativesT :+ Some(alternativeRuleT))
//                }
//              }
//            }
//          }
//        }
//      })
//    if (listOfMaybeAlternativeT.contains(None)) {
//      RuleTyperEvaluateUnknown()
//    } else {
//      val alternativesT = listOfMaybeAlternativeT.flatten
//      (RuleTyperEvaluateSuccess(OrAR(range, alternativesT)))
//    }
//  }
//
//  def evaluateComponentsRule(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    rule: ComponentsSR,
//  ): (IRuleTyperEvaluateResult[EqualsAR]) = {
//    val ComponentsSR(range, typedRule, components) = rule
//
//    val maybeTypeAndRuneRuleT =
//      evaluateTypedRule(state, env, conclusions, typedRule) match {
//        case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Components rule type disagrees!", Some(iec)))
//        case RuleTyperEvaluateUnknown() => (None)
//        case (RuleTyperEvaluateSuccess(typeAndRuneRuleT)) => (Some(typeAndRuneRuleT))
//      }
//
//    val maybeComponentRulesT =
//      typedRule.tyype match {
//        case KindTypeSR => {
//          evaluateKindComponents(state, env, conclusions, range, components) match {
//            case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed evaluating kind components!", Some(iec)))
//            case RuleTyperEvaluateUnknown() => (None)
//            case (RuleTyperEvaluateSuccess(templataFromRune)) => (Some(templataFromRune))
//          }
//        }
//        case CoordTypeSR => {
//          evaluateCoordComponents(state, env, conclusions, range, components) match {
//            case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed evaluating coord components!", Some(iec)))
//            case RuleTyperEvaluateUnknown() => (None)
//            case (RuleTyperEvaluateSuccess(templataFromRune)) => (Some(templataFromRune))
//          }
//        }
//        case PrototypeTypeSR => {
//          evaluatePrototypeComponents(state, env, conclusions, range, components) match {
//            case (iec @ RuleTyperEvaluateConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, range, "Failed evaluating prototype components!", Some(iec)))
//            case RuleTyperEvaluateUnknown() => (None)
//            case (RuleTyperEvaluateSuccess(templataFromRune)) => (Some(templataFromRune))
//          }
//        }
//        case _ => throw CompileErrorExceptionA(RangedInternalErrorA(range, "Can only destructure coords and kinds!"))
//      }
//
//    (maybeTypeAndRuneRuleT, maybeComponentRulesT) match {
//      case (Some(typeAndRuneRuleT), Some(componentRulesT)) => {
//        val equalsT =
//          EqualsAR(
//            range,
//            typeAndRuneRuleT,
//            ComponentsAR(range, typeAndRuneRuleT.resultType, componentRulesT))
//        (RuleTyperEvaluateSuccess(equalsT))
//      }
//      case (None, None) => {
//        RuleTyperEvaluateUnknown()
//      }
//    }
//  }
//
//  private def evaluateCoordComponents(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    outerRange: RangeS,
//    components: Vector[IRulexSR]):
//  (IRuleTyperEvaluateResult[Vector[IRulexAR]]) = {
//    components match {
//      case Vector(ownershipRuleS, permissionRuleS, kindRuleS) => {
//        val maybeOwnershipRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, OwnershipTemplataType, ownershipRuleS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, outerRange, "Ownership component conflicted!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) => (None)
//            case (RuleTyperMatchSuccess(ownershipRuleT)) => (Some(ownershipRuleT))
//          }
////        val maybeLocationRuleT =
////          makeMatcher().matchTypeAgainstRulexSR(env, conclusions, LocationTemplataType, locationRuleS) match {
////            case rtmc @ RuleTyperMatchConflict(_, _, _, _) => return RuleTyperEvaluateConflict(conclusions.conclusions, range, "Location component conflicted!", Some(rtmc))
////            case RuleTyperMatchUnknown(c) => (c, None)
////            case RuleTyperMatchContinue(c, locationRuleT) => (c, Some(locationRuleT))
////          }
//        val maybePermissionRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, PermissionTemplataType, permissionRuleS) match {
//            case rtmc @ RuleTyperMatchConflict(_, _, _, _) => return RuleTyperEvaluateConflict(conclusions.conclusions, outerRange, "Permission component conflicted!", Some(rtmc))
//            case RuleTyperMatchUnknown() => (None)
//            case RuleTyperMatchSuccess(permissionRuleT) => (Some(permissionRuleT))
//          }
//        val maybeKindRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, KindTemplataType, kindRuleS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, outerRange, "Kind component conflicted!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) => (None)
//            case (RuleTyperMatchSuccess(kindRuleT)) => (Some(kindRuleT))
//          }
//        (maybeOwnershipRuleT, maybePermissionRuleT, maybeKindRuleT) match {
//          case (Some(ownershipRuleT), Some(permissionRuleT), Some(kindRuleT)) => {
//            (RuleTyperEvaluateSuccess(Vector(ownershipRuleT, permissionRuleT, kindRuleT)))
//          }
//          case (_, _, _) => {
//            RuleTyperEvaluateUnknown()
//          }
//        }
//
//      }
//      case _ => {
//        throw CompileErrorExceptionA(RangedInternalErrorA(outerRange, "Coords must have 3 components"))
//      }
//    }
//  }
//
//  private def evaluatePrototypeComponents(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    outerRange: RangeS,
//    components: Vector[IRulexSR]):
//  (IRuleTyperEvaluateResult[Vector[IRulexAR]]) = {
//    components match {
//      case Vector(humanNameRuleS, argsPackRuleS, returnRuleS) => {
//        val maybeHumanNameRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, StringTemplataType, humanNameRuleS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, outerRange, "Prototype name component conflicted!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) => (None)
//            case (RuleTyperMatchSuccess(humanNameRuleT)) => (Some(humanNameRuleT))
//          }
//        val maybeCoordPackRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, PackTemplataType(CoordTemplataType), argsPackRuleS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => {
//              return (RuleTyperEvaluateConflict(conclusions.conclusions, outerRange, "Prototype args component conflicted!", Some(rtmc)))
//            }
//            case (RuleTyperMatchUnknown()) => (None)
//            case (RuleTyperMatchSuccess(kindRuleT)) => (Some(kindRuleT))
//          }
//        val maybeCoordRuleT =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, CoordTemplataType, returnRuleS) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, outerRange, "Prototype return component conflicted!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) => (None)
//            case (RuleTyperMatchSuccess(kindRuleT)) => (Some(kindRuleT))
//          }
//        (maybeHumanNameRuleT, maybeCoordPackRuleT, maybeCoordRuleT) match {
//          case (Some(humanNameRuleT), Some(coordPackRuleT), Some(coordRuleT)) => {
//            (RuleTyperEvaluateSuccess(Vector(humanNameRuleT, coordPackRuleT, coordRuleT)))
//          }
//          case (_, _, _) => {
//            RuleTyperEvaluateUnknown()
//          }
//        }
//
//      }
//      case _ => throw CompileErrorExceptionA(RangedInternalErrorA(outerRange, "Prototypes must have 3 components"))
//    }
//  }
//
//  private def evaluateKindComponents(
//    state: State,
//    env: Env,
//    conclusions: ConclusionsBox,
//    outerRange: RangeS,
//    components: Vector[IRulexSR]):
//  (IRuleTyperEvaluateResult[Vector[IRulexAR]]) = {
//    components match {
//      case Vector(mutabilityRule) => {
//        val maybeMutabilityRule =
//          makeMatcher().matchTypeAgainstRulexSR(state, env, conclusions, MutabilityTemplataType, mutabilityRule) match {
//            case (rtmc @ RuleTyperMatchConflict(_, _, _, _)) => return (RuleTyperEvaluateConflict(conclusions.conclusions, outerRange, "Mutability component conflicted!", Some(rtmc)))
//            case (RuleTyperMatchUnknown()) => (None)
//            case (RuleTyperMatchSuccess(mutabilityRuleT)) => (Some(mutabilityRuleT))
//          }
//        maybeMutabilityRule match {
//          case None => RuleTyperEvaluateUnknown()
//          case Some(mutabilityRuleT) => (RuleTyperEvaluateSuccess(Vector(mutabilityRuleT)))
//        }
//      }
//      case _ => throw CompileErrorExceptionA(RangedInternalErrorA(outerRange, "Kind rule must have one component"))
//    }
//  }
//
//  private def makeMatcher() = {
//    new RuleTyperMatcher[Env, State](
//      evaluateTemplex,
//      new RuleTyperMatcherDelegate[Env, State] {
//        override def lookupType(state: State, env: Env, range: RangeS, name: CodeTypeNameS): Conclusion = {
//          delegate.lookupType(state, env, range, name)
//        }
//
//        override def lookupType(state: State, env: Env, range: RangeS, name: INameS): Conclusion = {
//          delegate.lookupType(state, env, range, name)
//        }
//      })
//  }
//

}
