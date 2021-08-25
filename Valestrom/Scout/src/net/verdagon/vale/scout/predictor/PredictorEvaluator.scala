package net.verdagon.vale.scout.predictor

import net.verdagon.vale.{Err, Ok, Result, vassertSome, vfail, vimpl, vwat}
import net.verdagon.vale.scout.{IRuneS, RangeS}
import net.verdagon.vale.scout.patterns.{AtomSP, PatternSUtils}
import net.verdagon.vale.scout.rules.{ILiteralSR, ILookupSR, IRulexSR, ITypeSR, RuleFlattener, RuleSUtils}
import net.verdagon.vale.solver.{AugmentAR, CallAR, CoerceToCoord, CompleteSolve, CoordComponentsAR, FailedSolve, IRulexAR, ISolverDelegate, IncompleteSolve, LiteralAR, LookupAR, Planner, RepeaterSequenceAR, Solver, TentativeRune, World}

import scala.collection.mutable

object PredictorEvaluator {

  private[scout] def getAllRunes(
    userSpecifiedIdentifyingRunes: Vector[IRuneS],
    rules: Vector[IRulexSR],
    patterns1: Vector[AtomSP],
    maybeRetRune: Option[IRuneS]
  ): Set[IRuneS] = {
    (
      userSpecifiedIdentifyingRunes ++
        patterns1.flatMap(PatternSUtils.getDistinctOrderedRunesForPattern) ++
        RuleSUtils.getDistinctOrderedRunesForRulexes(rules) ++
        maybeRetRune.toVector
      ).toSet
  }

  def makeSolver():
  Solver[RangeS, ILiteralSR, ILookupSR, Unit, Unit, Unit, String] = {
    new Solver[RangeS, ILiteralSR, ILookupSR, Unit, Unit, Unit, String](
      new ISolverDelegate[RangeS, ILiteralSR, ILookupSR, Unit, Unit, Unit, String] {
        override def solve(state: Unit, env: Unit, range: RangeS, rule: IRulexAR[Int, RangeS, ILiteralSR, ILookupSR], runes: Int => Option[Unit]): Result[Map[Int, Unit], String] = {
          rule match {
            case LookupAR(range, rune, _) => Ok(Map(rune -> Unit))
            case LiteralAR(range, rune, _) => Ok(Map(rune -> Unit))
            case CoerceToCoord(range, coordRune, kindRune) => {
              (runes(coordRune).nonEmpty, runes(kindRune).nonEmpty) match {
                case (true, true) => vwat()
                case (true, false) => vwat()
                case (false, true) => Ok(Map(coordRune -> Unit))
                case (false, false) => vwat()
              }
            }
            case AugmentAR(range, resultRune, literal, innerRune) => {
              (runes(resultRune).nonEmpty, runes(innerRune).nonEmpty) match {
                case (true, true) => vwat()
                case (true, false) => Ok(Map(innerRune -> Unit))
                case (false, true) => Ok(Map(resultRune -> Unit))
                case (false, false) => vwat()
              }
            }
            case RepeaterSequenceAR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
              (runes(resultRune).nonEmpty, runes(mutabilityRune).nonEmpty,  runes(variabilityRune).nonEmpty,  runes(sizeRune).nonEmpty,  runes(elementRune).nonEmpty) match {
                case (false, true, true, true, true) => Ok(Map(resultRune -> Unit))
                case (true, false, false, false, false) => Ok(Map(mutabilityRune -> Unit, variabilityRune -> Unit, sizeRune -> Unit, elementRune -> Unit))
                case _ => vwat()
              }
            }
            case CallAR(range, resultRune, templateRune, argRunes) => {
              (runes(resultRune).nonEmpty, runes(templateRune).nonEmpty, !argRunes.map(runes).contains(None)) match {
                case (true, true, true) => vwat()
                case (true, true, false) => Ok(argRunes.map(a => (a -> ())).toMap)
                case (true, false, true) => vwat()
                case (true, false, false) => vwat()
                case (false, true, true) => Ok(Map(resultRune -> Unit))
                case (false, true, false) => vwat()
                case (false, false, true) => vwat()
                case (false, false, false) => vwat()
              }
            }
            case CoordComponentsAR(_, coordRune, ownershipRune, permissionRune, kindRune) => {
              runes(coordRune).nonEmpty match {
                case true => Ok(Map(ownershipRune -> Unit, permissionRune -> Unit, kindRune -> Unit))
                case false => {
                  (runes(ownershipRune).nonEmpty, runes(permissionRune).nonEmpty, runes(kindRune).nonEmpty) match {
                    case (true, true, true) => Ok(Map(coordRune -> Unit))
                    case _ => vfail()
                  }
                }
              }
            }
          }
        }
      })
  }

  private[scout] def solve(
    invocationRange: RangeS,
    runeSToTentativeRune: mutable.HashMap[IRuneS, TentativeRune],
    tentativeRuneToCanonicalRune: Map[TentativeRune, Int],
    tentativeRuneToType: mutable.HashMap[TentativeRune, ITypeSR],
    world: World[Int, RangeS, ILiteralSR, ILookupSR]
  ): Conclusions = {
    val (orderedCanonicalRules, canonicalRuneToPredictable) = Planner.solveAndReorder(world, Nil)

    val runeSToType =
      runeSToTentativeRune.map({ case (runeS, originalRune) =>
        tentativeRuneToType.get(originalRune) match {
          case None => List()
          case Some(tyype) => List(runeS -> tyype)
        }
      }).flatten.toMap

    val runeSToRune =
      runeSToTentativeRune.mapValues(tentativeRuneToCanonicalRune).toMap

    val runeSToPredictable = runeSToRune.mapValues(canonicalRune => canonicalRuneToPredictable(canonicalRune))

    val numCanonicalRunes = canonicalRuneToPredictable.length

    val solver = makeSolver()
    val conclusions =
      solver.solve(Unit, Unit, orderedCanonicalRules, numCanonicalRunes, invocationRange) match {
        case Ok(rawConclusions) => {
          if (rawConclusions.contains(None)) {
            runeSToRune
              .filter(runeSAndRune => rawConclusions(runeSAndRune._2).nonEmpty)
              .mapValues(i => vassertSome(rawConclusions(i)))
          } else {
            runeSToRune.mapValues(i => vassertSome(rawConclusions(i)))
          }
        }
        case Err(FailedSolve(err, rawConclusions)) => vimpl()
      }
    Conclusions(conclusions.keySet, runeSToType)
//    val conclusionsBox = ConclusionsBox(Conclusions(knowableRunesFromAbove, Map()))
//    solveUntilSettled(rules, conclusionsBox)
//    conclusionsBox.conclusions
  }
//
//  private def solveUntilSettled(
//    rules: Vector[IRulexSR],
//    conclusions: ConclusionsBox,
//  ): Unit = {
//    val conclusionsBefore = conclusions.conclusions
//
//    val _ = evaluateRules(conclusions, rules)
//
//    if (conclusions.conclusions != conclusionsBefore) {
//      // Things have not settled, we made some sort of progress in this last iteration.
//      // Keep going.
//      solveUntilSettled(rules, conclusions)
//    } else {
//      // No need to do one last match, because we just did an entire iteration where nothing changed.
//
//    }
//  }
//
//  private def evaluateRule(conclusions: ConclusionsBox, rule: IRulexSR): Boolean = {
//    rule match {
//      case r @ EqualsSR(_,_, _) => evaluateEqualsRule(conclusions, r)
//      case r @ IsaSR(_,_, _) => evaluateIsaRule(conclusions, r)
//      case r @ OrSR(_,_) => evaluateOrRule(conclusions, r)
//      case r @ ComponentsSR(_,_, _) => evaluateComponentsRule(conclusions, r)
//      case r @ TypedSR(_,_, _) => evaluateTypedRule(conclusions, r)
//      case r @ BuiltinCallSR(_,_, _) => evaluateRuleCall(conclusions, r)
//      case IntSR(_, _) => true
//      case StringSR(_, _) => true
//      case BoolSR(_, _) => true
//      case MutabilitySR(_, _) => true
//      case PermissionSR(_, _) => true
//      case LocationSR(_, _) => true
//      case OwnershipSR(_, _) => true
//      case VariabilitySR(_, _) => true
//      case NameSR(_, _) => true
//      case AbsoluteNameSR(_, _) => true
//      case BorrowSR(_, inner) => evaluateRule(conclusions, inner)
//      case RuneSR(_, rune) => {
//        conclusions.knowableValueRunes.contains(rune)
//      }
//      case InterpretedSR(_, _, _, kindRule) => evaluateRule(conclusions, kindRule)
//      case CallSR(_, templateRule, paramRules) => {
//        val templateKnown = evaluateRule(conclusions, templateRule)
//        val argsKnown = evaluateRulees(conclusions, paramRules)
//        templateKnown && argsKnown.forall(_ == true)
//      }
//      case PrototypeSR(_, _, _, _) => {
//        vfail("Unimplemented")
//      }
//      case PackSR(_, memberTemplexes) => {
//        val membersKnown =
//          evaluateRulees(conclusions, memberTemplexes)
//        membersKnown.forall(_ == true)
//      }
//      case RepeaterSequenceSR(_, mutabilityTemplex, variabilityTemplex, sizeTemplex, elementTemplex) => {
//        val mutabilityKnown =
//          evaluateRule(conclusions, mutabilityTemplex)
//        val variabilityKnown =
//          evaluateRule(conclusions, variabilityTemplex)
//        val sizeKnown =
//          evaluateRule(conclusions, sizeTemplex)
//        val elementKnown =
//          evaluateRule(conclusions, elementTemplex)
//        mutabilityKnown && variabilityKnown && sizeKnown && elementKnown
//      }
//      case ManualSequenceSR(_, elementsTemplexes) => {
//        val membersKnown =
//          evaluateRulees(conclusions, elementsTemplexes)
//        membersKnown.forall(_ == true)
//      }
//    }
//  }
//
////  private def evaluatePackRule(conclusions: ConclusionsBox, rule: PackSR): Boolean = {
////    val PackSR(elements) = rule
////    evaluateRules(conclusions, elements).forall(_ == true)
////  }
//
//  private def evaluateRules(
//    conclusions: ConclusionsBox,
//    rules: Vector[IRulexSR],
//  ): Vector[Boolean] = {
//    rules.map(evaluateRule(conclusions, _))
//  }
//
//  private def evaluateRuleCall(
//    conclusions: ConclusionsBox,
//    ruleCall: BuiltinCallSR,
//  ): Boolean = {
//    val BuiltinCallSR(range, name, argumentRules) = ruleCall
//
//    name match {
//      case "toRef" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
//      case "passThroughIfConcrete" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
//      case "passThroughIfStruct" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
//      case "passThroughIfInterface" => {
//        val Vector(kindRule) = argumentRules
//        evaluateRule(conclusions, kindRule)
//      }
////      case "resolveExactSignature" => {
////        val Vector(nameRule, argsRule) = argumentRules
////        val evaluateNameSuccess = evaluateRule(conclusions, nameRule)
////        val evaluateArgsSuccess = evaluateRule(conclusions, argsRule)
////        evaluateNameSuccess && evaluateArgsSuccess
////      }
//      case _ => throw CompileErrorExceptionS(RangedInternalErrorS(range, "Unknown function \"" + name + "\"!"))
//    }
//  }
//  private def evaluateRulees(
//    conclusions: ConclusionsBox,
//    ruleTemplexes: Vector[IRulexSR],
//  ): Vector[Boolean] = {
//    val knowns =
//      ruleTemplexes.map({
//        case (ruleTemplex) => {
//          val result = evaluateRule(conclusions, ruleTemplex)
//          result
//        }
//      })
//    knowns
//  }
//
//  private def evaluateTypedRule(
//    conclusions: ConclusionsBox,
//    rule: TypedSR):
//  Boolean = {
//    val TypedSR(_, rune, tyype) = rule
//    conclusions.markRuneTypeKnown(rune, tyype)
//    conclusions.knowableValueRunes.contains(rune)
//  }
//
//  private def evaluateEqualsRule(
//    conclusions: ConclusionsBox,
//    rule: EqualsSR,
//  ): Boolean = {
//    val EqualsSR(_, leftRule, rightRule) = rule
//
//    val leftKnown =
//      evaluateRule(conclusions, leftRule)
//    val rightKnown =
//      evaluateRule(conclusions, rightRule)
//    if (!leftKnown && !rightKnown) {
//      false
//    } else {
//      PredictorMatcher.matchAgainstRulexSR(conclusions, leftRule)
//      PredictorMatcher.matchAgainstRulexSR(conclusions, rightRule)
//      true
//    }
//  }
//
//  private def evaluateIsaRule(
//    conclusions: ConclusionsBox,
//    rule: IsaSR,
//  ): Boolean = {
//    val IsaSR(_, leftRule, rightRule) = rule
//
//    val leftKnown =
//      evaluateRule(conclusions, leftRule)
//    val rightKnown =
//      evaluateRule(conclusions, rightRule)
//
//    // Knowing the right rule doesn't really help us with anything, unfortunately...
//    val _ = rightKnown
//
//    // We return the left thing for the rule, so if we know the left thing, we know the result of the rule.
//    leftKnown
//  }
//
//  private def evaluateOrRule(
//    conclusions: ConclusionsBox,
//    rule: OrSR
//  ): Boolean = {
//    val possibilitiesKnowns =
//      evaluateRules(conclusions, rule.alternatives)
//    // This is a conservative guess. We might be able to return true if only one is known.
//    possibilitiesKnowns.forall(_ == true)
//  }
//
//  private def evaluateComponentsRule(
//    conclusions: ConclusionsBox,
//    rule: ComponentsSR,
//  ): Boolean = {
//    val ComponentsSR(_, typedRule, componentsRules) = rule
//
//    val runeKnown =
//      evaluateRule(conclusions, typedRule)
//
//    val componentsKnown =
//      evaluateRules(conclusions, componentsRules)
//    val allComponentsKnown = componentsKnown.forall(_ == true)
//
//    runeKnown || allComponentsKnown
//  }
}
