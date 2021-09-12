package net.verdagon.vale.scout.predictor

import net.verdagon.vale.{Err, Ok, Result, vassertSome, vfail, vimpl, vwat}
import net.verdagon.vale.scout.{IRuneS, RangeS}
import net.verdagon.vale.scout.patterns.{AtomSP, PatternSUtils}
import net.verdagon.vale.scout.rules.{ILiteralSL, IRulexSR, ITypeSR, RuleFlattener}

import scala.collection.mutable

object PredictorEvaluator {

//  private[scout] def getAllRunes(
//    userSpecifiedIdentifyingRunes: Vector[IRuneS],
//    rules: Vector[IRulexSR],
//    patterns1: Vector[AtomSP],
//    maybeRetRune: Option[IRuneS]
//  ): Set[IRuneS] = {
//    (
//      userSpecifiedIdentifyingRunes ++
//        patterns1.flatMap(PatternSUtils.getDistinctOrderedRunesForPattern) ++
//        RuleSUtils.getDistinctOrderedRunesForRulexes(rules) ++
//        maybeRetRune.toVector
//      ).toSet
//  }
//
//  def makeSolver():
//  Solver[RangeS, IValueSR, IValueSR, Unit, Unit, Unit, String] = {
//    new Solver[RangeS, IValueSR, IValueSR, Unit, Unit, Unit, String](
//      new ISolverDelegate[RangeS, IValueSR, IValueSR, Unit, Unit, Unit, String] {
//        override def solve(state: Unit, env: Unit, range: RangeS, rule: IRulexAR[Int, RangeS, IValueSR, IValueSR], runes: Int => Option[Unit]): Result[Map[Int, Unit], String] = {
//          rule match {
//            case LookupAR(range, rune, _) => Ok(Map(rune -> Unit))
//            case LiteralAR(range, rune, _) => Ok(Map(rune -> Unit))
//            case CoerceToCoord(range, coordRune, kindRune) => {
//              (runes(coordRune).nonEmpty, runes(kindRune).nonEmpty) match {
//                case (true, true) => vwat()
//                case (true, false) => vwat()
//                case (false, true) => Ok(Map(coordRune -> Unit))
//                case (false, false) => vwat()
//              }
//            }
//            case AugmentAR(range, resultRune, literal, innerRune) => {
//              (runes(resultRune).nonEmpty, runes(innerRune).nonEmpty) match {
//                case (true, true) => vwat()
//                case (true, false) => Ok(Map(innerRune -> Unit))
//                case (false, true) => Ok(Map(resultRune -> Unit))
//                case (false, false) => vwat()
//              }
//            }
//            case RepeaterSequenceAR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
//              (runes(resultRune).nonEmpty, runes(mutabilityRune).nonEmpty,  runes(variabilityRune).nonEmpty,  runes(sizeRune).nonEmpty,  runes(elementRune).nonEmpty) match {
//                case (false, true, true, true, true) => Ok(Map(resultRune -> Unit))
//                case (true, false, false, false, false) => Ok(Map(mutabilityRune -> Unit, variabilityRune -> Unit, sizeRune -> Unit, elementRune -> Unit))
//                case _ => vwat()
//              }
//            }
//            case CallAR(range, resultRune, templateRune, argRunes) => {
//              (runes(resultRune).nonEmpty, runes(templateRune).nonEmpty, !argRunes.map(runes).contains(None)) match {
//                case (true, true, true) => vwat()
//                case (true, true, false) => Ok(argRunes.map(a => (a -> ())).toMap)
//                case (true, false, true) => vwat()
//                case (true, false, false) => vwat()
//                case (false, true, true) => Ok(Map(resultRune -> Unit))
//                case (false, true, false) => vwat()
//                case (false, false, true) => vwat()
//                case (false, false, false) => vwat()
//              }
//            }
//            case CoordComponentsAR(_, coordRune, ownershipRune, permissionRune, kindRune) => {
//              runes(coordRune).nonEmpty match {
//                case true => Ok(Map(ownershipRune -> Unit, permissionRune -> Unit, kindRune -> Unit))
//                case false => {
//                  (runes(ownershipRune).nonEmpty, runes(permissionRune).nonEmpty, runes(kindRune).nonEmpty) match {
//                    case (true, true, true) => Ok(Map(coordRune -> Unit))
//                    case _ => vfail()
//                  }
//                }
//              }
//            }
//          }
//        }
//      })
//  }
//
//  private[scout] def solve(
//    invocationRange: RangeS,
//    runeSToTentativeRune: mutable.HashMap[IRuneS, TentativeRune],
//    tentativeRuneToCanonicalRune: Map[TentativeRune, Int],
//    tentativeRuneToType: mutable.HashMap[TentativeRune, ITypeSR],
//    world: Analysis[Int, RangeS, IValueSR, IValueSR]
//  ): Conclusions = {
//    val (orderedCanonicalRules, canonicalRuneToPredictable) = Planner.solveAndReorder(world, Nil)
//
//    val runeSToType =
//      runeSToTentativeRune.map({ case (runeS, originalRune) =>
//        tentativeRuneToType.get(originalRune) match {
//          case None => List()
//          case Some(tyype) => List(runeS -> tyype)
//        }
//      }).flatten.toMap
//
//    val runeSToRune =
//      runeSToTentativeRune.mapValues(tentativeRuneToCanonicalRune).toMap
//
//    val runeSToPredictable = runeSToRune.mapValues(canonicalRune => canonicalRuneToPredictable(canonicalRune))
//
//    val numCanonicalRunes = canonicalRuneToPredictable.length
//
//    val solver = makeSolver()
//    val conclusions =
//      solver.solve(Unit, Unit, orderedCanonicalRules, numCanonicalRunes, invocationRange) match {
//        case Ok(rawConclusions) => {
//          if (rawConclusions.contains(None)) {
//            runeSToRune
//              .filter(runeSAndRune => rawConclusions(runeSAndRune._2).nonEmpty)
//              .mapValues(i => vassertSome(rawConclusions(i)))
//          } else {
//            runeSToRune.mapValues(i => vassertSome(rawConclusions(i)))
//          }
//        }
//        case Err(FailedSolve(err, rawConclusions)) => vimpl()
//      }
//    Conclusions(conclusions.keySet, runeSToType)
//  }
}
