package net.verdagon.vale.astronomer.ruletyper

import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale._
import net.verdagon.vale.astronomer._
import net.verdagon.vale.solver.{ISolveRule, Planner, Solver}

import scala.collection.immutable.List

case class AstronomySolveError(unknownRunes: Iterable[IRuneS])
object AstronomySolver { // extends ISolveRule[IRulexSR, IRuneS, Unit, Unit, ITemplataType, ITemplataType]
  def getRunes(rule: IRulexSR): Array[IRuneS] = {
    rule match {
      case LookupSR(range, rune, literal) => Array(rune)
      case EqualsSR(range, left, right) => Array(left, right)
      case IsaSR(range, sub, suuper) => Array(sub, suuper)
      case KindComponentsSR(range, resultRune, mutabilityRune) => Array(resultRune, mutabilityRune)
      case CoordComponentsSR(range, resultRune, ownershipRune, permissionRune, kindRune) => Array(resultRune, ownershipRune, permissionRune, kindRune)
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
  }

  def getPuzzles(predicting: Boolean, rule: IRulexSR): Array[Array[IRuneS]] = {
    rule match {
      case LookupSR(_, rune, literal) => {
        if (predicting) {
          // This Array() literally means nothing can solve this puzzle.
          // It needs to be passed in via plan/solve's initiallyKnownRunes parameter.
          Array()
        } else {
          // Lookup rules get their type from their rune, which is either populated beforehand,
          // or figured out from another rule, see AMPLR.
          Array(Array(rune))
        }
      }
      // These rules might not be knowable when predicting; the templateRune is likely a LookupSR
      // which we can't really know at prediction time.
      case CallSR(range, resultRune, templateRune, args) => Array(Array(resultRune, templateRune), Array(templateRune) ++ args)
      case PackSR(_, resultRune, members) => Array(Array(resultRune), members)
      // Below here, all rules know exactly what types they operate on.
      case KindComponentsSR(_, resultRune, mutabilityRune) => Array(Array())
      case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => Array(Array())
      case OneOfSR(_, rune, literals) => Array(Array())
      case IsConcreteSR(_, rune) => Array(Array(rune))
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
    state: AstroutsBox,
    env: Environment,
    ruleIndex: Int,
    rule: IRulexSR,
    getConclusion: IRuneS => Option[ITemplataType],
    concludeRune: (IRuneS, ITemplataType) => Unit):
  Result[Unit, Unit] = {
    rule match {
      case KindComponentsSR(range, resultRune, mutabilityRune) => {
        concludeRune(resultRune, KindTemplataType)
        concludeRune(mutabilityRune, MutabilityTemplataType)
        Ok(())
      }
      case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => {
        concludeRune(resultRune, CoordTemplataType)
        concludeRune(ownershipRune, OwnershipTemplataType)
        concludeRune(permissionRune, PermissionTemplataType)
        concludeRune(kindRune, KindTemplataType)
        Ok(())
      }
      case OneOfSR(_, resultRune, literals) => {
        val types = literals.map(_.getType()).map(Astronomer.translateRuneType).toSet
        if (types.size > 1) {
          vfail("OneOf rule's possibilities must all be the same type!")
        }
        concludeRune(resultRune, types.head)
        Ok(())
      }
      case IsConcreteSR(_, rune) => {
        concludeRune(rune, KindTemplataType)
        Ok(())
      }
      case IsInterfaceSR(_, rune) => {
        concludeRune(rune, KindTemplataType)
        Ok(())
      }
      case IsStructSR(_, rune) => {
        concludeRune(rune, KindTemplataType)
        Ok(())
      }
      case CoerceToCoord(_, coordRune, kindRune) => {
        concludeRune(kindRune, KindTemplataType)
        concludeRune(coordRune, KindTemplataType)
        Ok(())
      }
      case LiteralSR(_, rune, literal) => {
        concludeRune(rune, Astronomer.translateRuneType(literal.getType()))
        Ok(())
      }
      case LookupSR(range, rune, AbsoluteNameSN(name)) => {
        // Lookup rules get their type from their rune, which is either populated beforehand,
        // or figured out from another rule, see AMPLR.
        // So here, we do nothing.
        Ok(())
      }
      case LookupSR(range, rune, ImpreciseNameSN(name @ CodeTypeNameS(_))) => {
        // Lookup rules get their type from their rune, which is either populated beforehand,
        // or figured out from another rule, see AMPLR.
        // So here, we do nothing.
        Ok(())
      }
      case LookupSR(range, rune, ImpreciseNameSN(_)) => vimpl()
      case AugmentSR(_, resultRune, literals, innerRune) => vimpl()
      case PackSR(_, resultRune, members) => vimpl()
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => vimpl()
      case ManualSequenceSR(_, resultRune, elements) => vimpl()
      case CoordListSR(_, resultRune, elements) => vimpl()
    }
  }

  def solve(
    astrouts: AstroutsBox,
    env: Environment,
    predicting: Boolean,
    rules: IndexedSeq[IRulexSR],
    // Some runes don't appear in the rules, for example if they are in the identifying runes,
    // but not in any of the members or rules.
    additionalRunes: Iterable[IRuneS],
    expectCompleteSolve: Boolean,
    initiallyKnownRunes: Map[IRuneS, ITemplataType]):
  Result[Map[IRuneS, ITemplataType], AstronomySolveError] = {
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules,
        additionalRunes,
        getRunes,
        (rule: IRulexSR) => getPuzzles(predicting, rule),
        initiallyKnownRunes.keySet,
        { case EqualsSR(_, a, b) => (a, b)}: PartialFunction[IRulexSR, (IRuneS, IRuneS)])
    val conclusions =
      Solver.solve[IRulexSR, IRuneS, Environment, AstroutsBox, ITemplataType, Unit](
        astrouts, env, numCanonicalRunes, rules, ruleExecutionOrder,
        getRunes,
        userRuneToCanonicalRune,
        userRuneToCanonicalRune.keys,
        initiallyKnownRunes,
        solveRule
      ) match {
        case Ok(c) => c.toMap
        case Err(e) => vfail(e)
      }
    if (expectCompleteSolve && (conclusions.keySet != userRuneToCanonicalRune.keySet)) {
      Err(AstronomySolveError(userRuneToCanonicalRune.keySet -- conclusions.keySet))
    } else {
      Ok(conclusions)
    }
  }
}