package net.verdagon.vale.scout.predictor

import net.verdagon.vale._
import net.verdagon.vale.scout.{CodeTypeNameS, INameS, IRuneS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.solver.{Planner, Solver}
import net.verdagon.vale.templar.types._

case class AstronomySolveError(unknownRunes: Iterable[IRuneS])
object AstronomySolver {
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
          // This means we can solve this puzzle and dont need anything to do it.
          Array(Array())
        }
      }
      case CallSR(range, resultRune, templateRune, args) => {
        Array(
          Array(resultRune, templateRune),
          Array(templateRune) ++ args,
          Array(resultRune) ++ args)
      }
      case PackSR(_, resultRune, members) => Array(Array(resultRune), members)
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
    state: Unit,
    env: INameS => ITemplataType,
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
      case CallSR(range, resultRune, templateRune, argRunes) => {
        getConclusion(templateRune) match {
          case None => {

          }
          case Some(templateType) => {
            templateType match {
              case TemplateTemplataType(paramTypes, returnType) => {
                val effectiveReturnType =
                  returnType match {
                    case KindTemplataType => CoordTemplataType
                    case other => other
                  }
                concludeRune(resultRune, effectiveReturnType)

                argRunes.zip(paramTypes).foreach({ case (argRune, paramType) =>
                  concludeRune(argRune, paramType)
                })
              }
              case _ => vimpl(); Err(())
            }
          }
        }
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
        val types = literals.map(_.getType()).toSet
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
        concludeRune(coordRune, CoordTemplataType)
        Ok(())
      }
      case LiteralSR(_, rune, literal) => {
        concludeRune(rune, literal.getType())
        Ok(())
      }
      case LookupSR(range, rune, name) => {
        val actualType =
          env(name) match {
            case KindTemplataType => CoordTemplataType
            case other => other
          }
        concludeRune(rune, actualType)
        Ok(())
      }
      case AugmentSR(_, resultRune, literals, innerRune) => {
        concludeRune(resultRune, CoordTemplataType)
        concludeRune(innerRune, CoordTemplataType)
        Ok(())
      }
      case PackSR(_, resultRune, members) => vimpl()
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => vimpl()
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
      Solver.solve[IRulexSR, IRuneS, INameS => ITemplataType, Unit, ITemplataType, Unit](
        Unit,
        env,
        numCanonicalRunes, rules, ruleExecutionOrder,
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
