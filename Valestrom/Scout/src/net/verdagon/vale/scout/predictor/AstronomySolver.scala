package net.verdagon.vale.scout.predictor

import net.verdagon.vale._
import net.verdagon.vale.scout.{CodeTypeNameS, INameS, IRuneS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.solver.{Planner, Solver}
import net.verdagon.vale.templar.types._

case class AstronomySolveError(unknownRunes: Iterable[IRuneS])
object AstronomySolver {
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
    getConclusion: IRuneS => Option[ITemplataType],
    concludeRune: (IRuneS, ITemplataType) => Unit):
  Result[Unit, Unit] = {
    rule match {
      case KindComponentsSR(range, resultRune, mutabilityRune) => {
        concludeRune(resultRune.rune, KindTemplataType)
        concludeRune(mutabilityRune.rune, MutabilityTemplataType)
        Ok(())
      }
      case CallSR(range, resultRune, templateRune, argRunes) => {
        getConclusion(templateRune.rune) match {
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
                concludeRune(resultRune.rune, effectiveReturnType)

                argRunes.zip(paramTypes).foreach({ case (argRune, paramType) =>
                  concludeRune(argRune.rune, paramType)
                })
              }
              case _ => vimpl(); Err(())
            }
          }
        }
        Ok(())
      }
      case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => {
        concludeRune(resultRune.rune, CoordTemplataType)
        concludeRune(ownershipRune.rune, OwnershipTemplataType)
        concludeRune(permissionRune.rune, PermissionTemplataType)
        concludeRune(kindRune.rune, KindTemplataType)
        Ok(())
      }
      case PrototypeComponentsSR(_, resultRune, nameRune, paramListRune, returnRune) => {
        concludeRune(resultRune.rune, PrototypeTemplataType)
        concludeRune(nameRune.rune, StringTemplataType)
        concludeRune(paramListRune.rune, PackTemplataType(CoordTemplataType))
        concludeRune(returnRune.rune, CoordTemplataType)
        Ok(())
      }
      case OneOfSR(_, resultRune, literals) => {
        val types = literals.map(_.getType()).toSet
        if (types.size > 1) {
          vfail("OneOf rule's possibilities must all be the same type!")
        }
        concludeRune(resultRune.rune, types.head)
        Ok(())
      }
      case IsConcreteSR(_, rune) => {
        concludeRune(rune.rune, KindTemplataType)
        Ok(())
      }
      case IsInterfaceSR(_, rune) => {
        concludeRune(rune.rune, KindTemplataType)
        Ok(())
      }
      case IsStructSR(_, rune) => {
        concludeRune(rune.rune, KindTemplataType)
        Ok(())
      }
      case CoerceToCoord(_, coordRune, kindRune) => {
        concludeRune(kindRune.rune, KindTemplataType)
        concludeRune(coordRune.rune, CoordTemplataType)
        Ok(())
      }
      case LiteralSR(_, rune, literal) => {
        concludeRune(rune.rune, literal.getType())
        Ok(())
      }
      case LookupSR(range, rune, name) => {
        val actualType =
          env(name) match {
            case KindTemplataType => CoordTemplataType
            case other => other
          }
        concludeRune(rune.rune, actualType)
        Ok(())
      }
      case KindLookupSR(range, rune, name) => {
        concludeRune(rune.rune, KindTemplataType)
        Ok(())
      }
      case AugmentSR(_, resultRune, literals, innerRune) => {
        concludeRune(resultRune.rune, CoordTemplataType)
        concludeRune(innerRune.rune, CoordTemplataType)
        Ok(())
      }
      case PackSR(_, resultRune, memberRunes) => {
        getConclusion(resultRune.rune) match {
          case Some(PackTemplataType(elementType)) => {
            memberRunes.foreach(memberRune => concludeRune(memberRune.rune, elementType))
          }
          case Some(_) => {
            vfail("Pack rule's result must be a pack!")
          }
          case None => {
            val memberTypes = memberRunes.map(memberRune => vassertSome(getConclusion(memberRune.rune)))
            val distinctMemberTypes = memberTypes.distinct
            val memberType =
              if (distinctMemberTypes.size != 1) {
                vfail("Pack's members must all be the same type! Instead, got: " + memberTypes.mkString(""))
              } else {
                distinctMemberTypes.head
              }
            concludeRune(resultRune.rune, PackTemplataType(memberType))
          }
        }
        Ok(())
      }
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
        concludeRune(resultRune.rune, CoordTemplataType)
        concludeRune(mutabilityRune.rune, MutabilityTemplataType)
        concludeRune(variabilityRune.rune, VariabilityTemplataType)
        concludeRune(sizeRune.rune, IntegerTemplataType)
        concludeRune(elementRune.rune, CoordTemplataType)
        Ok(())
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
  Result[Map[IRuneS, ITemplataType], AstronomySolveError] = {
    val (numCanonicalRunes, userRuneToCanonicalRune, ruleExecutionOrder, canonicalRuneToIsSolved) =
      Planner.plan(
        rules,
        additionalRunes,
        getRunes,
        (rule: IRulexSR) => getPuzzles(predicting, rule),
        initiallyKnownRunes.keySet,
        { case EqualsSR(_, a, b) => (a.rune, b.rune)}: PartialFunction[IRulexSR, (IRuneS, IRuneS)])
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
