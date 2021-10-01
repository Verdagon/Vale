package net.verdagon.vale.templar.infer

import net.verdagon.vale._
import net.verdagon.vale.parser.{ConstraintP, ShareP}
import net.verdagon.vale.scout.{CodeTypeNameS, INameS, IRuneS, RangeS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.solver.{CompleteSolve, FailedSolve, ISolverOutcome, ISolverStateForRule, IncompleteSolve, Planner, RuleError, SolverConflict}
import net.verdagon.vale.templar.FunctionNameT
import net.verdagon.vale.templar.templata.{Conversions, CoordListTemplata, CoordTemplata, ITemplata, IntegerTemplata, InterfaceTemplata, KindTemplata, MutabilityTemplata, OwnershipTemplata, PermissionTemplata, PrototypeT, PrototypeTemplata, RuntimeSizedArrayTemplateTemplata, StaticSizedArrayTemplateTemplata, StringTemplata, StructTemplata, VariabilityTemplata}
import net.verdagon.vale.templar.types._

sealed trait ITemplarSolverError
case class KindIsNotConcrete(kind: KindT) extends ITemplarSolverError
case class KindIsNotInterface(kind: KindT) extends ITemplarSolverError
case class KindIsNotStruct(kind: KindT) extends ITemplarSolverError
case class CantShareMutable(kind: KindT) extends ITemplarSolverError
case class OwnershipDidntMatch(coord: CoordT, expectedOwnership: OwnershipT) extends ITemplarSolverError
case class PermissionDidntMatch(coord: CoordT, expectedPermission: PermissionT) extends ITemplarSolverError
case class CallResultWasntExpectedType(expected: ITemplata, actual: ITemplata) extends ITemplarSolverError {
  vpass()
}
case class OneOfFailed(rule: OneOfSR) extends ITemplarSolverError

class TemplarSolver[Env, State](
  delegate: IInfererDelegate[Env, State]
) {

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

  def getPuzzles(rule: IRulexSR): Array[Array[IRuneS]] = {
    rule match {
      // This means we can solve this puzzle and dont need anything to do it.
      case LookupSR(_, _, _) | KindLookupSR(_, _, _) => Array(Array())
      case CallSR(range, resultRune, templateRune, args) => {
        Array(
          Array(resultRune.rune, templateRune.rune),
          Array(templateRune.rune) ++ args.map(_.rune))
//          Array(resultRune.rune) ++ args.map(_.rune))
      }
      case PackSR(_, resultRune, members) => Array(Array(resultRune.rune), members.map(_.rune))
      case KindComponentsSR(_, kindRune, mutabilityRune) => Array(Array(kindRune.rune))
      case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => Array(Array(resultRune.rune), Array(ownershipRune.rune, permissionRune.rune, kindRune.rune))
      // Notice how there is no return rune in here; we can solve the entire rule with just the name and the parameter list.
      case PrototypeComponentsSR(range, resultRune, nameRune, paramListRune, returnRune) => Array(Array(resultRune.rune), Array(nameRune.rune, paramListRune.rune))
      case OneOfSR(_, rune, literals) => Array(Array(rune.rune))
      case EqualsSR(_, leftRune, rightRune) => Array(Array(leftRune.rune), Array(rightRune.rune))
      case IsConcreteSR(_, rune) => Array(Array(rune.rune))
      case IsInterfaceSR(_, rune) => Array(Array(rune.rune))
      case IsStructSR(_, rune) => Array(Array(rune.rune))
      case CoerceToCoord(_, coordRune, kindRune) => Array(Array())
      case LiteralSR(_, rune, literal) => Array(Array())
      case AugmentSR(_, resultRune, literals, innerRune) => Array(Array(innerRune.rune), Array(resultRune.rune))
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(Array())
      case ManualSequenceSR(_, resultRune, elements) => Array(Array())
      case CoordListSR(_, resultRune, elements) => Array(Array())
    }
  }

  private def solveRule(
    state: State,
    env: Env,
    ruleIndex: Int,
    rule: IRulexSR,
    solverState: ISolverStateForRule[IRulexSR, IRuneS, ITemplata]):
  // One might expect us to return the conclusions in this Result. Instead we take in a
  // lambda to avoid intermediate allocations, for speed.
  Result[Map[IRuneS, ITemplata], ITemplarSolverError] = {
    rule match {
      case KindComponentsSR(range, kindRune, mutabilityRune) => {
        val KindTemplata(kind) = vassertSome(solverState.getConclusion(kindRune.rune))
        val mutability = delegate.getMutability(state, kind)
        Ok(Map(mutabilityRune.rune -> MutabilityTemplata(mutability)))
      }
      case CoordComponentsSR(_, resultRune, ownershipRune, permissionRune, kindRune) => {
        solverState.getConclusion(resultRune.rune) match {
          case None => {
            val OwnershipTemplata(ownership) = vassertSome(solverState.getConclusion(ownershipRune.rune))
            val PermissionTemplata(permission) = vassertSome(solverState.getConclusion(permissionRune.rune))
            val KindTemplata(kind) = vassertSome(solverState.getConclusion(kindRune.rune))
            val newCoord = CoordT(ownership, permission, kind)
            Ok(Map(resultRune.rune -> CoordTemplata(newCoord)))
          }
          case Some(coord) => {
            val CoordTemplata(CoordT(ownership, permission, kind)) = coord
            Ok(
              Map(
                ownershipRune.rune -> OwnershipTemplata(ownership),
                permissionRune.rune -> PermissionTemplata(permission),
                kindRune.rune -> KindTemplata(kind)))
          }
        }
      }
      case PrototypeComponentsSR(_, resultRune, nameRune, paramListRune, returnRune) => {
        solverState.getConclusion(resultRune.rune) match {
          case None => {
            val StringTemplata(name) = vassertSome(solverState.getConclusion(nameRune.rune))
            val CoordListTemplata(coords) = vassertSome(solverState.getConclusion(paramListRune.rune))
            val prototype = delegate.resolveExactSignature(env, state, rule.range, name, coords)
            Ok(
              Map(
                resultRune.rune -> PrototypeTemplata(prototype),
                returnRune.rune -> CoordTemplata(prototype.returnType)))
          }
          case Some(prototype) => {
            val PrototypeTemplata(PrototypeT(fullName, returnType)) = prototype
            val humanName =
              fullName.last match {
                case FunctionNameT(humanName, _, _) => humanName
              }
            Ok(
              Map(
                nameRune.rune -> StringTemplata(humanName),
                returnRune.rune -> CoordTemplata(returnType)))
          }
        }
      }
      case EqualsSR(_, leftRune, rightRune) => {
        solverState.getConclusion(leftRune.rune) match {
          case None => Ok(Map(leftRune.rune -> vassertSome(solverState.getConclusion(rightRune.rune))))
          case Some(left) => Ok(Map(rightRune.rune -> left))
        }
      }
      case rule @ OneOfSR(_, resultRune, literals) => {
        val result = vassertSome(solverState.getConclusion(resultRune.rune))
        val templatas = literals.map(literalToTemplata)
        if (templatas.contains(result)) {
          Ok(Map())
        } else {
          Err(OneOfFailed(rule))
        }
      }
      case rule @ IsConcreteSR(_, rune) => {
        val templata = vassertSome(solverState.getConclusion(rune.rune))
        templata match {
          case KindTemplata(kind) => {
            kind match {
              case InterfaceTT(_) => {
                Err(KindIsNotConcrete(kind))
              }
              case _ => Ok(Map())
            }
          }
          case _ => vwat() // Should be impossible, all template rules are type checked
        }
      }
      case rule @ IsInterfaceSR(_, rune) => {
        val templata = vassertSome(solverState.getConclusion(rune.rune))
        templata match {
          case KindTemplata(kind) => {
            kind match {
              case InterfaceTT(_) => Ok(Map())
              case _ => Err(KindIsNotInterface(kind))
            }
          }
          case _ => vwat() // Should be impossible, all template rules are type checked
        }
      }
      case IsStructSR(_, rune) => {
        val templata = vassertSome(solverState.getConclusion(rune.rune))
        templata match {
          case KindTemplata(kind) => {
            kind match {
              case StructTT(_) => Ok(Map())
              case _ => Err(KindIsNotStruct(kind))
            }
          }
          case _ => vwat() // Should be impossible, all template rules are type checked
        }
      }
      case CoerceToCoord(_, coordRune, kindRune) => {
        vimpl()
//        Ok(Map(kindRune.rune, KindTemplataType))
//        Ok(Map(coordRune.rune, CoordTemplataType))
      }
      case LiteralSR(_, rune, literal) => {
        val templata = literalToTemplata(literal)
        Ok(Map(rune.rune -> templata))
      }
      case LookupSR(range, rune, name) => {
        val result = delegate.lookupTemplataImprecise(env, state, range, name)
        Ok(Map(rune.rune -> result))
      }
      case KindLookupSR(range, rune, name) => {
        val result = delegate.lookupTemplataImprecise(env, state, range, name)
        Ok(Map(rune.rune -> result))
      }
      case AugmentSR(_, resultRune, literals, innerRune) => {
        solverState.getConclusion(innerRune.rune) match {
          case Some(CoordTemplata(initialCoord)) => {
            val newCoord =
              literals.foldLeft(initialCoord)({
                case (coord, OwnershipLiteralSL(newOwnership)) => {
                  delegate.getMutability(state, coord.kind) match {
                    case MutableT => {
                      if (newOwnership == ShareP) {
                        return Err(CantShareMutable(coord.kind))
                      }
                      coord.copy(ownership = Conversions.evaluateOwnership(newOwnership))
                    }
                    case ImmutableT => coord
                  }
                }
                case (coord, PermissionLiteralSL(newPermission)) => {
                  delegate.getMutability(state, coord.kind) match {
                    case MutableT => coord.copy(permission = Conversions.evaluatePermission(newPermission))
                    case ImmutableT => coord
                  }
                }
              })
            Ok(Map(resultRune.rune -> CoordTemplata(newCoord)))
          }
          case None => {
            val CoordTemplata(initialCoord) = vassertSome(solverState.getConclusion(resultRune.rune))
            val newCoord =
              literals.foldLeft(initialCoord)({
                case (coord, OwnershipLiteralSL(requiredOwnership)) => {
                  delegate.getMutability(state, coord.kind) match {
                    case MutableT => {
                      if (requiredOwnership == ShareP) {
                        return Err(CantShareMutable(coord.kind))
                      }
                      if (coord.ownership == Conversions.evaluateOwnership(requiredOwnership)) {
                        coord.copy(ownership = OwnT)
                      } else {
                        return Err(OwnershipDidntMatch(coord, Conversions.evaluateOwnership(requiredOwnership)))
                      }
                    }
                    case ImmutableT => coord
                  }
                }
                case (coord, PermissionLiteralSL(requiredPermission)) => {
                  delegate.getMutability(state, coord.kind) match {
                    case MutableT => {
                      if (coord.permission == Conversions.evaluatePermission(requiredPermission)) {
                        coord.copy(permission = ReadwriteT)
                      } else {
                        return Err(PermissionDidntMatch(coord, Conversions.evaluatePermission(requiredPermission)))
                      }
                    }
                    case ImmutableT => coord
                  }
                }
              })
            Ok(Map(innerRune.rune -> CoordTemplata(newCoord)))
          }
        }

      }
      case PackSR(_, resultRune, memberRunes) => {
        solverState.getConclusion(resultRune.rune) match {
          case None => {
            val members =
              memberRunes.map(memberRune => {
                val CoordTemplata(coord) = vassertSome(solverState.getConclusion(memberRune.rune))
                coord
              })
            Ok(Map(resultRune.rune -> CoordListTemplata(members.toVector)))
          }
          case Some(CoordListTemplata(members)) => {
            vassert(members.size == memberRunes.size)
            Ok(memberRunes.map(_.rune).zip(members.map(CoordTemplata)).toMap)
          }
        }
      }
      case RepeaterSequenceSR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
        solverState.getConclusion(resultRune.rune) match {
          case None => {
            vimpl()
          }
          case Some(result) => {
            result match {
              case KindTemplata(StaticSizedArrayTT(size, RawArrayTT(elementType, mutability, variability))) => {
                Ok(
                  Map(
                    elementRune.rune -> CoordTemplata(elementType),
                    sizeRune.rune -> IntegerTemplata(size),
                    mutabilityRune.rune -> MutabilityTemplata(mutability),
                    variabilityRune.rune -> VariabilityTemplata(variability)))
              }
              case CoordTemplata(CoordT(OwnT | ShareT, _, StaticSizedArrayTT(size, RawArrayTT(elementType, mutability, variability)))) => {
                Ok(
                  Map(
                    elementRune.rune -> CoordTemplata(elementType),
                    sizeRune.rune -> IntegerTemplata(size),
                    mutabilityRune.rune -> MutabilityTemplata(mutability),
                    variabilityRune.rune -> VariabilityTemplata(variability)))
              }
              case _ => return Err(CallResultWasntExpectedType(StaticSizedArrayTemplateTemplata(), result))
            }
          }
        }
      }
      case ManualSequenceSR(_, resultRune, elements) => vimpl()
      case CoordListSR(_, resultRune, elements) => vimpl()
      case CallSR(range, resultRune, templateRune, argRunes) => {
        val template = vassertSome(solverState.getConclusion(templateRune.rune))
        solverState.getConclusion(resultRune.rune) match {
          case Some(result) => {
            template match {
              case RuntimeSizedArrayTemplateTemplata() => {
                result match {
                  case CoordTemplata(CoordT(ShareT | OwnT, _, RuntimeSizedArrayTT(RawArrayTT(memberType, mutability, variability)))) => {
                    val Array(mutabilityRune, variabilityRune, elementRune) = argRunes
                    Ok(
                      Map(
                        mutabilityRune.rune -> MutabilityTemplata(mutability),
                        variabilityRune.rune -> VariabilityTemplata(variability),
                        elementRune.rune -> CoordTemplata(memberType)))
                  }
                  case KindTemplata(RuntimeSizedArrayTT(RawArrayTT(memberType, mutability, variability))) => {
                    val Array(mutabilityRune, variabilityRune, elementRune) = argRunes
                    Ok(
                      Map(
                        mutabilityRune.rune -> MutabilityTemplata(mutability),
                        variabilityRune.rune -> VariabilityTemplata(variability),
                        elementRune.rune -> CoordTemplata(memberType)))
                  }
                  case _ => return Err(CallResultWasntExpectedType(template, result))
                }
              }
              case it @ InterfaceTemplata(_, _) => {
                result match {
                  case KindTemplata(interface @ InterfaceTT(_)) => {
                    if (!delegate.citizenIsFromTemplate(interface, it)) {
                      return Err(CallResultWasntExpectedType(it, result))
                    }
                    vassert(argRunes.size == interface.fullName.last.templateArgs.size)
                    Ok(argRunes.map(_.rune).zip(interface.fullName.last.templateArgs).toMap)
                  }
                  case CoordTemplata(CoordT(OwnT | ShareT, _, interface @ InterfaceTT(_))) => {
                    if (!delegate.citizenIsFromTemplate(interface, it)) {
                      return Err(CallResultWasntExpectedType(it, result))
                    }
                    vassert(argRunes.size == interface.fullName.last.templateArgs.size)
                    Ok(argRunes.map(_.rune).zip(interface.fullName.last.templateArgs).toMap)
                  }
                  case _ => return Err(CallResultWasntExpectedType(template, result))
                }
              }
            }
          }
          case None => {
            template match {
              case RuntimeSizedArrayTemplateTemplata() => {
                val args = argRunes.map(argRune => vassertSome(solverState.getConclusion(argRune.rune)))
                val Array(MutabilityTemplata(mutability), VariabilityTemplata(variability), CoordTemplata(coord)) = args
                val rsaKind = delegate.getRuntimeSizedArrayKind(env, state, coord, mutability, variability)
                Ok(Map(resultRune.rune -> KindTemplata(rsaKind)))
              }
              case it @ StructTemplata(_, _) => {
                val args = argRunes.map(argRune => vassertSome(solverState.getConclusion(argRune.rune)))
                val kind = delegate.evaluateStructTemplata(state, range, it, args.toVector)
                Ok(Map(resultRune.rune -> KindTemplata(kind)))
              }
              case it @ InterfaceTemplata(_, _) => {
                val args = argRunes.map(argRune => vassertSome(solverState.getConclusion(argRune.rune)))
                val kind = delegate.evaluateInterfaceTemplata(state, range, it, args.toVector)
                Ok(Map(resultRune.rune -> KindTemplata(kind)))
              }
//              case TemplateTemplataType(paramTypes, returnType) => {
//                val effectiveReturnType =
//                  returnType match {
//                    case KindTemplataType => CoordTemplataType
//                    case other => other
//                  }
//                Ok(Map(resultRune.rune, effectiveReturnType))
//
//                argRunes.zip(paramTypes).foreach({ case (argRune, paramType) =>
//                  Ok(Map(argRune.rune, paramType))
//                })
//              }
              case _ => vimpl()
            }
          }
        }
      }
    }
  }

  private def literalToTemplata(literal: ILiteralSL) = {
    literal match {
      case PermissionLiteralSL(permission) => PermissionTemplata(Conversions.evaluatePermission(permission))
      case MutabilityLiteralSL(mutability) => MutabilityTemplata(Conversions.evaluateMutability(mutability))
      case OwnershipLiteralSL(ownership) => OwnershipTemplata(Conversions.evaluateOwnership(ownership))
      case VariabilityLiteralSL(variability) => VariabilityTemplata(Conversions.evaluateVariability(variability))
      case StringLiteralSL(string) => StringTemplata(string)
    }
  }

  def solve(
    range: RangeS,
    env: Env,
    state: State,
    rules: IndexedSeq[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    initiallyKnownRuneToTemplata: Map[IRuneS, ITemplata]):
  ISolverOutcome[IRulexSR, IRuneS, ITemplata, ITemplarSolverError] = {

    rules.flatMap(_.runeUsages.map(_.rune)).foreach(rune => vassert(runeToType.contains(rune)))

    initiallyKnownRuneToTemplata.foreach({ case (rune, templata) =>
      vassert(templata.tyype == vassertSome(runeToType.get(rune)))
    })

    val solverState =
      Planner.makeInitialSolverState(
        rules,
        getRunes,
        (rule: IRulexSR) => getPuzzles(rule),
        initiallyKnownRuneToTemplata)
    Planner.solve[IRulexSR, IRuneS, Env, State, ITemplata, ITemplarSolverError](
      state,
      env,
      solverState,
      (
        state: State,
        env: Env,
        ruleIndex: Int,
        rule: IRulexSR,
        solverState: ISolverStateForRule[IRulexSR, IRuneS, ITemplata]) =>
      {
        solveRule(state, env, ruleIndex, rule, solverState) match {
          case Err(e) => Err(e)
          case Ok(conclusions) => {
            Ok(
              conclusions.map({ case (rune, conclusion) =>
                val coerced =
                  delegate.coerce(env, state, range, vassertSome(runeToType.get(rune)), conclusion)
                vassert(coerced.tyype == vassertSome(runeToType.get(rune)))
                (rune -> coerced)
              }))
          }
        }
      }
    ) match {
      case Err(f @ FailedSolve(_, _, _)) => f
      case Ok(conclusionsStream) => {
        val conclusions = conclusionsStream.toMap
        if (conclusions.keySet != solverState.getAllRunes()) {
          IncompleteSolve(
            conclusions,
            solverState.getUnsolvedRules(),
            solverState.getAllRunes() -- conclusions.keySet)
        } else {
          CompleteSolve(conclusions)
        }
      }
    }
  }
}
