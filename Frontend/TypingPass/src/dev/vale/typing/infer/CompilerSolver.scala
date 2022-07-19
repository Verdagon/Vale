package dev.vale.typing.infer

import dev.vale.options.GlobalOptions
import dev.vale.parsing.ast.ShareP
import dev.vale.postparsing.rules._
import dev.vale.{Err, Ok, RangeS, Result, vassert, vassertSome, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.solver.{CompleteSolve, FailedSolve, ISolveRule, ISolverError, ISolverOutcome, ISolverState, IStepState, IncompleteSolve, RuleError, Solver, SolverConflict}
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.ast.PrototypeT
import dev.vale.typing.names.{CitizenNameT, FullNameT, FunctionNameT, IFunctionNameT, INameT}
import dev.vale.typing.templata.{Conversions, PlaceholderTemplata, _}
import dev.vale.typing.types._
import dev.vale._
import dev.vale.postparsing.ArgumentRuneS
import dev.vale.postparsing.rules._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.{templata, types}
import dev.vale.typing.types._

import scala.collection.immutable.HashSet
import scala.collection.mutable

sealed trait ITypingPassSolverError
case class KindIsNotConcrete(kind: KindT) extends ITypingPassSolverError
case class KindIsNotInterface(kind: KindT) extends ITypingPassSolverError
case class KindIsNotStruct(kind: KindT) extends ITypingPassSolverError
case class CouldntFindFunction(range: RangeS, fff: FindFunctionFailure) extends ITypingPassSolverError {
  vpass()
}
case class CantShareMutable(kind: KindT) extends ITypingPassSolverError
case class CantSharePlaceholder(kind: KindT) extends ITypingPassSolverError
case class SendingNonCitizen(kind: KindT) extends ITypingPassSolverError
case class CantCheckPlaceholder(range: RangeS) extends ITypingPassSolverError
case class ReceivingDifferentOwnerships(params: Vector[(IRuneS, CoordT)]) extends ITypingPassSolverError
case class SendingNonIdenticalKinds(sendCoord: CoordT, receiveCoord: CoordT) extends ITypingPassSolverError
case class NoCommonAncestors(params: Vector[(IRuneS, CoordT)]) extends ITypingPassSolverError
case class LookupFailed(name: IImpreciseNameS) extends ITypingPassSolverError
case class NoAncestorsSatisfyCall(params: Vector[(IRuneS, CoordT)]) extends ITypingPassSolverError
case class CantDetermineNarrowestKind(kinds: Set[KindT]) extends ITypingPassSolverError
case class OwnershipDidntMatch(coord: CoordT, expectedOwnership: OwnershipT) extends ITypingPassSolverError
case class CallResultWasntExpectedType(expected: ITemplata[ITemplataType], actual: ITemplata[ITemplataType]) extends ITypingPassSolverError
case class OneOfFailed(rule: OneOfSR) extends ITypingPassSolverError
case class KindDoesntImplementInterface(sub: CitizenRefT, suuper: InterfaceTT) extends ITypingPassSolverError
case class WrongNumberOfTemplateArgs(expectedNumArgs: Int) extends ITypingPassSolverError
case class FunctionDoesntHaveName(range: RangeS, name: IFunctionNameT) extends ITypingPassSolverError
case class CantGetComponentsOfPlaceholderPrototype(range: RangeS) extends ITypingPassSolverError

trait IInfererDelegate[Env, State] {
  def lookupMemberTypes(
    state: State,
    kind: KindT,
    // This is here so that the predictor can just give us however many things
    // we expect.
    expectedNumMembers: Int
  ): Option[Vector[CoordT]]

  def getMutability(state: State, kind: KindT): ITemplata[MutabilityTemplataType]

  def lookupTemplata(env: Env, state: State, range: RangeS, name: INameT): ITemplata[ITemplataType]

  def lookupTemplataImprecise(env: Env, state: State, range: RangeS, name: IImpreciseNameS): Option[ITemplata[ITemplataType]]

  def coerce(env: Env, state: State, range: RangeS, toType: ITemplataType, templata: ITemplata[ITemplataType]): ITemplata[ITemplataType]

  def isDescendant(env: Env, state: State, kind: KindT): Boolean
  def isAncestor(env: Env, state: State, kind: KindT): Boolean

  def resolveStruct(
    env: Env,
    state: State,
    callRange: RangeS,
    templata: StructTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (KindT)

  def resolveInterface(
    env: Env,
    state: State,
    callRange: RangeS,
    templata: InterfaceTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (KindT)

  def getStaticSizedArrayKind(
    env: Env,
    state: State,
    mutability: ITemplata[MutabilityTemplataType],
    variability: ITemplata[VariabilityTemplataType],
    size: ITemplata[IntegerTemplataType],
    element: CoordT):
  StaticSizedArrayTT

  def getRuntimeSizedArrayKind(env: Env, state: State, type2: CoordT, arrayMutability: ITemplata[MutabilityTemplataType]): RuntimeSizedArrayTT

  def getAncestors(coutputs: State, descendant: KindT, includeSelf: Boolean):
  (Set[KindT])

  def getInterfaceTemplataType()(it: InterfaceTemplata): ITemplataType
  def getStructTemplataType()(st: StructTemplata): ITemplataType

  def getMemberCoords(state: State, structTT: StructTT): Vector[CoordT]

  def structIsClosure(state: State, structTT: StructTT): Boolean

  def kindIsFromTemplate(
    state: State,
    actualCitizenRef: KindT,
    expectedCitizenTemplata: ITemplata[ITemplataType]):
  Boolean

  def resolveFunction(
    env: Env,
    state: State,
    range: RangeS,
    name: StrI,
    coords: Vector[CoordT]):
  Result[PrototypeTemplata, FindFunctionFailure]

  def assemblePrototype(
    env: Env,
    state: State,
    range: RangeS,
    name: StrI,
    coords: Vector[CoordT],
    returnType: CoordT):
  PrototypeT
}

class CompilerSolver[Env, State](
  globalOptions: GlobalOptions,
  interner: Interner,
  delegate: IInfererDelegate[Env, State]
) {

  def getRunes(rule: IRulexSR): Array[IRuneS] = {
    val result = rule.runeUsages.map(_.rune)

    if (globalOptions.sanityCheck) {
      val sanityChecked: Array[RuneUsage] =
        rule match {
          case LookupSR(range, rune, literal) => Array(rune)
          case LookupSR(range, rune, literal) => Array(rune)
          case RuneParentEnvLookupSR(range, rune) => Array(rune)
          case EqualsSR(range, left, right) => Array(left, right)
          case CoordIsaSR(range, sub, suuper) => Array(sub, suuper)
          case KindComponentsSR(range, resultRune, mutabilityRune) => Array(resultRune, mutabilityRune)
          case CoordComponentsSR(range, resultRune, ownershipRune, kindRune) => Array(resultRune, ownershipRune, kindRune)
          case PrototypeComponentsSR(range, resultRune, paramsRune, returnRune) => Array(resultRune, paramsRune, returnRune)
          case DefinitionFuncSR(range, resultRune, name, paramsListRune, returnRune) => Array(resultRune, paramsListRune, returnRune)
          case CallSiteFuncSR(range, resultRune, name, paramsListRune, returnRune) => Array(resultRune, paramsListRune, returnRune)
          case ResolveSR(range, resultRune, name, paramsListRune) => Array(resultRune, paramsListRune)
          case OneOfSR(range, rune, literals) => Array(rune)
          case IsConcreteSR(range, rune) => Array(rune)
          case IsInterfaceSR(range, rune) => Array(rune)
          case IsStructSR(range, rune) => Array(rune)
          case CoerceToCoordSR(range, coordRune, kindRune) => Array(coordRune, kindRune)
          case LiteralSR(range, rune, literal) => Array(rune)
          case AugmentSR(range, resultRune, ownership, innerRune) => Array(resultRune, innerRune)
          case CallSR(range, resultRune, templateRune, args) => Array(resultRune, templateRune) ++ args
//          case PrototypeSR(range, resultRune, name, parameters, returnTypeRune) => Array(resultRune) ++ parameters ++ Array(returnTypeRune)
          case PackSR(range, resultRune, members) => Array(resultRune) ++ members
          case StaticSizedArraySR(range, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune)
          case RuntimeSizedArraySR(range, resultRune, mutabilityRune, elementRune) => Array(resultRune, mutabilityRune, elementRune)
          //        case ManualSequenceSR(range, resultRune, elements) => Array(resultRune) ++ elements
          //        case CoordListSR(range, resultRune, elements) => Array(resultRune) ++ elements
          case CoordSendSR(range, senderRune, receiverRune) => Array(senderRune, receiverRune)
          case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => Array(resultRune, coordListRune)
        }
      vassert(result sameElements sanityChecked.map(_.rune))
    }
    result
  }

  def getPuzzles(rule: IRulexSR): Array[Array[IRuneS]] = {
    rule match {
      // This means we can solve this puzzle and dont need anything to do it.
      case LookupSR(_, _, _) => Array(Array())
      case RuneParentEnvLookupSR(_, rune) => Array(Array())
      case CallSR(range, resultRune, templateRune, args) => {
        Array(
          Array(resultRune.rune, templateRune.rune),
          Array(templateRune.rune) ++ args.map(_.rune))
//          Array(resultRune.rune) ++ args.map(_.rune))
      }
      case PackSR(_, resultRune, members) => Array(Array(resultRune.rune), members.map(_.rune))
      case KindComponentsSR(_, kindRune, mutabilityRune) => Array(Array(kindRune.rune))
      case CoordComponentsSR(_, resultRune, ownershipRune, kindRune) => Array(Array(resultRune.rune), Array(ownershipRune.rune, kindRune.rune))
      case PrototypeComponentsSR(_, resultRune, paramsRune, returnRune) => Array(Array(resultRune.rune))
      case CallSiteFuncSR(range, resultRune, name, paramListRune, returnRune) => Array(Array(resultRune.rune))
      // Definition doesn't need the placeholder to be present, it's what populates the placeholder.
      case DefinitionFuncSR(range, placeholderRune, name, paramListRune, returnRune) => Array(Array(paramListRune.rune, returnRune.rune))
      case ResolveSR(range, resultRune, name, paramsListRune) => Array(Array(paramsListRune.rune))
      case OneOfSR(_, rune, literals) => Array(Array(rune.rune))
      case EqualsSR(_, leftRune, rightRune) => Array(Array(leftRune.rune), Array(rightRune.rune))
      case IsConcreteSR(_, rune) => Array(Array(rune.rune))
      case IsInterfaceSR(_, rune) => Array(Array(rune.rune))
      case IsStructSR(_, rune) => Array(Array(rune.rune))
      case CoerceToCoordSR(_, coordRune, kindRune) => Array(Array(coordRune.rune), Array(kindRune.rune))
      case LiteralSR(_, rune, literal) => Array(Array())
      case AugmentSR(_, resultRune, ownership, innerRune) => Array(Array(innerRune.rune), Array(resultRune.rune))
      case StaticSizedArraySR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => Array(Array(resultRune.rune), Array(mutabilityRune.rune, variabilityRune.rune, sizeRune.rune, elementRune.rune))
      case RuntimeSizedArraySR(_, resultRune, mutabilityRune, elementRune) => Array(Array(resultRune.rune), Array(mutabilityRune.rune, elementRune.rune))
      // See SAIRFU, this will replace itself with other rules.
      case CoordSendSR(_, senderRune, receiverRune) => Array(Array(senderRune.rune), Array(receiverRune.rune))
      case CoordIsaSR(range, senderRune, receiverRune) => Array(Array(senderRune.rune, receiverRune.rune))
      case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => Array(Array(coordListRune.rune))
    }
  }

  def solve(
    range: RangeS,
    env: Env,
    state: State,
    rules: IndexedSeq[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    initiallyKnownRuneToTemplata: Map[IRuneS, ITemplata[ITemplataType]]):
  ISolverOutcome[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError] = {

    rules.foreach(rule => rule.runeUsages.foreach(rune => vassert(runeToType.contains(rune.rune))))

    // These two shouldn't both be in the rules, see SROACSD.
    vassert(
      rules.collect({ case CallSiteFuncSR(_, _, _, _, _) => }).isEmpty ||
      rules.collect({ case DefinitionFuncSR(_, _, _, _, _) => }).isEmpty)

    initiallyKnownRuneToTemplata.foreach({ case (rune, templata) =>
      vassert(templata.tyype == vassertSome(runeToType.get(rune)))
    })

    val solver =
      new Solver[IRulexSR, IRuneS, Env, State, ITemplata[ITemplataType], ITypingPassSolverError](
        globalOptions.sanityCheck, globalOptions.useOptimizedSolver)
    val solverState =
      solver.makeInitialSolverState(
        rules,
        getRunes,
        (rule: IRulexSR) => getPuzzles(rule),
        initiallyKnownRuneToTemplata)

    val ruleSolver = new CompilerRuleSolver(interner, delegate, runeToType)
    solver.solve(
        (rule: IRulexSR) => getPuzzles(rule),
        state,
        env,
        solverState,
        ruleSolver) match {
      case Err(f @ FailedSolve(_, _, _)) => f
      case Ok((stepsStream, conclusionsStream)) => {
        val conclusions = conclusionsStream.toMap
        val allRunes = runeToType.keySet ++ solverState.getAllRunes().map(solverState.getUserRune)
        if (conclusions.keySet != allRunes) {
          IncompleteSolve(
            stepsStream.toVector,
//            conclusions,
//            solverState.getAllRules(),
            solverState.getUnsolvedRules(),
            allRunes -- conclusions.keySet,
            conclusions)
        } else {
          CompleteSolve(conclusions)
        }
      }
    }
  }
}

class CompilerRuleSolver[Env, State](
    interner: Interner,
    delegate: IInfererDelegate[Env, State],
    runeToType: Map[IRuneS, ITemplataType])
  extends ISolveRule[IRulexSR, IRuneS, Env, State, ITemplata[ITemplataType], ITypingPassSolverError] {

  override def complexSolve(
      state: State,
      env: Env,
      solverState: ISolverState[IRulexSR, IRuneS, ITemplata[ITemplataType]],
      stepState: IStepState[IRulexSR, IRuneS, ITemplata[ITemplataType]]):
  Result[Unit, ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    val equivalencies = new Equivalencies(solverState.getUnsolvedRules())

    val unsolvedRules = solverState.getUnsolvedRules()
    val receiverRunes =
      equivalencies.getKindEquivalentRunes(
        unsolvedRules.collect({
          case CoordSendSR(_, _, receiverRune) => receiverRune.rune
          case CoordIsaSR(_, _, receiverRune) => receiverRune.rune
        }))

    val newConclusions =
      receiverRunes.flatMap(receiver => {
        val runesSendingToThisReceiver =
          equivalencies.getKindEquivalentRunes(
            unsolvedRules.collect({
              case CoordSendSR(_, s, r) if r.rune == receiver => s.rune
              case CoordIsaSR(_, s, r) if r.rune == receiver => s.rune
            }))
        val callRules =
          unsolvedRules.collect({ case z @ CallSR(_, r, _, _) if equivalencies.getKindEquivalentRunes(r.rune).contains(receiver) => z })
        val senderConclusions =
          runesSendingToThisReceiver
            .flatMap(senderRune => solverState.getConclusion(senderRune).map(senderRune -> _))
            .map({
              case (senderRune, CoordTemplata(coord)) => (senderRune -> coord)
              case other => vwat(other)
            })
            .toVector
        val callTemplates =
          equivalencies.getKindEquivalentRunes(
            callRules.map(_.templateRune.rune))
            .flatMap(solverState.getConclusion)
            .toVector
        vassert(callTemplates.distinct.size <= 1)
        // If true, there are some senders/constraints we don't know yet, so lets be
        // careful to not assume between any possibilities below.
        val allSendersKnown = senderConclusions.size == runesSendingToThisReceiver.size
        val allCallsKnown = callRules.size == callTemplates.size
        solveReceives(state, senderConclusions, callTemplates, allSendersKnown, allCallsKnown) match {
          case Err(e) => return Err(RuleError(e))
          case Ok(None) => None
          case Ok(Some(receiverInstantiationKind)) => {
            // We know the kind, but to really know the coord we have to look at all the rules that
            // factored into it, and may even have to default to something else.

            val possibleCoords =
              unsolvedRules.collect({
                case AugmentSR(range, resultRune, ownership, innerRune)
                  if resultRune.rune == receiver => {
                  types.CoordT(
                    Conversions.evaluateOwnership(ownership),
                    receiverInstantiationKind)
                }
              }) ++
                senderConclusions.map(_._2).map({ case CoordT(ownership, _) =>
                  types.CoordT(ownership, receiverInstantiationKind)
                })
            if (possibleCoords.nonEmpty) {
              val ownership =
                possibleCoords.map(_.ownership).distinct match {
                  case Vector() => vwat()
                  case Vector(ownership) => ownership
                  case _ => return Err(RuleError(ReceivingDifferentOwnerships(senderConclusions)))
                }
              Some(receiver -> CoordTemplata(types.CoordT(ownership, receiverInstantiationKind)))
            } else {
              // Just conclude a kind, which will coerce to an owning coord, and hope it's right.
              Some(receiver -> templata.KindTemplata(receiverInstantiationKind))
            }
          }
        }
      }).toMap

    newConclusions.foreach({ case (rune, conclusion) =>
      stepState.concludeRune[ITypingPassSolverError](rune, conclusion)
    })

    Ok(())
  }

  private def solveReceives(
    state: State,
    senders: Vector[(IRuneS, CoordT)],
    callTemplates: Vector[ITemplata[ITemplataType]],
    allSendersKnown: Boolean,
    allCallsKnown: Boolean):
  Result[Option[KindT], ITypingPassSolverError] = {
    val senderKinds = senders.map(_._2.kind)
    if (senderKinds.isEmpty) {
      return Ok(None)
    }

    // For example [Flamethrower, Rockets] becomes [[Flamethrower, IWeapon, ISystem], [Rockets, IWeapon, ISystem]]
    val senderAncestorLists = senderKinds.map(delegate.getAncestors(state, _, true))
    // Calculates the intersection of them all, eg [IWeapon, ISystem]
    val commonAncestors = senderAncestorLists.reduce(_.intersect(_))
    if (commonAncestors.size == 0) {
      return Err(NoCommonAncestors(senders))
    }
    // Filter by any call templates. eg if there's a X = ISystem:Y call, then we're now [ISystem]
    val commonAncestorsCallConstrained =
      if (callTemplates.isEmpty) {
        commonAncestors
      } else {
        commonAncestors.filter(ancestor => callTemplates.exists(template => delegate.kindIsFromTemplate(state,ancestor, template)))
      }

    val narrowedCommonAncestor =
      if (commonAncestorsCallConstrained.size == 0) {
        // If we get here, it means we passed in a bunch of nonsense that doesn't match our Call rules.
        // For example, passing in a Some<T> when a List<T> is expected.
        return Err(NoAncestorsSatisfyCall(senders))
      } else if (commonAncestorsCallConstrained.size == 1) {
        // If we get here, it doesn't matter if there are any other senders or calls, we know
        // it has to be this.
        // If we're wrong, it will be doublechecked by the solver anyway.
        commonAncestorsCallConstrained.head
      } else {
        if (!allSendersKnown) {
          // There are some senders out there, which might force us to choose one of the ancestors.
          // We don't know them yet, so we can't conclude anything.
          return Ok(None)
        }
        if (!allCallsKnown) {
          // There are some calls out there, which will determine which one of the possibilities it is.
          // We don't know them yet, so we can't conclude anything.
          return Ok(None)
        }
        // If there are multiple, like [IWeapon, ISystem], get rid of any that are parents of others, now [IWeapon].
        narrow(state, commonAncestorsCallConstrained) match {
          case Ok(x) => x
          case Err(e) => return Err(e)
        }
      }
    Ok(Some(narrowedCommonAncestor))
  }

  def narrow(
    state: State,
    kinds: Set[KindT]):
  Result[KindT, ITypingPassSolverError] = {
    vassert(kinds.size > 1)
    val narrowedAncestors = mutable.HashSet[KindT]()
    narrowedAncestors ++= kinds
    // Remove anything that's an ancestor of something else in the set
    kinds.foreach(kind => {
      narrowedAncestors --= delegate.getAncestors(state, kind, false)
    })
    if (narrowedAncestors.size == 0) {
      vwat() // Shouldnt happen
    } else if (narrowedAncestors.size == 1) {
      Ok(narrowedAncestors.head)
    } else {
      Err(CantDetermineNarrowestKind(narrowedAncestors.toSet))
    }
  }

  override def solve(
    state: State,
    env: Env,
    solverState: ISolverState[IRulexSR, IRuneS, ITemplata[ITemplataType]],
    ruleIndex: Int,
    rule: IRulexSR,
    stepState: IStepState[IRulexSR, IRuneS, ITemplata[ITemplataType]]):
  Result[Unit, ISolverError[IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    solveRule(state, env, ruleIndex, rule, new IStepState[IRulexSR, IRuneS, ITemplata[ITemplataType]] {
      override def addRule(rule: IRulexSR): Unit = stepState.addRule(rule)
      override def getConclusion(rune: IRuneS): Option[ITemplata[ITemplataType]] = stepState.getConclusion(rune)
      override def getUnsolvedRules(): Vector[IRulexSR] = stepState.getUnsolvedRules()
      override def concludeRune[ErrType](rune: IRuneS, conclusion: ITemplata[ITemplataType]): Unit = {
        val coerced =
          delegate.coerce(
            env,
            state,
            RangeS.internal(interner, -6434324),
            vassertSome(runeToType.get(rune)),
            conclusion)
        vassert(coerced.tyype == vassertSome(runeToType.get(rune)))
        stepState.concludeRune[ErrType](rune, coerced)
      }
    }) match {
      case Ok(x) => Ok(x)
      case Err(e) => Err(RuleError(e))
    }
  }

  private def solveRule(
    state: State,
    env: Env,
    ruleIndex: Int,
    rule: IRulexSR,
    stepState: IStepState[IRulexSR, IRuneS, ITemplata[ITemplataType]]):
  // One might expect us to return the conclusions in this Result. Instead we take in a
  // lambda to avoid intermediate allocations, for speed.
  Result[Unit, ITypingPassSolverError] = {
    rule match {
      case KindComponentsSR(range, kindRune, mutabilityRune) => {
        val KindTemplata(kind) = vassertSome(stepState.getConclusion(kindRune.rune))
        val mutability = delegate.getMutability(state, kind)
        stepState.concludeRune[ITypingPassSolverError](mutabilityRune.rune, mutability)
        Ok(())
      }
      case CoordComponentsSR(_, resultRune, ownershipRune, kindRune) => {
        stepState.getConclusion(resultRune.rune) match {
          case None => {
            val OwnershipTemplata(ownership) = vassertSome(stepState.getConclusion(ownershipRune.rune))
            val KindTemplata(kind) = vassertSome(stepState.getConclusion(kindRune.rune))
            val newCoord = CoordT(ownership, kind)
            stepState.concludeRune[ITypingPassSolverError](resultRune.rune, CoordTemplata(newCoord))
            Ok(())
          }
          case Some(coord) => {
            val CoordTemplata(CoordT(ownership, kind)) = coord
            stepState.concludeRune[ITypingPassSolverError](ownershipRune.rune, OwnershipTemplata(ownership))
            stepState.concludeRune[ITypingPassSolverError](kindRune.rune, KindTemplata(kind))
            Ok(())
          }
        }
      }
      case PrototypeComponentsSR(_, resultRune, ownershipRune, kindRune) => {
        val PrototypeTemplata(_, prototype) = vassertSome(stepState.getConclusion(resultRune.rune))
        stepState.concludeRune[ITypingPassSolverError](ownershipRune.rune, CoordListTemplata(prototype.paramTypes))
        stepState.concludeRune[ITypingPassSolverError](kindRune.rune, CoordTemplata(prototype.returnType))
        Ok(())
      }
      case ResolveSR(range, resultRune, name, paramListRune) => {
        // If we're here, then we're resolving a prototype.
        // This happens at the call-site.
        // The function (or struct) can either supply a default resolve rule (usually
        // via the `func moo(int)void` syntax) or let the caller pass it in.

        val CoordListTemplata(paramCoords) = vassertSome(stepState.getConclusion(paramListRune.rune))
        val prototypeTemplata =
          delegate.resolveFunction(env, state, range, name, paramCoords) match {
            case Err(e) => return Err(CouldntFindFunction(range, e))
            case Ok(x) => x
          }
        stepState.concludeRune[ITypingPassSolverError](resultRune.rune, prototypeTemplata)
        Ok(())

      }
      case CallSiteFuncSR(range, prototypeRune, name, paramListRune, returnRune) => {
        // If we're here, then we're solving in the callsite, not the definition.
        // This should look up a function with that name and param list, and make sure
        // its return matches.

        vassertSome(stepState.getConclusion(prototypeRune.rune)) match {
          case PrototypeTemplata(range, prototype) => {
            stepState.concludeRune[ITypingPassSolverError](
              paramListRune.rune, CoordListTemplata(prototype.paramTypes))
            stepState.concludeRune[ITypingPassSolverError](
              returnRune.rune, CoordTemplata(prototype.returnType))
          }
          case _ => {
            return Err(CantCheckPlaceholder(range))
          }
        }

        Ok(())
      }
      case DefinitionFuncSR(range, resultRune, name, paramListRune, returnRune) => {
        // If we're here, then we're solving in the definition, not the callsite.
        // Skip checking that they match, just assume they do.

        val CoordListTemplata(paramCoords) = vassertSome(stepState.getConclusion(paramListRune.rune))
        val CoordTemplata(returnType) = vassertSome(stepState.getConclusion(returnRune.rune))

        // Now introduce a prototype that lets us call it with this new name, that we
        // can call it by.
        val newPrototype =
          delegate.assemblePrototype(env, state, range, name, paramCoords, returnType)

        stepState.concludeRune[ITypingPassSolverError](
          resultRune.rune, PrototypeTemplata(range, newPrototype))
        Ok(())
      }
      case EqualsSR(_, leftRune, rightRune) => {
        stepState.getConclusion(leftRune.rune) match {
          case None => {
            stepState.concludeRune[ITypingPassSolverError](leftRune.rune, vassertSome(stepState.getConclusion(rightRune.rune)))
            Ok(())
          }
          case Some(left) => {
            stepState.concludeRune[ITypingPassSolverError](rightRune.rune, left)
            Ok(())
          }
        }
      }
      case CoordSendSR(range, senderRune, receiverRune) => {
        // See IRFU and SRCAMP for what's going on here.
        stepState.getConclusion(receiverRune.rune) match {
          case None => {
            stepState.getConclusion(senderRune.rune) match {
              case None => vwat()
              case Some(CoordTemplata(coord)) => {
                if (delegate.isDescendant(env, state, coord.kind)) {
                  // We know that the sender can be upcast, so we can't shortcut.
                  // We need to wait for the receiver rune to know what to do.
                  stepState.addRule(CoordIsaSR(range, senderRune, receiverRune))
                  Ok(())
                } else {
                  // We're sending something that can't be upcast, so both sides are definitely the same type.
                  // We can shortcut things here, even knowing only the sender's type.
                  stepState.concludeRune[ITypingPassSolverError](receiverRune.rune, CoordTemplata(coord))
                  Ok(())
                }
              }
            }
          }
          case Some(CoordTemplata(coord)) => {
            if (delegate.isAncestor(env, state, coord.kind)) {
              // We know that the receiver is an interface, so we can't shortcut.
              // We need to wait for the sender rune to be able to confirm the sender
              // implements the receiver.
              stepState.addRule(CoordIsaSR(range, senderRune, receiverRune))
              Ok(())
            } else {
              // We're receiving a concrete type, so both sides are definitely the same type.
              // We can shortcut things here, even knowing only the receiver's type.
              stepState.concludeRune[ITypingPassSolverError](senderRune.rune, CoordTemplata(coord))
              Ok(())
            }
          }
          case other => vwat(other)
        }
      }
      case KindIsaSR(range, subRune, superRune) => {
        val sub =
          vassertSome(stepState.getConclusion(subRune.rune)) match {
            case KindTemplata(kind : CitizenRefT) => kind
            case other => vwat(other)
          }
        val suuper =
          vassertSome(stepState.getConclusion(superRune.rune)) match {
            case KindTemplata(i @ InterfaceTT(_)) => i
            case other => vwat(other)
          }
        if (delegate.getAncestors(state, sub, true).contains(suuper)) {
          Ok(())
        } else {
          Err(KindDoesntImplementInterface(sub, suuper))
        }
      }
      case CoordIsaSR(range, subRune, superRune) => {
        val CoordTemplata(subCoord) =
          vassertSome(stepState.getConclusion(subRune.rune))
        val subCitizen =
          subCoord.kind match {
            case cit : CitizenRefT => cit
            case other => return Err(SendingNonCitizen(other))
          }

        val CoordTemplata(superCoord) =
          vassertSome(stepState.getConclusion(superRune.rune))
        val superCitizen =
          superCoord.kind match {
            case cit : CitizenRefT => cit
            case other => return Err(SendingNonCitizen(other))
          }

        superCitizen match {
          case StructTT(_) => {
            if (subCoord == superCoord) {
              Ok(())
            } else {
              Err(SendingNonIdenticalKinds(subCoord, superCoord))
            }
          }
          case superInterface @ InterfaceTT(_) => {
            if (delegate.getAncestors(state, subCitizen, true).contains(superCitizen)) {
              Ok(())
            } else {
              Err(KindDoesntImplementInterface(subCitizen, superInterface))
            }
          }
        }
      }
      case rule @ OneOfSR(_, resultRune, literals) => {
        val result = vassertSome(stepState.getConclusion(resultRune.rune))
        val templatas = literals.map(literalToTemplata)
        if (templatas.contains(result)) {
          Ok(())
        } else {
          Err(OneOfFailed(rule))
        }
      }
      case rule @ IsConcreteSR(_, rune) => {
        val templata = vassertSome(stepState.getConclusion(rune.rune))
        templata match {
          case KindTemplata(kind) => {
            kind match {
              case InterfaceTT(_) => {
                Err(KindIsNotConcrete(kind))
              }
              case _ => Ok(())
            }
          }
          case _ => vwat() // Should be impossible, all template rules are type checked
        }
      }
      case rule @ IsInterfaceSR(_, rune) => {
        val templata = vassertSome(stepState.getConclusion(rune.rune))
        templata match {
          case KindTemplata(kind) => {
            kind match {
              case InterfaceTT(_) => Ok(())
              case _ => Err(KindIsNotInterface(kind))
            }
          }
          case _ => vwat() // Should be impossible, all template rules are type checked
        }
      }
      case IsStructSR(_, rune) => {
        val templata = vassertSome(stepState.getConclusion(rune.rune))
        templata match {
          case KindTemplata(kind) => {
            kind match {
              case StructTT(_) => Ok(())
              case _ => Err(KindIsNotStruct(kind))
            }
          }
          case _ => vwat() // Should be impossible, all template rules are type checked
        }
      }
      case CoerceToCoordSR(range, coordRune, kindRune) => {
        stepState.getConclusion(kindRune.rune) match {
          case None => {
            val CoordTemplata(coord) = vassertSome(stepState.getConclusion(coordRune.rune))
            stepState.concludeRune[ITypingPassSolverError](kindRune.rune, KindTemplata(coord.kind))
            Ok(())
          }
          case Some(kind) => {
            val coerced = delegate.coerce(env, state, range, CoordTemplataType(), kind)
            stepState.concludeRune[ITypingPassSolverError](coordRune.rune, coerced)
            Ok(())
          }
        }
      }
      case LiteralSR(_, rune, literal) => {
        val templata = literalToTemplata(literal)
        stepState.concludeRune[ITypingPassSolverError](rune.rune, templata)
        Ok(())
      }
      case LookupSR(range, rune, name) => {
        val result =
          delegate.lookupTemplataImprecise(env, state, range, name) match {
            case None => return Err(LookupFailed(name))
            case Some(x) => x
          }
        stepState.concludeRune[ITypingPassSolverError](rune.rune, result)
        Ok(())
      }
      case RuneParentEnvLookupSR(range, rune) => {
        // This rule does nothing. Not sure why we have it.
        //        val result = delegate.lookupTemplataImprecise(env, state, range, RuneNameS(rune.rune))
        //        stepState.concludeRune[ICompilerSolverError](rune.rune, result)
        Ok(())
      }
      case AugmentSR(_, resultRune, augmentOwnership, innerRune) => {
        stepState.getConclusion(innerRune.rune) match {
          case Some(CoordTemplata(initialCoord)) => {
            val newCoord =
              delegate.getMutability(state, initialCoord.kind) match {
                case PlaceholderTemplata(_, MutabilityTemplataType()) => {
                  if (augmentOwnership == ShareP) {
                    return Err(CantSharePlaceholder(initialCoord.kind))
                  }
                  initialCoord
                    .copy(ownership = Conversions.evaluateOwnership(augmentOwnership))
                }
                case MutabilityTemplata(MutableT) => {
                  if (augmentOwnership == ShareP) {
                    return Err(CantShareMutable(initialCoord.kind))
                  }
                  initialCoord
                    .copy(ownership = Conversions.evaluateOwnership(augmentOwnership))
                }
                case MutabilityTemplata(ImmutableT) => initialCoord
              }
            stepState.concludeRune[ITypingPassSolverError](resultRune.rune, CoordTemplata(newCoord))
            Ok(())
          }
          case None => {
            val CoordTemplata(initialCoord) = vassertSome(stepState.getConclusion(resultRune.rune))
            val newCoord =
              delegate.getMutability(state, initialCoord.kind) match {
                case PlaceholderTemplata(fullNameT, tyype) => vimpl()
                case MutabilityTemplata(MutableT) => {
                  if (augmentOwnership == ShareP) {
                    return Err(CantShareMutable(initialCoord.kind))
                  }
                  if (initialCoord.ownership != Conversions.evaluateOwnership(augmentOwnership)) {
                    return Err(OwnershipDidntMatch(initialCoord, Conversions.evaluateOwnership(augmentOwnership)))
                  }
                  initialCoord.copy(ownership = OwnT)
                }
                case MutabilityTemplata(ImmutableT) => initialCoord
              }
            stepState.concludeRune[ITypingPassSolverError](innerRune.rune, CoordTemplata(newCoord))
            Ok(())
          }
        }

      }
      case PackSR(_, resultRune, memberRunes) => {
        stepState.getConclusion(resultRune.rune) match {
          case None => {
            val members =
              memberRunes.map(memberRune => {
                val CoordTemplata(coord) = vassertSome(stepState.getConclusion(memberRune.rune))
                coord
              })
            stepState.concludeRune[ITypingPassSolverError](resultRune.rune, CoordListTemplata(members.toVector))
            Ok(())
          }
          case Some(CoordListTemplata(members)) => {
            vassert(members.size == memberRunes.size)
            memberRunes.zip(members).foreach({ case (rune, coord) =>
              stepState.concludeRune[ITypingPassSolverError](rune.rune, templata.CoordTemplata(coord))
            })
            Ok(())
          }
        }
      }
      case StaticSizedArraySR(_, resultRune, mutabilityRune, variabilityRune, sizeRune, elementRune) => {
        stepState.getConclusion(resultRune.rune) match {
          case None => {
            val mutability = ITemplata.expectMutability(vassertSome(stepState.getConclusion(mutabilityRune.rune)))
            val variability = ITemplata.expectVariability(vassertSome(stepState.getConclusion(variabilityRune.rune)))
            val size = ITemplata.expectInteger(vassertSome(stepState.getConclusion(sizeRune.rune)))
            val CoordTemplata(element) = vassertSome(stepState.getConclusion(elementRune.rune))
            val arrKind =
              delegate.getStaticSizedArrayKind(env, state, mutability, variability, size, element)
            stepState.concludeRune[ITypingPassSolverError](resultRune.rune, KindTemplata(arrKind))
            Ok(())
          }
          case Some(result) => {
            result match {
              case KindTemplata(StaticSizedArrayTT(size, mutability, variability, elementType)) => {
                stepState.concludeRune[ITypingPassSolverError](elementRune.rune, CoordTemplata(elementType))
                stepState.concludeRune[ITypingPassSolverError](sizeRune.rune, size)
                stepState.concludeRune[ITypingPassSolverError](mutabilityRune.rune, mutability)
                stepState.concludeRune[ITypingPassSolverError](variabilityRune.rune, variability)
                Ok(())
              }
              case CoordTemplata(CoordT(OwnT | ShareT, StaticSizedArrayTT(size, mutability, variability, elementType))) => {
                stepState.concludeRune[ITypingPassSolverError](elementRune.rune, CoordTemplata(elementType))
                stepState.concludeRune[ITypingPassSolverError](sizeRune.rune, size)
                stepState.concludeRune[ITypingPassSolverError](mutabilityRune.rune, mutability)
                stepState.concludeRune[ITypingPassSolverError](variabilityRune.rune, variability)
                Ok(())
              }
              case _ => return Err(CallResultWasntExpectedType(StaticSizedArrayTemplateTemplata(), result))
            }
          }
        }
      }
      case RuntimeSizedArraySR(_, resultRune, mutabilityRune, elementRune) => {
        stepState.getConclusion(resultRune.rune) match {
          case None => {
            val mutability = ITemplata.expectMutability(vassertSome(stepState.getConclusion(mutabilityRune.rune)))
            val CoordTemplata(element) = vassertSome(stepState.getConclusion(elementRune.rune))
            val arrKind =
              delegate.getRuntimeSizedArrayKind(env, state, element, mutability)
            stepState.concludeRune[ITypingPassSolverError](resultRune.rune, KindTemplata(arrKind))
            Ok(())
          }
          case Some(result) => {
            result match {
              case KindTemplata(RuntimeSizedArrayTT(mutability, elementType)) => {
                stepState.concludeRune[ITypingPassSolverError](elementRune.rune, CoordTemplata(elementType))
                stepState.concludeRune[ITypingPassSolverError](mutabilityRune.rune, mutability)
                Ok(())
              }
              case CoordTemplata(CoordT(OwnT | ShareT, RuntimeSizedArrayTT(mutability, elementType))) => {
                stepState.concludeRune[ITypingPassSolverError](elementRune.rune, CoordTemplata(elementType))
                stepState.concludeRune[ITypingPassSolverError](mutabilityRune.rune, mutability)
                Ok(())
              }
              case _ => return Err(CallResultWasntExpectedType(RuntimeSizedArrayTemplateTemplata(), result))
            }
          }
        }
      }
      case RefListCompoundMutabilitySR(range, resultRune, coordListRune) => {
        val CoordListTemplata(coords) = vassertSome(stepState.getConclusion(coordListRune.rune))
        if (coords.forall(_.ownership == ShareT)) {
          stepState.concludeRune[ITypingPassSolverError](resultRune.rune, MutabilityTemplata(ImmutableT))
        } else {
          stepState.concludeRune[ITypingPassSolverError](resultRune.rune, MutabilityTemplata(MutableT))
        }
        Ok(())
      }
      case CallSR(range, resultRune, templateRune, argRunes) => {
        val template = vassertSome(stepState.getConclusion(templateRune.rune))
        stepState.getConclusion(resultRune.rune) match {
          case Some(result) => {
            template match {
              case RuntimeSizedArrayTemplateTemplata() => {
                result match {
                  case CoordTemplata(CoordT(ShareT | OwnT, RuntimeSizedArrayTT(mutability, memberType))) => {
                    if (argRunes.size != 2) {
                      return Err(WrongNumberOfTemplateArgs(2))
                    }
                    val Array(mutabilityRune, elementRune) = argRunes
                    stepState.concludeRune[ITypingPassSolverError](mutabilityRune.rune, mutability)
                    stepState.concludeRune[ITypingPassSolverError](elementRune.rune, CoordTemplata(memberType))
                    Ok(())
                  }
                  case KindTemplata(RuntimeSizedArrayTT(mutability, memberType)) => {
                    val Array(mutabilityRune, elementRune) = argRunes
                    stepState.concludeRune[ITypingPassSolverError](mutabilityRune.rune, mutability)
                    stepState.concludeRune[ITypingPassSolverError](elementRune.rune, CoordTemplata(memberType))
                    Ok(())
                  }
                  case _ => return Err(CallResultWasntExpectedType(template, result))
                }
              }
              case it @ InterfaceTemplata(_, _) => {
                result match {
                  case KindTemplata(interface @ InterfaceTT(_)) => {
                    if (!delegate.kindIsFromTemplate(state,interface, it)) {
                      return Err(CallResultWasntExpectedType(it, result))
                    }
                    vassert(argRunes.size == interface.fullName.last.templateArgs.size)
                    argRunes.zip(interface.fullName.last.templateArgs).foreach({ case (rune, templateArg) =>
                      stepState.concludeRune[ITypingPassSolverError](rune.rune, templateArg)
                    })
                    Ok(())
                  }
                  case CoordTemplata(CoordT(OwnT | ShareT, interface @ InterfaceTT(_))) => {
                    if (!delegate.kindIsFromTemplate(state,interface, it)) {
                      return Err(CallResultWasntExpectedType(it, result))
                    }
                    vassert(argRunes.size == interface.fullName.last.templateArgs.size)
                    argRunes.zip(interface.fullName.last.templateArgs).foreach({ case (rune, templateArg) =>
                      stepState.concludeRune[ITypingPassSolverError](rune.rune, templateArg)
                    })
                    Ok(())
                  }
                  case _ => return Err(CallResultWasntExpectedType(template, result))
                }
              }
              case it @ KindTemplata(templateInterface @ InterfaceTT(_)) => {
                result match {
                  case KindTemplata(instantiationInterface @ InterfaceTT(_)) => {
                    if (templateInterface != instantiationInterface) {
                      return Err(CallResultWasntExpectedType(it, result))
                    }
                    argRunes.zip(instantiationInterface.fullName.last.templateArgs).foreach({ case (rune, templateArg) =>
                      stepState.concludeRune[ITypingPassSolverError](rune.rune, templateArg)
                    })
                    Ok(())
                  }
                  case _ => return Err(CallResultWasntExpectedType(template, result))
                }
              }
              case st @ StructTemplata(_, _) => {
                result match {
                  case KindTemplata(struct @ StructTT(_)) => {
                    if (!delegate.kindIsFromTemplate(state,struct, st)) {
                      return Err(CallResultWasntExpectedType(st, result))
                    }
                    vassert(argRunes.size == struct.fullName.last.templateArgs.size)
                    argRunes.zip(struct.fullName.last.templateArgs).foreach({ case (rune, templateArg) =>
                      stepState.concludeRune[ITypingPassSolverError](rune.rune, templateArg)
                    })
                    Ok(())
                  }
                  case CoordTemplata(CoordT(OwnT | ShareT, struct @ StructTT(_))) => {
                    if (!delegate.kindIsFromTemplate(state,struct, st)) {
                      return Err(CallResultWasntExpectedType(st, result))
                    }
                    vassert(argRunes.size == struct.fullName.last.templateArgs.size)
                    argRunes.zip(struct.fullName.last.templateArgs).foreach({ case (rune, templateArg) =>
                      stepState.concludeRune[ITypingPassSolverError](rune.rune, templateArg)
                    })
                    Ok(())
                  }
                  case _ => return Err(CallResultWasntExpectedType(template, result))
                }
              }
            }
          }
          case None => {
            template match {
              case RuntimeSizedArrayTemplateTemplata() => {
                val args = argRunes.map(argRune => vassertSome(stepState.getConclusion(argRune.rune)))
                val Array(m, CoordTemplata(coord)) = args
                val mutability = ITemplata.expectMutability(m)
                val rsaKind = delegate.getRuntimeSizedArrayKind(env, state, coord, mutability)
                stepState.concludeRune[ITypingPassSolverError](resultRune.rune, KindTemplata(rsaKind))
                Ok(())
              }
              case it @ StructTemplata(_, _) => {
                val args = argRunes.map(argRune => vassertSome(stepState.getConclusion(argRune.rune)))
                val kind = delegate.resolveStruct(env, state, range, it, args.toVector)
                stepState.concludeRune[ITypingPassSolverError](resultRune.rune, KindTemplata(kind))
                Ok(())
              }
              case it @ InterfaceTemplata(_, _) => {
                val args = argRunes.map(argRune => vassertSome(stepState.getConclusion(argRune.rune)))
                val kind = delegate.resolveInterface(env, state, range, it, args.toVector)
                stepState.concludeRune[ITypingPassSolverError](resultRune.rune, KindTemplata(kind))
                Ok(())
              }
              case kt @ KindTemplata(_) => {
                stepState.concludeRune[ITypingPassSolverError](resultRune.rune, kt)
                Ok(())
              }
              case other => vimpl(other)
            }
          }
        }
      }
    }
  }

  private def literalToTemplata(literal: ILiteralSL) = {
    literal match {
      case MutabilityLiteralSL(mutability) => MutabilityTemplata(Conversions.evaluateMutability(mutability))
      case OwnershipLiteralSL(ownership) => OwnershipTemplata(Conversions.evaluateOwnership(ownership))
      case VariabilityLiteralSL(variability) => VariabilityTemplata(Conversions.evaluateVariability(variability))
      case StringLiteralSL(string) => StringTemplata(string)
      case IntLiteralSL(num) => IntegerTemplata(num)
    }
  }
}
