package dev.vale.typing

import dev.vale.highertyping.FunctionA
import dev.vale.{Err, Interner, Ok, Profiler, RangeS, Result, typing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.postparsing.rules._
import dev.vale.solver._
import dev.vale.postparsing._
import dev.vale.typing.env.{CitizenEnvironment, EnvironmentHelper, GlobalEnvironment, IEnvironment, ILookupContext, IVariableT, TemplataEnvEntry, TemplatasStore}
import dev.vale.typing.infer.{CompilerSolver, IInfererDelegate, ITypingPassSolverError}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, FullNameT, INameT, NameTranslator, ResolvingEnvNameT, RuneNameT}
import dev.vale.typing.templata.{CoordTemplata, ITemplata, InterfaceTemplata, KindTemplata, RuntimeSizedArrayTemplateTemplata, StructTemplata}

import scala.collection.immutable.Set

case class InferEnv(
  // We look in this for declared functions, see CSSNCE.
  callingEnv: Option[IEnvironment],
  // We look in this for everything else, such as type names like "int" etc.
  declaringEnv: IEnvironment,
)

case class InitialSend(
  senderRune: RuneUsage,
  receiverRune: RuneUsage,
  sendTemplata: ITemplata[ITemplataType])

case class InitialKnown(
  rune: RuneUsage,
  templata: ITemplata[ITemplataType])

class InferCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    nameTranslator: NameTranslator,
    delegate: IInfererDelegate[InferEnv, CompilerOutputs]) {
  def solveComplete(
    declaringEnv: IEnvironment,
    callingEnv: Option[IEnvironment], // See CSSNCE
    coutputs: CompilerOutputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend]):
  Result[Map[IRuneS, ITemplata[ITemplataType]], IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]] = {
    solve(declaringEnv, callingEnv, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends) match {
      case f @ FailedSolve(_, _, _) => Err(f)
      case i @ IncompleteSolve(_, _, _, _) => Err(i)
      case CompleteSolve(conclusions) => Ok(conclusions)
    }
  }

  def solveExpectComplete(
    declaringEnv: IEnvironment,
    callingEnv: Option[IEnvironment], // See CSSNCE
    coutputs: CompilerOutputs,
    rules: Vector[IRulexSR],
    runeToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend]):
  Map[IRuneS, ITemplata[ITemplataType]] = {
    solve(declaringEnv, callingEnv, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends) match {
      case f @ FailedSolve(_, _, err) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, f))
      }
      case i @ IncompleteSolve(_, _, _, _) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(invocationRange, i))
      }
      case CompleteSolve(conclusions) => conclusions
    }
  }


  def solve(
    declaringEnv: IEnvironment,
    callingEnv: Option[IEnvironment], // See CSSNCE
    state: CompilerOutputs,
    initialRules: Vector[IRulexSR],
    initialRuneToType: Map[IRuneS, ITemplataType],
    invocationRange: RangeS,
    initialKnowns: Vector[InitialKnown],
    initialSends: Vector[InitialSend]
  ): ISolverOutcome[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError] = {
    Profiler.frame(() => {
      val runeToType =
        initialRuneToType ++
        initialSends.map({ case InitialSend(senderRune, _, _) =>
          senderRune.rune -> CoordTemplataType()
        })
      val rules =
        initialRules ++
        initialSends.map({ case InitialSend(senderRune, receiverRune, _) =>
          CoordSendSR(receiverRune.range, senderRune, receiverRune)
        })
      val alreadyKnown =
        initialKnowns.map({ case InitialKnown(rune, templata) => rune.rune -> templata }).toMap ++
        initialSends.map({ case InitialSend(senderRune, _, senderTemplata) =>
          (senderRune.rune -> senderTemplata)
        })

      val outcome =
        new CompilerSolver[InferEnv, CompilerOutputs](opts.globalOptions, interner, delegate)
          .solve(
            invocationRange,
            InferEnv(callingEnv, declaringEnv),
            state,
            rules,
            runeToType,
            alreadyKnown)
      outcome match {
        case CompleteSolve(conclusions) => {
          checkTemplateInstantiations(declaringEnv, callingEnv, state, rules.toArray, conclusions)
        }
        case IncompleteSolve(_, _, _, incompleteConclusions) => {
          checkTemplateInstantiations(declaringEnv, callingEnv, state, rules.toArray, incompleteConclusions)
        }
        case FailedSolve(_, _, _) =>
      }
      outcome
    })
  }


  case class ResolvingEnvironment(
    globalEnv: GlobalEnvironment,
    parentEnv: IEnvironment,
    fullName: FullNameT[ResolvingEnvNameT],
    templatas: TemplatasStore
  ) extends IEnvironment {
    override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

    override def lookupWithNameInner(
      name: INameT,
      lookupFilter: Set[ILookupContext],
      getOnlyNearest: Boolean):
    Iterable[ITemplata[ITemplataType]] = {
      EnvironmentHelper.lookupWithNameInner(
        this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
    }

    override def lookupWithImpreciseNameInner(
      name: IImpreciseNameS,
      lookupFilter: Set[ILookupContext],
      getOnlyNearest: Boolean):
    Iterable[ITemplata[ITemplataType]] = {
      EnvironmentHelper.lookupWithImpreciseNameInner(
        this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
    }
  }

  def checkTemplateInstantiations(
    declaringEnv: IEnvironment,
    maybeCallingEnv: Option[IEnvironment], // See CSSNCE
    state: CompilerOutputs,
    rules: Array[IRulexSR],
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Unit = {
    val name = declaringEnv.fullName.addStep(ResolvingEnvNameT())
    val templatasStore =
      TemplatasStore(name, Map(), Map())
        .addEntries(
          interner,
          conclusions.map({case (nameS, templata) =>
            interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
          }).toVector)
    val temporaryEnv =
      ResolvingEnvironment(
        declaringEnv.globalEnv, declaringEnv, name, templatasStore)

    rules.foreach({
      case r @ CallSR(_, _, _, _) => {
        checkCall(InferEnv(maybeCallingEnv, temporaryEnv), state, r, conclusions)
      }
      case _ =>
    })
  }

  def checkCall(env: InferEnv, state: CompilerOutputs, c: CallSR, conclusions: Map[IRuneS, ITemplata[ITemplataType]]): Unit = {
    val CallSR(range, resultRune, templateRune, argRunes) = c

    val template =
      conclusions.get(templateRune.rune) match {
        case Some(t) => t
        case None => return
      }
    val args =
      argRunes.map(argRune => {
        conclusions.get(argRune.rune) match {
          case Some(t) => t
          case None => return
        }
      })

    template match {
      case RuntimeSizedArrayTemplateTemplata() => {
        val Array(m, CoordTemplata(coord)) = args
        val mutability = ITemplata.expectMutability(m)
        delegate.getRuntimeSizedArrayKind(env, state, coord, mutability)
      }
      case it @ StructTemplata(_, _) => {
        delegate.resolveStruct(env, state, range, it, args.toVector)
      }
      case it @ InterfaceTemplata(_, _) => {
        delegate.resolveInterface(env, state, range, it, args.toVector)
      }
      case kt @ KindTemplata(_) => {
        Ok(kt)
      }
      case other => vimpl(other)
    }
  }
}

object InferCompiler {
  // Some rules should be excluded from the call site, see SROACSD.
  def includeRuleInCallSiteSolve(rule: IRulexSR): Boolean = {
    rule match {
      case DefinitionFuncSR(_, _, _, _, _) => false
      case _ => true
    }
  }

  // Some rules should be excluded from the call site, see SROACSD.
  def includeRuleInDefinitionSolve(rule: IRulexSR): Boolean = {
    rule match {
      case CallSiteFuncSR(_, _, _, _, _) => false
      case ResolveSR(_, _, _, _) => false
      case _ => true
    }
  }
}