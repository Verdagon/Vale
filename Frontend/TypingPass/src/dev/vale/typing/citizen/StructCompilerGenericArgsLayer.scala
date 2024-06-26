package dev.vale.typing.citizen

import dev.vale.highertyping.FunctionA
import dev.vale.postparsing._
import dev.vale.postparsing.rules.{IRulexSR, RuneUsage}
import dev.vale.typing.env.IInDenizenEnvironmentT
import dev.vale.typing.{CompilerOutputs, InferCompiler, InitialKnown, TypingPassOptions}
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Accumulator, Err, Interner, Keywords, Ok, Profiler, RangeS, typing, vassert, vassertSome, vcurious, vfail, vimpl, vregionmut, vwat}
import dev.vale.highertyping._
import dev.vale.solver.{CompleteSolve, FailedSolve, IncompleteSolve}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.env._

import scala.collection.immutable.{List, Set}

class StructCompilerGenericArgsLayer(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler,
    delegate: IStructCompilerDelegate) {
  val core = new StructCompilerCore(opts, interner, keywords, nameTranslator, delegate)

  def resolveStruct(
    coutputs: CompilerOutputs,
    originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    structTemplata: StructDefinitionTemplataT,
    templateArgs: Vector[ITemplataT[ITemplataType]]):
  IResolveOutcome[StructTT] = {
    Profiler.frame(() => {
      val StructDefinitionTemplataT(declaringEnv, structA) = structTemplata
      val structTemplateName = nameTranslator.translateStructName(structA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        structA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(callRange.head, genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
          structA.headerRules.toVector, structA.genericParameters, templateArgs.size)

      val contextRegion = RegionT()

      // Check if its a valid use of this template
      val envs = InferEnv(originalCallingEnv, callRange, callLocation, declaringEnv, contextRegion)
      val solver =
        inferCompiler.makeSolver(
          envs,
          coutputs,
          callSiteRules,
          structA.headerRuneToType,
          callRange,
          initialKnowns,
          Vector())
      inferCompiler.continue(envs, coutputs, solver) match {
        case Ok(()) =>
        case Err(x) => return ResolveFailure(callRange, ResolvingSolveFailedOrIncomplete(x))
      }
      val CompleteResolveSolve(inferences, runeToFunctionBound) =
        inferCompiler.checkResolvingConclusionsAndResolve(
          envs,
          coutputs,
          callRange,
          callLocation,
          structA.headerRuneToType,
          callSiteRules,
          Vector(),
          solver) match {
          case Ok(ccs) => ccs
          case Err(x) => return ResolveFailure(callRange, x)
        }

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = structA.genericParameters.map(_.rune.rune).map(inferences)
      val structName = structTemplateName.makeStructName(interner, finalGenericArgs)
      val id = declaringEnv.id.addStep(structName)

      coutputs.addInstantiationBounds(
        opts.globalOptions.sanityCheck,
        interner, originalCallingEnv.denizenTemplateId, id, runeToFunctionBound)
      val structTT = interner.intern(StructTT(id))

      ResolveSuccess(structTT)
    })
  }

  // See SFWPRL for how this is different from resolveInterface.
  def predictInterface(
    coutputs: CompilerOutputs,
    originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    interfaceTemplata: InterfaceDefinitionTemplataT,
    templateArgs: Vector[ITemplataT[ITemplataType]]):
  (InterfaceTT) = {
    Profiler.frame(() => {
      val InterfaceDefinitionTemplataT(declaringEnv, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateInterfaceName(interfaceA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        interfaceA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(callRange.head, genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assemblePredictRules(
          interfaceA.genericParameters, templateArgs.size)
      val runesForPrediction =
        (interfaceA.genericParameters.map(_.rune.rune) ++
          callSiteRules.flatMap(_.runeUsages.map(_.rune))).toSet
      val runeToTypeForPrediction =
        runesForPrediction.toVector.map(r => r -> interfaceA.runeToType(r)).toMap

      val contextRegion = RegionT()

      // This *doesnt* check to make sure it's a valid use of the template. Its purpose is really
      // just to populate any generic parameter default values.

      // We're just predicting, see STCMBDP.
      val inferences =
        inferCompiler.partialSolve(
          InferEnv(originalCallingEnv, callRange, callLocation, declaringEnv, contextRegion),
          coutputs,
          callSiteRules,
          runeToTypeForPrediction,
          callRange,
          initialKnowns,
          Vector()) match {
          case Ok(i) => i
          case Err(e) => throw CompileErrorExceptionT(typing.TypingPassSolverError(callRange, e))
        }

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = interfaceA.genericParameters.map(_.rune.rune).map(inferences)
      val interfaceName = interfaceTemplateName.makeInterfaceName(interner, finalGenericArgs)
      val id = declaringEnv.id.addStep(interfaceName)

      // Usually when we make an InterfaceTT we put the instantiation bounds into the coutputs,
      // but we unfortunately can't here because we're just predicting an interface; we'll
      // try to resolve it later and then put the bounds in. Hopefully this InterfaceTT doesn't
      // escape into the wild.
      val interfaceTT = interner.intern(InterfaceTT(id))
      interfaceTT
    })
  }

  // See SFWPRL for how this is different from resolveStruct.
  def predictStruct(
    coutputs: CompilerOutputs,
    originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    structTemplata: StructDefinitionTemplataT,
    templateArgs: Vector[ITemplataT[ITemplataType]]):
  (StructTT) = {
    Profiler.frame(() => {
      val StructDefinitionTemplataT(declaringEnv, structA) = structTemplata
      val structTemplateName = nameTranslator.translateStructName(structA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        structA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(vassertSome(callRange.headOption), genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assemblePredictRules(
          structA.genericParameters, templateArgs.size)
      val runesForPrediction =
        (structA.genericParameters.map(_.rune.rune) ++
          callSiteRules.flatMap(_.runeUsages.map(_.rune))).toSet
      val runeToTypeForPrediction =
        runesForPrediction.toVector.map(r => r -> structA.headerRuneToType(r)).toMap

      // This *doesnt* check to make sure it's a valid use of the template. Its purpose is really
      // just to populate any generic parameter default values.

      // Maybe we should make this incremental too, like when solving definitions?

      val contextRegion = RegionT()

      val inferences =
      // We're just predicting, see STCMBDP.
        inferCompiler.partialSolve(
          InferEnv(originalCallingEnv, callRange, callLocation, declaringEnv, contextRegion),
          coutputs,
          callSiteRules,
          runeToTypeForPrediction,
          callRange,
          initialKnowns,
          Vector()) match {
          case Ok(i) => i
          case Err(e) => throw CompileErrorExceptionT(typing.TypingPassSolverError(callRange, e))
        }

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = structA.genericParameters.map(_.rune.rune).map(inferences)
      val structName = structTemplateName.makeStructName(interner, finalGenericArgs)
      val id = declaringEnv.id.addStep(structName)

      // Usually when we make an InterfaceTT we put the instantiation bounds into the coutputs,
      // but we unfortunately can't here because we're just predicting an interface; we'll
      // try to resolve it later and then put the bounds in. Hopefully this InterfaceTT doesn't
      // escape into the wild.
      val structTT = interner.intern(StructTT(id))
      structTT
    })
  }

  def resolveInterface(
    coutputs: CompilerOutputs,
    originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    interfaceTemplata: InterfaceDefinitionTemplataT,
    templateArgs: Vector[ITemplataT[ITemplataType]]):
  IResolveOutcome[InterfaceTT] = {
    Profiler.frame(() => {
      val InterfaceDefinitionTemplataT(declaringEnv, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateInterfaceName(interfaceA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        interfaceA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(callRange.head, genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
          interfaceA.rules.toVector, interfaceA.genericParameters, templateArgs.size)

      val contextRegion = RegionT()

      // This checks to make sure it's a valid use of this template.
      val CompleteResolveSolve(inferences, runeToFunctionBound) =
        inferCompiler.solveForResolving(
        InferEnv(originalCallingEnv, callRange, callLocation, declaringEnv, contextRegion),
        coutputs,
        callSiteRules,
          interfaceA.runeToType,
          callRange,
        callLocation,
          initialKnowns,
          Vector()) match {
          case Ok(ccs) => ccs
          case Err(x) => return ResolveFailure(callRange, x)
        }

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = interfaceA.genericParameters.map(_.rune.rune).map(inferences)
      val interfaceName = interfaceTemplateName.makeInterfaceName(interner, finalGenericArgs)
      val id = declaringEnv.id.addStep(interfaceName)

      coutputs.addInstantiationBounds(
        opts.globalOptions.sanityCheck,
        interner, originalCallingEnv.denizenTemplateId, id, runeToFunctionBound)
      val interfaceTT = interner.intern(InterfaceTT(id))

      ResolveSuccess(interfaceTT)
    })
  }

  def compileStruct(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    structTemplata: StructDefinitionTemplataT):
  UncheckedDefiningConclusions = {
    Profiler.frame(() => {
      val StructDefinitionTemplataT(declaringEnv, structA) = structTemplata
      val structTemplateName = nameTranslator.translateStructName(structA.name)
      val structTemplateId = declaringEnv.id.addStep(structTemplateName)

      // We declare the struct's outer environment in the precompile stage instead of here because
      // of MDATOEF.
      val outerEnv = coutputs.getOuterEnvForType(parentRanges, structTemplateId)

      val allRulesS = structA.headerRules ++ structA.memberRules
      val allRuneToType = structA.headerRuneToType ++ structA.membersRuneToType
      val definitionRules = allRulesS.filter(InferCompiler.includeRuleInDefinitionSolve)

      val envs = InferEnv(outerEnv, List(structA.range), callLocation, outerEnv, RegionT())
      val solver =
        inferCompiler.makeSolver(
          envs, coutputs, definitionRules, allRuneToType, structA.range :: parentRanges, Vector(), Vector())
      // Incrementally solve and add placeholders, see IRAGP.
      inferCompiler.incrementallySolve(
        envs, coutputs, solver,
        // Each step happens after the solver has done all it possibly can. Sometimes this can lead
        // to races, see RRBFS.
        (solver) => {
          TemplataCompiler.getFirstUnsolvedIdentifyingRune(structA.genericParameters, (rune) => solver.getConclusion(rune).nonEmpty) match {
            case None => false
            case Some((genericParam, index)) => {
              val placeholderPureHeight = vregionmut(None)
              // Make a placeholder for every argument even if it has a default, see DUDEWCD.
              val templata =
                templataCompiler.createPlaceholder(
                  coutputs, outerEnv, structTemplateId, genericParam, index, allRuneToType, placeholderPureHeight, true)
              solver.manualStep(Map(genericParam.rune.rune -> templata))
              true
            }
          }
        }) match {
        case Err(f @ FailedCompilerSolve(_, _, err)) => {
          throw CompileErrorExceptionT(typing.TypingPassSolverError(structA.range :: parentRanges, f))
        }
        case Ok(true) =>
        case Ok(false) => // Incomplete, will be detected in the below expectCompleteSolve
      }
      val inferences =
        inferCompiler.interpretResults(allRuneToType, solver) match {
          case Err(e) => throw CompileErrorExceptionT(typing.TypingPassSolverError(structA.range :: parentRanges, e))
          case Ok(conclusions) => conclusions
        }
      // Be sure to eventually check these conclusions
      val uncheckedDefiningConclusions =
        UncheckedDefiningConclusions(
          envs,
          structA.range :: parentRanges,
          callLocation,
          definitionRules,
          inferences)

      structA.maybePredictedMutability match {
        case None => {
          val mutability =
            ITemplataT.expectMutability(inferences(structA.mutabilityRune.rune))
          coutputs.declareTypeMutability(structTemplateId, mutability)
        }
        case Some(_) =>
      }

      val templateArgs = structA.genericParameters.map(_.rune.rune).map(inferences)

      val id = assembleStructName(structTemplateId, templateArgs)

      val innerEnv =
        CitizenEnvironmentT(
          outerEnv.globalEnv,
          outerEnv,
          structTemplateId,
          id,
          TemplatasStore(id, Map(), Map())
            .addEntries(
              interner,
              inferences.toVector
                .map({ case (rune, templata) => (interner.intern(RuneNameT(rune)), TemplataEnvEntry(templata)) })))

      coutputs.declareTypeInnerEnv(structTemplateId, innerEnv)

      core.compileStruct(outerEnv, innerEnv, coutputs, parentRanges, callLocation, structA)

      uncheckedDefiningConclusions
    })
  }

  def compileInterface(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    interfaceTemplata: InterfaceDefinitionTemplataT):
  UncheckedDefiningConclusions = {
    Profiler.frame(() => {
      val InterfaceDefinitionTemplataT(declaringEnv, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateInterfaceName(interfaceA.name)
      val interfaceTemplateId = declaringEnv.id.addStep(interfaceTemplateName)

      // We declare the interface's outer environment in the precompile stage instead of here because
      // of MDATOEF.
      val outerEnv = coutputs.getOuterEnvForType(parentRanges, interfaceTemplateId)

      //      val fullName = env.fullName.addStep(interfaceLastName)

      val definitionRules = interfaceA.rules.filter(InferCompiler.includeRuleInDefinitionSolve)

      val envs = InferEnv(outerEnv, List(interfaceA.range), callLocation, outerEnv, RegionT())
      val solver =
        inferCompiler.makeSolver(
          envs, coutputs, definitionRules, interfaceA.runeToType, interfaceA.range :: parentRanges, Vector(), Vector())
      // Incrementally solve and add placeholders, see IRAGP.
      inferCompiler.incrementallySolve(
        envs, coutputs, solver,
        // Each step happens after the solver has done all it possibly can. Sometimes this can lead
        // to races, see RRBFS.
        (solver) => {
          TemplataCompiler.getFirstUnsolvedIdentifyingRune(interfaceA.genericParameters, (rune) => solver.getConclusion(rune).nonEmpty) match {
            case None => false
            case Some((genericParam, index)) => {
              // Make a placeholder for every argument even if it has a default, see DUDEWCD.
              val placeholderPureHeight = vregionmut(None)
              val templata =
                templataCompiler.createPlaceholder(
                  coutputs, outerEnv, interfaceTemplateId, genericParam, index, interfaceA.runeToType, placeholderPureHeight, true)
              solver.manualStep(Map(genericParam.rune.rune -> templata))
              true
            }
          }
        }) match {
        case Err(f @ FailedCompilerSolve(_, _, err)) => {
          throw CompileErrorExceptionT(typing.TypingPassSolverError(interfaceA.range :: parentRanges, f))
        }
        case Ok(true) =>
        case Ok(false) => // Incomplete, will be detected in the below expectCompleteSolve
      }
      val inferences =
        inferCompiler.interpretResults(interfaceA.runeToType, solver) match {
          case Err(e) => throw CompileErrorExceptionT(typing.TypingPassSolverError(interfaceA.range :: parentRanges, e))
          case Ok(conclusions) => conclusions
        }
      val uncheckedConclusions =
        UncheckedDefiningConclusions(
          envs, interfaceA.range :: parentRanges, callLocation, definitionRules, inferences)

      interfaceA.maybePredictedMutability match {
        case None => {
          val mutability = ITemplataT.expectMutability(inferences(interfaceA.mutabilityRune.rune))
          coutputs.declareTypeMutability(interfaceTemplateId, mutability)
        }
        case Some(_) =>
      }

      val templateArgs = interfaceA.genericParameters.map(_.rune.rune).map(inferences)

      val id = assembleInterfaceName(interfaceTemplateId, templateArgs)

      val innerEnv =
        CitizenEnvironmentT(
          outerEnv.globalEnv,
          outerEnv,
          interfaceTemplateId,
          id,
          TemplatasStore(id, Map(), Map())
            .addEntries(
              interner,
              inferences.toVector
                .map({ case (rune, templata) => (interner.intern(RuneNameT(rune)), TemplataEnvEntry(templata)) })))

      coutputs.declareTypeInnerEnv(interfaceTemplateId, innerEnv)

      core.compileInterface(outerEnv, innerEnv, coutputs, parentRanges, callLocation, interfaceA)

      uncheckedConclusions
    })
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: NodeEnvironmentT,
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    name: IFunctionDeclarationNameS,
    functionS: FunctionA,
    members: Vector[NormalStructMemberT]):
  (StructTT, MutabilityT, FunctionTemplataT) = {
    core.makeClosureUnderstruct(
      containingFunctionEnv, coutputs, parentRanges, callLocation, name, functionS, members)
  }

  def assembleStructName(
    templateName: IdT[IStructTemplateNameT],
    templateArgs: Vector[ITemplataT[ITemplataType]]):
  IdT[IStructNameT] = {
    templateName.copy(
      localName = templateName.localName.makeStructName(interner, templateArgs))
  }

  def assembleInterfaceName(
    templateName: IdT[IInterfaceTemplateNameT],
    templateArgs: Vector[ITemplataT[ITemplataType]]):
  IdT[IInterfaceNameT] = {
    templateName.copy(
      localName = templateName.localName.makeInterfaceName(interner, templateArgs))
  }
}
