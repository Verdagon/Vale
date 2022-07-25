package dev.vale.typing.citizen

import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.{GenericParameterS, IFunctionDeclarationNameS, ITemplataType}
import dev.vale.postparsing.rules.{IRulexSR, RuneUsage}
import dev.vale.typing.env.IEnvironment
import dev.vale.typing.{CompilerOutputs, InferCompiler, InitialKnown, TypingPassOptions}
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.names.{AnonymousSubstructNameT, FullNameT, IInterfaceTemplateNameT, IStructTemplateNameT, NameTranslator, StructTemplateNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Accumulator, Interner, Keywords, Profiler, RangeS, typing, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.highertyping._
import dev.vale.solver.{CompleteSolve, FailedSolve, IncompleteSolve}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.StructCompilerMiddle
import dev.vale.typing.env._

import scala.collection.immutable.List

class StructCompilerGenericArgsLayer(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler,
    delegate: IStructCompilerDelegate) {
  val middle = new StructCompilerMiddle(opts, interner, keywords, nameTranslator, delegate)

  def resolveStruct(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    structTemplata: StructTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (StructTT) = {
    Profiler.frame(() => {
      val StructTemplata(declaringEnv, structA) = structTemplata
      val structTemplateName = nameTranslator.translateStructName(structA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        structA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(callRange, genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
          structA.headerRules.toVector, structA.genericParameters, templateArgs.size)

      // Check if its a valid use of this template
      val inferences =
        inferCompiler.solveExpectComplete(
          declaringEnv,
          Some(callingEnv),
          coutputs,
          callSiteRules,
          structA.headerRuneToType,
          callRange,
          initialKnowns,
          Vector())

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = structA.genericParameters.map(_.rune.rune).map(inferences)
      val structName = structTemplateName.makeStructName(interner, finalGenericArgs)
      val fullName = declaringEnv.fullName.addStep(structName)
      val structTT = interner.intern(StructTT(fullName))
      structTT
    })
  }

  // See SFWPRL for how this is different from resolveInterface.
  def predictInterface(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    interfaceTemplata: InterfaceTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (InterfaceTT) = {
    Profiler.frame(() => {
      val InterfaceTemplata(env, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateInterfaceName(interfaceA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        interfaceA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(callRange, genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assemblePredictRules(
          interfaceA.genericParameters, templateArgs.size)
      val runesForPrediction =
        (interfaceA.genericParameters.map(_.rune.rune) ++
          callSiteRules.flatMap(_.runeUsages.map(_.rune))).toSet
      val runeToTypeForPrediction =
        runesForPrediction.toArray.map(r => r -> interfaceA.runeToType(r)).toMap

      // This *doesnt* check to make sure it's a valid use of the template. Its purpose is really
      // just to populate any generic parameter default values.
      val inferences =
        inferCompiler.solveExpectComplete(
          env,
          Some(callingEnv),
          coutputs,
          callSiteRules,
          runeToTypeForPrediction,
          callRange,
          initialKnowns,
          Vector())

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = interfaceA.genericParameters.map(_.rune.rune).map(inferences)
      val interfaceName = interfaceTemplateName.makeInterfaceName(interner, finalGenericArgs)
      val fullName = env.fullName.addStep(interfaceName)
      val interfaceTT = interner.intern(InterfaceTT(fullName))
      interfaceTT
    })
  }

  // See SFWPRL for how this is different from resolveStruct.
  def predictStruct(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    structTemplata: StructTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (StructTT) = {
    Profiler.frame(() => {
      val StructTemplata(env, structA) = structTemplata
      val structTemplateName = nameTranslator.translateStructName(structA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        structA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(callRange, genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assemblePredictRules(
          structA.genericParameters, templateArgs.size)
      val runesForPrediction =
        (structA.genericParameters.map(_.rune.rune) ++
          callSiteRules.flatMap(_.runeUsages.map(_.rune))).toSet
      val runeToTypeForPrediction =
        runesForPrediction.toArray.map(r => r -> structA.headerRuneToType(r)).toMap

      // This *doesnt* check to make sure it's a valid use of the template. Its purpose is really
      // just to populate any generic parameter default values.
      val inferences =
      inferCompiler.solveExpectComplete(
        env,
        Some(callingEnv),
        coutputs,
        callSiteRules,
        runeToTypeForPrediction,
        callRange,
        initialKnowns,
        Vector())

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = structA.genericParameters.map(_.rune.rune).map(inferences)
      val structName = structTemplateName.makeStructName(interner, finalGenericArgs)
      val fullName = env.fullName.addStep(structName)
      val structTT = interner.intern(StructTT(fullName))
      structTT
    })
  }

  def resolveInterface(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    interfaceTemplata: InterfaceTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (InterfaceTT) = {
    Profiler.frame(() => {
      val InterfaceTemplata(env, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateInterfaceName(interfaceA.name)

      // We no longer assume this:
      //   vassert(templateArgs.size == structA.genericParameters.size)
      // because we have default generic arguments now.

      val initialKnowns =
        interfaceA.genericParameters.zip(templateArgs).map({ case (genericParam, templateArg) =>
          InitialKnown(RuneUsage(callRange, genericParam.rune.rune), templateArg)
        })

      val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
          interfaceA.rules.toVector, interfaceA.genericParameters, templateArgs.size)

      // This checks to make sure it's a valid use of this template.
      val inferences =
        inferCompiler.solveExpectComplete(
          env,
          Some(callingEnv),
          coutputs,
          callSiteRules,
          interfaceA.runeToType,
          callRange,
          initialKnowns,
          Vector())

      // We can't just make a StructTT with the args they gave us, because they may have been
      // missing some, in which case we had to run some default rules.
      // Let's use the inferences to make one.

      val finalGenericArgs = interfaceA.genericParameters.map(_.rune.rune).map(inferences)
      val interfaceName = interfaceTemplateName.makeInterfaceName(interner, finalGenericArgs)
      val fullName = env.fullName.addStep(interfaceName)
      val interfaceTT = interner.intern(InterfaceTT(fullName))
      interfaceTT
    })
  }

  def compileStruct(
    coutputs: CompilerOutputs,
    structTemplata: StructTemplata):
  Unit = {
    Profiler.frame(() => {
      val StructTemplata(declaringEnv, structA) = structTemplata
      val structTemplateName = nameTranslator.translateStructName(structA.name)
      val structTemplateFullName = declaringEnv.fullName.addStep(structTemplateName)
      coutputs.declareEnvForTemplate(structTemplateFullName, declaringEnv)

      val allRulesS = structA.headerRules ++ structA.memberRules
      val allRuneToType = structA.headerRuneToType ++ structA.membersRuneToType
      val definitionRules = allRulesS.filter(InferCompiler.includeRuleInDefinitionSolve)

      // This is temporary, to support specialization like:
      //   extern("vale_runtime_sized_array_mut_new")
      //   func Array<M, E>(size int) []<M>E
      //   where M Mutability = mut, E Ref;
      // In the future we might need to outlaw specialization, unsure.
      val preliminaryInferences =
        inferCompiler.solve(
          declaringEnv, None, coutputs, definitionRules.toVector, allRuneToType, structA.range, Vector(), Vector()) match {
          case f @ FailedSolve(_, _, err) => {
            throw CompileErrorExceptionT(typing.TypingPassSolverError(structA.range, f))
          }
          case IncompleteSolve(_, _, _, incompleteConclusions) => incompleteConclusions
          case CompleteSolve(conclusions) => conclusions
        }
      // Now we can use preliminaryInferences to know whether or not we need a placeholder for an identifying rune.

      val initialKnowns =
        structA.genericParameters.zipWithIndex.flatMap({ case (genericParam, index) =>
          preliminaryInferences.get(genericParam.rune.rune) match {
            case Some(x) => Some(InitialKnown(genericParam.rune, x))
            case None => {
              // Make a placeholder for every argument even if it has a default, see DUDEWCD.
              val runeType = vassertSome(allRuneToType.get(genericParam.rune.rune))
              val templata = templataCompiler.createPlaceholder(coutputs, declaringEnv, structTemplateFullName, index, runeType)
              Some(InitialKnown(genericParam.rune, templata))
            }
          }
        })


      val inferences =
        inferCompiler.solveExpectComplete(
          declaringEnv,
          None,
          coutputs,
          definitionRules.toVector,
          allRuneToType,
          structA.range,
          initialKnowns,
//            structA.genericParameters.zip(templateArgs).map({ case (GenericParameterS(rune, default), genericArg) =>
//              InitialKnown(RuneUsage(rune.range, rune.rune), genericArg)
//            }),
          Vector())

      structA.maybePredictedMutability match {
        case None => {
          val mutability =
            ITemplata.expectMutability(inferences(structA.mutabilityRune.rune))
          coutputs.declareTemplateMutability(structTemplateFullName, mutability)
        }
        case Some(_) =>
      }

      middle.compileStruct(declaringEnv, coutputs, structA, inferences)
    })
  }

  def compileInterface(
    coutputs: CompilerOutputs,
    interfaceTemplata: InterfaceTemplata):
  Unit = {
    Profiler.frame(() => {
      val InterfaceTemplata(declaringEnv, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateInterfaceName(interfaceA.name)
      val interfaceTemplateFullName = declaringEnv.fullName.addStep(interfaceTemplateName)
      coutputs.declareEnvForTemplate(interfaceTemplateFullName, declaringEnv)

      //      val fullName = env.fullName.addStep(interfaceLastName)

      val definitionRules = interfaceA.rules.filter(InferCompiler.includeRuleInDefinitionSolve)

      // This is temporary, to support specialization like:
      //   extern("vale_runtime_sized_array_mut_new")
      //   func Array<M, E>(size int) []<M>E
      //   where M Mutability = mut, E Ref;
      // In the future we might need to outlaw specialization, unsure.
      val preliminaryInferences =
        inferCompiler.solve(
          declaringEnv, None, coutputs, definitionRules, interfaceA.runeToType, interfaceA.range, Vector(), Vector()) match {
          case f @ FailedSolve(_, _, err) => {
            throw CompileErrorExceptionT(typing.TypingPassSolverError(interfaceA.range, f))
          }
          case IncompleteSolve(_, _, _, incompleteConclusions) => incompleteConclusions
          case CompleteSolve(conclusions) => conclusions
        }
      // Now we can use preliminaryInferences to know whether or not we need a placeholder for an identifying rune.

      val initialKnowns =
        interfaceA.genericParameters.zipWithIndex.flatMap({ case (genericParam, index) =>
          preliminaryInferences.get(genericParam.rune.rune) match {
            case Some(x) => Some(InitialKnown(genericParam.rune, x))
            case None => {
              genericParam.default match {
                case Some(defaultGenericParam) => {
                  // Don't populate a placeholder for this, see DAPGPD.
                  None
                }
                case None => {
                  val runeType = vassertSome(interfaceA.runeToType.get(genericParam.rune.rune))
                  val templata = templataCompiler.createPlaceholder(coutputs, declaringEnv, interfaceTemplateFullName, index, runeType)
                  Some(InitialKnown(genericParam.rune, templata))
                }
              }
            }
          }
        })

      val inferences =
        inferCompiler.solveExpectComplete(
          declaringEnv,
          None,
          coutputs,
          definitionRules,
          interfaceA.runeToType,
          interfaceA.range,
          initialKnowns,
          Vector())

      interfaceA.maybePredictedMutability match {
        case None => {
          val mutability = ITemplata.expectMutability(inferences(interfaceA.mutabilityRune.rune))
          coutputs.declareTemplateMutability(interfaceTemplateFullName, mutability)
        }
        case Some(_) =>
      }

      middle.compileInterface(declaringEnv, coutputs, interfaceA, inferences)
    })
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    coutputs: CompilerOutputs,
    name: IFunctionDeclarationNameS,
    functionS: FunctionA,
    members: Vector[StructMemberT]):
  (StructTT, MutabilityT, FunctionTemplata) = {
    middle.makeClosureUnderstruct(containingFunctionEnv, coutputs, name, functionS, members)
  }
}
