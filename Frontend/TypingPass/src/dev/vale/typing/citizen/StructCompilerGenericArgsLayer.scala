package dev.vale.typing.citizen

import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.{IFunctionDeclarationNameS, ITemplataType}
import dev.vale.postparsing.rules.RuneUsage
import dev.vale.typing.env.IEnvironment
import dev.vale.typing.{CompilerOutputs, InferCompiler, InitialKnown, TypingPassOptions}
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.names.NameTranslator
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Interner, Keywords, Profiler, RangeS, vassert, vfail, vimpl, vwat}
import dev.vale.highertyping._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.StructCompilerMiddle
import dev.vale.typing.env._
import dev.vale.typing.names.AnonymousSubstructNameT

import scala.collection.immutable.List

class StructCompilerGenericArgsLayer(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    inferCompiler: InferCompiler,
    ancestorHelper: AncestorHelper,
    delegate: IStructCompilerDelegate) {
  val middle = new StructCompilerMiddle(opts, interner, keywords, nameTranslator, ancestorHelper, delegate)

  def resolveStruct(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    structTemplata: StructTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (StructTT) = {
    Profiler.frame(() => {
      val StructTemplata(declaringEnv, structA) = structTemplata
      val structTemplateName = nameTranslator.translateCitizenName(structA.name)
      val structName = structTemplateName.makeCitizenName(interner, templateArgs)
      val fullName = declaringEnv.fullName.addStep(structName)
//      val fullName = declaringEnv.fullName.addStep(structLastName)

      coutputs.structDeclared(interner.intern(StructTT(fullName))) match {
        case Some(structTT) => {
          (structTT)
        }
        case None => {
          // not sure if this is okay or not, do we allow this?
          if (templateArgs.size != structA.genericParameters.size) {
            vfail("wat?")
          }
          val temporaryStructRef = interner.intern(StructTT(fullName))
          coutputs.declareKind(temporaryStructRef)

          structA.maybePredictedMutability match {
            case None =>
            case Some(predictedMutability) => {
              coutputs.declareCitizenMutability(
                temporaryStructRef,
                MutabilityTemplata(Conversions.evaluateMutability(predictedMutability)))
            }
          }
          vassert(structA.genericParameters.size == templateArgs.size)

          val callSiteRules = structA.rules.filter(inferCompiler.includeRuleInCallSiteSolve)

          val inferences =
            inferCompiler.solveExpectComplete(
              declaringEnv,
              Some(callingEnv),
              coutputs,
              callSiteRules,
              structA.runeToType,
              callRange,
              structA.genericParameters.map(_.rune.rune).zip(templateArgs)
                .map({ case (a, b) => InitialKnown(RuneUsage(callRange, a), b) }),
              Vector())

          structA.maybePredictedMutability match {
            case None => {
              val mutability =
                ITemplata.expectMutability(inferences(structA.mutabilityRune.rune))
              coutputs.declareCitizenMutability(temporaryStructRef, mutability)
            }
            case Some(_) =>
          }

          middle.resolveStruct(declaringEnv, coutputs, callRange, structA, inferences)
        }
      }
    })
  }

  def compileStruct(
    coutputs: CompilerOutputs,
    callRange: RangeS,
    structTemplata: StructTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (StructTT) = {
    Profiler.frame(() => {
      val StructTemplata(declaringEnv, structA) = structTemplata
      val structTemplateName = nameTranslator.translateCitizenName(structA.name)
      val structName = structTemplateName.makeCitizenName(interner, templateArgs)
      val fullName = declaringEnv.fullName.addStep(structName)
      //      val fullName = env.fullName.addStep(structLastName)

      coutputs.structDeclared(interner.intern(StructTT(fullName))) match {
        case Some(structTT) => {
          (structTT)
        }
        case None => {
          // not sure if this is okay or not, do we allow this?
          if (templateArgs.size != structA.genericParameters.size) {
            vfail("wat?")
          }
          val temporaryStructRef = interner.intern(StructTT(fullName))
          coutputs.declareKind(temporaryStructRef)

          structA.maybePredictedMutability match {
            case None =>
            case Some(predictedMutability) => {
              coutputs.declareCitizenMutability(
                temporaryStructRef,
                MutabilityTemplata(Conversions.evaluateMutability(predictedMutability)))
            }
          }
          vassert(structA.genericParameters.size == templateArgs.size)

          val definitionRules = structA.rules.filter(inferCompiler.includeRuleInDefinitionSolve)

          val inferences =
            inferCompiler.solveExpectComplete(
              declaringEnv,
              None,
              coutputs,
              definitionRules,
              structA.runeToType,
              callRange,
              structA.genericParameters.map(_.rune.rune).zip(templateArgs)
                .map({ case (a, b) => InitialKnown(RuneUsage(callRange, a), b) }),
              Vector())

          structA.maybePredictedMutability match {
            case None => {
              val mutability =
                ITemplata.expectMutability(inferences(structA.mutabilityRune.rune))
              coutputs.declareCitizenMutability(temporaryStructRef, mutability)
            }
            case Some(_) =>
          }

          middle.compileStruct(declaringEnv, coutputs, callRange, structA, inferences)
        }
      }
    })
  }

  def getInterfaceRef(
    coutputs: CompilerOutputs,
    callRange: RangeS,
    interfaceTemplata: InterfaceTemplata,
    templateArgs: Vector[ITemplata[ITemplataType]]):
  (InterfaceTT) = {
    Profiler.frame(() => {
      val InterfaceTemplata(env, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateCitizenName(interfaceA.name)
      val interfaceName = interfaceTemplateName.makeCitizenName(interner, templateArgs)
      val fullName = env.fullName.addStep(interfaceName)
//      val fullName = env.fullName.addStep(interfaceLastName)

      coutputs.interfaceDeclared(interner.intern(InterfaceTT(fullName))) match {
        case Some(interfaceTT) => {
          (interfaceTT)
        }
        case None => {
          // not sure if this is okay or not, do we allow this?
          if (templateArgs.size != interfaceA.genericParameters.size) {
            vfail("wat?")
          }
          val temporaryInterfaceRef = interner.intern(InterfaceTT(fullName))
          coutputs.declareKind(temporaryInterfaceRef)


          interfaceA.maybePredictedMutability match {
            case None =>
            case Some(predictedMutability) => {
              coutputs.declareCitizenMutability(
                temporaryInterfaceRef,
                MutabilityTemplata(Conversions.evaluateMutability(predictedMutability)))
            }
          }
          vassert(interfaceA.genericParameters.size == templateArgs.size)

          val inferences =
            inferCompiler.solveExpectComplete(
              env,
              vimpl(),
              coutputs,
              interfaceA.rules,
              interfaceA.runeToType,
              callRange,
              interfaceA.genericParameters.map(_.rune.rune).zip(templateArgs)
                .map({ case (a, b) => InitialKnown(RuneUsage(callRange, a), b) }),
              Vector())

          interfaceA.maybePredictedMutability match {
            case None => {
              val mutability = ITemplata.expectMutability(inferences(interfaceA.mutabilityRune.rune))
              coutputs.declareCitizenMutability(temporaryInterfaceRef, mutability)
            }
            case Some(_) =>
          }

          middle.getInterfaceRef(env, coutputs, callRange, interfaceA, inferences)
        }
      }
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
