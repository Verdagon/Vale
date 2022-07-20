package dev.vale.typing.citizen

import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.{GenericParameterS, IFunctionDeclarationNameS, ITemplataType}
import dev.vale.postparsing.rules.RuneUsage
import dev.vale.typing.env.IEnvironment
import dev.vale.typing.{CompilerOutputs, InferCompiler, InitialKnown, TypingPassOptions}
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.names.NameTranslator
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Interner, Keywords, Profiler, RangeS, vassert, vcurious, vfail, vimpl, vwat}
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
      val structTemplateName = nameTranslator.translateStructName(structA.name)
      val structName = structTemplateName.makeStructName(interner, templateArgs)
      val fullName = declaringEnv.fullName.addStep(structName)
      val structTT = interner.intern(StructTT(fullName))

      // not sure if this is okay or not, do we allow this?
      if (templateArgs.size != structA.genericParameters.size) {
        vfail("wat?")
      }

      val callSiteRules = structA.rules.filter(inferCompiler.includeRuleInCallSiteSolve)

      // Check if its a valid use of this template
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

      structTT
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

      if (coutputs.structDeclared(structTemplateFullName)) {
        vcurious()
        return
      }

      coutputs.declareTemplate(structTemplateFullName)

      structA.maybePredictedMutability match {
        case None =>
        case Some(predictedMutability) => {
          coutputs.declareTemplateMutability(
            structTemplateFullName,
            MutabilityTemplata(Conversions.evaluateMutability(predictedMutability)))
        }
      }

      val definitionRules = structA.rules.filter(inferCompiler.includeRuleInDefinitionSolve)

      val placeholders = vimpl()

      val inferences =
        inferCompiler.solveExpectComplete(
          declaringEnv,
          None,
          coutputs,
          definitionRules,
          structA.runeToType,
          structA.range,
          placeholders,
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
      val InterfaceTemplata(env, interfaceA) = interfaceTemplata
      val interfaceTemplateName = nameTranslator.translateInterfaceName(interfaceA.name)
      val interfaceTemplateFullName = env.fullName.addStep(interfaceTemplateName)
//      val fullName = env.fullName.addStep(interfaceLastName)

      if (coutputs.interfaceDeclared(interfaceTemplateFullName)) {
        vcurious()
        return
      }

      coutputs.declareTemplate(interfaceTemplateFullName)

      interfaceA.maybePredictedMutability match {
        case None =>
        case Some(predictedMutability) => {
          coutputs.declareTemplateMutability(
            interfaceTemplateFullName,
            MutabilityTemplata(Conversions.evaluateMutability(predictedMutability)))
        }
      }

      val placeholders = vimpl()

      val inferences =
        inferCompiler.solveExpectComplete(
          env,
          vimpl(),
          coutputs,
          interfaceA.rules,
          interfaceA.runeToType,
          interfaceA.range,
          placeholders,
//          interfaceA.genericParameters.zip(templateArgs).map({ case (GenericParameterS(rune, default), genericArg) =>
//            InitialKnown(RuneUsage(rune.range, rune.rune), genericArg)
//          }),
          Vector())

      interfaceA.maybePredictedMutability match {
        case None => {
          val mutability = ITemplata.expectMutability(inferences(interfaceA.mutabilityRune.rune))
          coutputs.declareTemplateMutability(interfaceTemplateFullName, mutability)
        }
        case Some(_) =>
      }

      middle.compileInterface(env, coutputs, interfaceA, inferences)
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
      val interfaceName = interfaceTemplateName.makeInterfaceName(interner, templateArgs)
      val fullName = env.fullName.addStep(interfaceName)
      val interfaceTT = interner.intern(InterfaceTT(fullName))

      // not sure if this is okay or not, do we allow this?
      if (templateArgs.size != interfaceA.genericParameters.size) {
        vfail("wat?")
      }

      val callSiteRules = interfaceA.rules.filter(inferCompiler.includeRuleInCallSiteSolve)

      // This checks to make sure it's a valid use of this template.
      val inferences =
        inferCompiler.solveExpectComplete(
          env,
          Some(callingEnv),
          coutputs,
          callSiteRules,
          interfaceA.runeToType,
          callRange,
          interfaceA.genericParameters.map(_.rune.rune).zip(templateArgs)
            .map({ case (a, b) => InitialKnown(RuneUsage(callRange, a), b) }),
          Vector())

      interfaceTT
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
