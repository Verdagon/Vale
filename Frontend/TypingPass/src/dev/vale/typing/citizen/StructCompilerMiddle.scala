package dev.vale.typing.citizen

import dev.vale.highertyping.{FunctionA, InterfaceA, StructA}
import dev.vale.postparsing._
import dev.vale.typing.env.{CitizenEnvironment, IEnvironment, TemplataEnvEntry, TemplatasStore}
import dev.vale.typing.{CompilerOutputs, TypingPassOptions, env}
import dev.vale.typing.names.{AnonymousSubstructNameT, FullNameT, IInterfaceTemplateNameT, IStructTemplateNameT, ITemplateNameT, NameTranslator, RuneNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.{Interner, Keywords, Profiler, RangeS, vfail, vimpl}
import dev.vale.highertyping.FunctionA
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.postparsing._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.env.CitizenEnvironment
import dev.vale.typing.function.FunctionCompiler

import scala.collection.immutable.List

class StructCompilerMiddle(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    delegate: IStructCompilerDelegate) {
  val core = new StructCompilerCore(opts, interner, keywords, nameTranslator, delegate)

  def compileStruct(
    structOuterEnv: IEnvironment,
    structTemplateFullName: FullNameT[IStructTemplateNameT],
    coutputs: CompilerOutputs,
    structS: StructA,
    templatasByRune: Map[IRuneS, ITemplata[ITemplataType]]):
  Unit = {
    val coercedFinalTemplateArgs2 = structS.genericParameters.map(_.rune.rune).map(templatasByRune)

    val localEnv =
      CitizenEnvironment(
        structOuterEnv.globalEnv,
        structOuterEnv,
        structTemplateFullName,
        structOuterEnv.fullName,
        TemplatasStore(structOuterEnv.fullName, Map(), Map())
          .addEntries(
            interner,
            templatasByRune.toVector
              .map({ case (rune, templata) => (interner.intern(RuneNameT(rune)), TemplataEnvEntry(templata)) })))
    core.compileStruct(
      localEnv,
      structTemplateFullName,
      coutputs, structS, coercedFinalTemplateArgs2)
  }

  def compileInterface(
    interfaceOuterEnv: IEnvironment,
    coutputs: CompilerOutputs,
    interfaceTemplateName: FullNameT[IInterfaceTemplateNameT],
    interfaceA: InterfaceA,
    templatasByRune: Map[IRuneS, ITemplata[ITemplataType]]):
  Unit = {
    val coercedFinalTemplateArgs2 = interfaceA.genericParameters.map(_.rune.rune).map(templatasByRune)

    val localEnv =
      env.CitizenEnvironment(
        interfaceOuterEnv.globalEnv,
        interfaceOuterEnv,
        interfaceTemplateName,
        interfaceOuterEnv.fullName,
        env.TemplatasStore(interfaceOuterEnv.fullName, Map(), Map())
          .addEntries(
            interner,
            templatasByRune.toVector
              .map({ case (rune, templata) => (interner.intern(RuneNameT(rune)), TemplataEnvEntry(templata)) })))
    core.compileInterface(localEnv, coutputs, interfaceTemplateName, interfaceA, coercedFinalTemplateArgs2)
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    coutputs: CompilerOutputs,
    name: IFunctionDeclarationNameS,
    functionS: FunctionA,
    members: Vector[StructMemberT]):
  (StructTT, MutabilityT, FunctionTemplata) = {
    core.makeClosureUnderstruct(containingFunctionEnv, coutputs, name, functionS, members)
  }
}
