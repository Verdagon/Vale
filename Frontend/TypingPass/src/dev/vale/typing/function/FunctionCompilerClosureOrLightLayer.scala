package dev.vale.typing.function

import dev.vale.{Interner, Keywords, Profiler, RangeS, vassert, vfail, vimpl}
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing._
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.postparsing.IFunctionDeclarationNameS
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.env._
import FunctionCompiler.IEvaluateFunctionResult
import dev.vale.typing.ast.{FunctionBannerT, FunctionHeaderT, PrototypeT}
import dev.vale.typing.env.{AddressibleClosureVariableT, BuildingFunctionEnvironmentWithClosureds, IEnvEntry, IEnvironment, IVariableT, ReferenceClosureVariableT, TemplataEnvEntry, TemplatasStore}
import dev.vale.typing.{CompilerOutputs, ConvertHelper, InferCompiler, TemplataCompiler, TypingPassOptions, env}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, FullNameT, IFunctionTemplateNameT, INameT, NameTranslator}
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.{List, Map}

// When typingpassing a function, these things need to happen:
// - Spawn a local environment for the function
// - Add any closure args to the environment
// - Incorporate any template arguments into the environment
// There's a layer to take care of each of these things.
// This file is the outer layer, which spawns a local environment for the function.
class FunctionCompilerClosureOrLightLayer(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler,
    convertHelper: ConvertHelper,
    structCompiler: StructCompiler,
    delegate: IFunctionCompilerDelegate) {
  val ordinaryOrTemplatedLayer =
    new FunctionCompilerOrdinaryOrTemplatedLayer(
      opts, interner, keywords, nameTranslator, templataCompiler, inferCompiler, convertHelper, structCompiler, delegate)
//
//  // This is for the early stages of Compiler when it's scanning banners to put in
//  // its env. We just want its banner, we don't want to evaluate it.
//  def predictOrdinaryLightFunctionBanner(
//    outerEnv: IEnvironment,
//    coutputs: CompilerOutputs,
//    function: FunctionA):
//  (FunctionBannerT) = {
//    checkNotClosure(function);
//    vassert(!function.isTemplate)
//
//    val newEnv = makeEnvWithoutClosureStuff(outerEnv, function)
//    ordinaryOrTemplatedLayer.predictOrdinaryFunctionBanner(
//      newEnv, coutputs)
//  }


  def evaluateOrdinaryLightFunctionFromNonCallForBanner(
      outerEnv: IEnvironment,
      coutputs: CompilerOutputs,
    callRange: RangeS,
    function: FunctionA):
  (FunctionBannerT) = {
    checkNotClosure(function);
    vassert(!function.isTemplate)

    val newEnv = makeEnvWithoutClosureStuff(outerEnv, function)
    coutputs.declareTemplate(newEnv.fullName)
    coutputs.declareEnvForTemplate(newEnv.fullName, outerEnv)
    ordinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      newEnv, coutputs, callRange)
  }

  def evaluateTemplatedClosureFunctionFromCallForBanner(
      declaringEnv: IEnvironment,
      coutputs: CompilerOutputs,
      callingEnv: IEnvironment,
      callRange: RangeS,
      closureStructRef: StructTT,
      function: FunctionA,
      alreadySpecifiedTemplateArgs: Vector[ITemplata[ITemplataType]],
      argTypes2: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    vassert(function.isTemplate)

    val (variables, entries) = makeClosureVariablesAndEntries(coutputs, closureStructRef)
    val name = makeNameWithClosureds(declaringEnv, function.name)
    coutputs.declareTemplate(name)
    coutputs.declareEnvForTemplate(name, declaringEnv)
    val newEnv =
      BuildingFunctionEnvironmentWithClosureds(
        declaringEnv.globalEnv,
        declaringEnv,
        name,
        TemplatasStore(name, Map(), Map()).addEntries(interner, entries),
        function,
        variables)

    ordinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
      newEnv, coutputs, callingEnv, callRange, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
    outerEnv: IEnvironment,
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment,
    callRange: RangeS,
    closureStructRef: StructTT,
    function: FunctionA,
    alreadySpecifiedTemplateArgs: Vector[ITemplata[ITemplataType]],
    argTypes2: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    vassert(function.isTemplate)

    val (variables, entries) = makeClosureVariablesAndEntries(coutputs, closureStructRef)
    val name = makeNameWithClosureds(outerEnv, function.name)
    coutputs.declareTemplate(name)
    coutputs.declareEnvForTemplate(name, outerEnv)
    val newEnv =
      env.BuildingFunctionEnvironmentWithClosureds(
        outerEnv.globalEnv,
        outerEnv,
        name,
        TemplatasStore(name, Map(), Map()).addEntries(interner, entries),
        function,
        variables)
    ordinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      newEnv, coutputs, callingEnv, callRange, alreadySpecifiedTemplateArgs, argTypes2)
  }

  def evaluateTemplatedLightFunctionFromCallForPrototype2(
      ourEnv: IEnvironment,
      coutputs: CompilerOutputs,
      callingEnv: IEnvironment, // See CSSNCE
      callRange: RangeS,
      function: FunctionA,
      explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
      args: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    checkNotClosure(function);
    vassert(function.isTemplate)

    val newEnv = makeEnvWithoutClosureStuff(ourEnv, function)
    coutputs.declareTemplate(newEnv.fullName)
    coutputs.declareEnvForTemplate(newEnv.fullName, ourEnv)
    ordinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForPrototype(
      newEnv, coutputs, callingEnv, callRange, explicitTemplateArgs, args)
  }

  def evaluateGenericLightFunctionFromCallForPrototype2(
    ourEnv: IEnvironment,
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    function: FunctionA,
    explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
    args: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    checkNotClosure(function);

    val newEnv = makeEnvWithoutClosureStuff(ourEnv, function)
    ordinaryOrTemplatedLayer.evaluateGenericFunctionFromCallForPrototype(
      newEnv, coutputs, callingEnv, callRange, explicitTemplateArgs, args)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForHeader(
      outerEnv: IEnvironment,
      coutputs: CompilerOutputs,
    function: FunctionA):
  (FunctionHeaderT) = {
    vassert(!function.isTemplate)

    val newEnv = makeEnvWithoutClosureStuff(outerEnv, function)
    coutputs.declareTemplate(newEnv.fullName)
    coutputs.declareEnvForTemplate(newEnv.fullName, outerEnv)
    ordinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      newEnv, coutputs)
  }

  def evaluateGenericLightFunctionFromNonCall(
    outerEnv: IEnvironment,
    coutputs: CompilerOutputs,
    function: FunctionA):
  (FunctionHeaderT) = {
    val newEnv = makeEnvWithoutClosureStuff(outerEnv, function)
    coutputs.declareTemplate(newEnv.fullName)
    coutputs.declareEnvForTemplate(newEnv.fullName, outerEnv)
    ordinaryOrTemplatedLayer.evaluateGenericFunctionFromNonCall(
      newEnv, coutputs)
  }

  def evaluateTemplatedLightFunctionFromNonCallForHeader(
    outerEnv: IEnvironment,
    coutputs: CompilerOutputs,
    function: FunctionA):
  (FunctionHeaderT) = {
    vassert(function.isTemplate)

    val newEnv = makeEnvWithoutClosureStuff(outerEnv, function)
    coutputs.declareTemplate(newEnv.fullName)
    coutputs.declareEnvForTemplate(newEnv.fullName, outerEnv)
    ordinaryOrTemplatedLayer.evaluateTemplatedFunctionFromNonCallForHeader(
      newEnv, coutputs)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // func main():Int{main()}
  def evaluateOrdinaryLightFunctionFromCallForPrototype(
    outerEnv: IEnvironment,
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    function: FunctionA
  ): PrototypeTemplata = {
    checkNotClosure(function)
    vassert(!function.isTemplate)

    val name = makeNameWithClosureds(outerEnv, function.name)
    coutputs.declareTemplate(name)
    coutputs.declareEnvForTemplate(name, outerEnv)
    val newEnv =
      env.BuildingFunctionEnvironmentWithClosureds(
        outerEnv.globalEnv,
        outerEnv,
        name,
        TemplatasStore(name, Map(), Map()),
        function,
        Vector.empty)
    ordinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromCallForPrototype(
      newEnv, callingEnv, coutputs, callRange)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForBanner(
    outerEnv: IEnvironment,
    coutputs: CompilerOutputs,
    callRange: RangeS,
    closureStructRef: StructTT,
    function: FunctionA):
  (FunctionBannerT) = {
    vassert(!function.isTemplate)

    val name = makeNameWithClosureds(outerEnv, function.name)
    coutputs.declareTemplate(name)
    coutputs.declareEnvForTemplate(name, outerEnv)
    val (variables, entries) = makeClosureVariablesAndEntries(coutputs, closureStructRef)
    val newEnv =
      env.BuildingFunctionEnvironmentWithClosureds(
        outerEnv.globalEnv,
        outerEnv,
        name,
        TemplatasStore(name, Map(), Map()).addEntries(interner, entries),
        function,
        variables)
    ordinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForBanner(
      newEnv, coutputs, callRange)
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForHeader(
      outerEnv: IEnvironment,
      coutputs: CompilerOutputs,
      closureStructRef: StructTT,
    function: FunctionA):
  (FunctionHeaderT) = {
    // We dont here because it knows from how many variables
    // it closures... but even lambdas without closured vars are still closures and are still
    // backed by structs.
    vassert(!function.isTemplate)

    val name = makeNameWithClosureds(outerEnv, function.name)
    coutputs.declareTemplate(name)
    coutputs.declareEnvForTemplate(name, outerEnv)
    val (variables, entries) = makeClosureVariablesAndEntries(coutputs, closureStructRef)
    val newEnv =
      env.BuildingFunctionEnvironmentWithClosureds(
        outerEnv.globalEnv,
        outerEnv,
        name,
        TemplatasStore(name, Map(), Map()).addEntries(interner, entries),
        function,
        variables)
    ordinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromNonCallForHeader(
      newEnv, coutputs)
  }

  def evaluateOrdinaryClosureFunctionFromCallForPrototype(
    outerEnv: IEnvironment,
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    closureStructRef: StructTT,
    function: FunctionA):
  (PrototypeTemplata) = {
    // We dont here because it knows from how many variables
    // it closures... but even lambdas without closured vars are still closures and are still
    // backed by structs.
    vassert(!function.isTemplate)

    val name = makeNameWithClosureds(outerEnv, function.name)
    coutputs.declareTemplate(name)
    coutputs.declareEnvForTemplate(name, outerEnv)
    val (variables, entries) = makeClosureVariablesAndEntries(coutputs, closureStructRef)
    val newEnv =
      env.BuildingFunctionEnvironmentWithClosureds(
        outerEnv.globalEnv,
        outerEnv,
        name,
        TemplatasStore(name, Map(), Map()).addEntries(interner, entries),
        function,
        variables)
    ordinaryOrTemplatedLayer.evaluateOrdinaryFunctionFromCallForPrototype(
      newEnv, callingEnv, coutputs)
  }

  def evaluateTemplatedClosureFunctionFromNonCallForHeader(
    outerEnv: IEnvironment,
    coutputs: CompilerOutputs,
    closureStructRef: StructTT,
    function: FunctionA):
  (FunctionHeaderT) = {
    // We dont here because it knows from how many variables
    // it closures... but even lambdas without closured vars are still closures and are still
    // backed by structs.
    vassert(!function.isTemplate)

    val name = makeNameWithClosureds(outerEnv, function.name)
    coutputs.declareTemplate(name)
    coutputs.declareEnvForTemplate(name, outerEnv)
    val (variables, entries) = makeClosureVariablesAndEntries(coutputs, closureStructRef)
    val newEnv =
      env.BuildingFunctionEnvironmentWithClosureds(
        outerEnv.globalEnv,
        outerEnv,
        name,
        TemplatasStore(name, Map(), Map()).addEntries(interner, entries),
        function,
        variables)
    ordinaryOrTemplatedLayer.evaluateTemplatedFunctionFromNonCallForHeader(
      newEnv, coutputs)
  }

  // This is called while we're trying to figure out what function1s to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      declaringEnv: IEnvironment,
      coutputs: CompilerOutputs,
      callingEnv: IEnvironment, // See CSSNCE
      callRange: RangeS,
      function: FunctionA,
      explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
      args: Vector[CoordT]):
  (IEvaluateFunctionResult[FunctionBannerT]) = {
    checkNotClosure(function)
    vassert(function.isTemplate)

    val newEnv = makeEnvWithoutClosureStuff(declaringEnv, function)
    coutputs.declareTemplate(newEnv.fullName)
    coutputs.declareEnvForTemplate(newEnv.fullName, declaringEnv)
    ordinaryOrTemplatedLayer.evaluateTemplatedLightBannerFromCall(
        newEnv, coutputs, callingEnv, callRange, explicitTemplateArgs, args)
  }

  def evaluateTemplatedFunctionFromCallForBanner(
      outerEnv: IEnvironment,
      coutputs: CompilerOutputs,
      callingEnv: IEnvironment, // See CSSNCE
      function: FunctionA,
      callRange: RangeS,
      alreadySpecifiedTemplateArgs: Vector[ITemplata[ITemplataType]],
      paramFilters: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    vassert(function.isTemplate)

    val newEnv = makeEnvWithoutClosureStuff(outerEnv, function)
    coutputs.declareTemplate(newEnv.fullName)
    coutputs.declareEnvForTemplate(newEnv.fullName, outerEnv)
    ordinaryOrTemplatedLayer.evaluateTemplatedFunctionFromCallForBanner(
        newEnv, coutputs, callingEnv, callRange, alreadySpecifiedTemplateArgs, paramFilters)
  }

  private def makeEnvWithoutClosureStuff(
    outerEnv: IEnvironment,
    function: FunctionA
  ): BuildingFunctionEnvironmentWithClosureds = {
    val name = makeNameWithClosureds(outerEnv, function.name)
    env.BuildingFunctionEnvironmentWithClosureds(
      outerEnv.globalEnv,
      outerEnv,
      name,
      TemplatasStore(name, Map(), Map()),
      function,
      Vector.empty)
  }

  private def makeNameWithClosureds(
    outerEnv: IEnvironment,
    functionName: IFunctionDeclarationNameS
  ): FullNameT[IFunctionTemplateNameT] = {
    outerEnv.fullName.addStep(nameTranslator.translateFunctionNameToTemplateName(functionName))
  }

  private def checkNotClosure(function: FunctionA) = {
    function.body match {
      case CodeBodyS(body1) => vassert(body1.closuredNames.isEmpty)
      case ExternBodyS =>
      case GeneratedBodyS(_) =>
      case AbstractBodyS =>
      case _ => vfail()
    }
  }

  private def makeClosureVariablesAndEntries(coutputs: CompilerOutputs, closureStructRef: StructTT):
  (Vector[IVariableT], Vector[(INameT, IEnvEntry)]) = {
    val closureStructDef = coutputs.lookupStruct(closureStructRef);
    val substituter =
      TemplataCompiler.getPlaceholderSubstituter(interner, closureStructRef.fullName)
    val variables =
      closureStructDef.members.map(member => {
        val variableFullName = closureStructDef.templateName.addStep(member.name)
        member.tyype match {
          case AddressMemberTypeT(reference) => {
            AddressibleClosureVariableT(
              variableFullName, closureStructRef, member.variability, substituter.substituteForCoord(reference))
          }
          case ReferenceMemberTypeT(reference) => {
            ReferenceClosureVariableT(
              variableFullName, closureStructRef, member.variability, substituter.substituteForCoord(reference))
          }
        }
      })
    val entries =
      Vector[(INameT, IEnvEntry)](
        closureStructRef.fullName.last ->
          TemplataEnvEntry(KindTemplata(closureStructRef)))
    (variables, entries)
  }
}
