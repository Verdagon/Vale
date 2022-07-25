package dev.vale.typing.function

import dev.vale.{Err, Interner, Keywords, Ok, Profiler, RangeS, typing, vassert, vassertSome, vcurious, vimpl}
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.rules.{IRulexSR, RuneUsage}
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.function.FunctionCompiler.IEvaluateFunctionResult
import dev.vale.postparsing.patterns._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.postparsing.{IEnvironmentS => _, _}
import dev.vale.typing.OverloadResolver.InferFailure
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.env._
import FunctionCompiler.{EvaluateFunctionFailure, EvaluateFunctionSuccess, IEvaluateFunctionResult}
import dev.vale.solver.{CompleteSolve, FailedSolve, IncompleteSolve}
import dev.vale.typing.ast.{FunctionBannerT, FunctionHeaderT, PrototypeT}
import dev.vale.typing.env.{BuildingFunctionEnvironmentWithClosureds, BuildingFunctionEnvironmentWithClosuredsAndTemplateArgs, TemplataEnvEntry, TemplataLookupContext}
import dev.vale.typing.{CompilerOutputs, ConvertHelper, InferCompiler, InitialKnown, InitialSend, TemplataCompiler, TypingPassOptions}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsAndTemplateArgsT, FullNameT, NameTranslator, PlaceholderNameT, PlaceholderTemplateNameT, RuneNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types.CoordT
//import dev.vale.typingpass.infer.{InferSolveFailure, InferSolveSuccess}
import dev.vale.vwat

import scala.collection.immutable.{List, Set}

// When typingpassing a function, these things need to happen:
// - Spawn a local environment for the function
// - Add any closure args to the environment
// - Incorporate any template arguments into the environment
// There's a layer to take care of each of these things.
// This file is the outer layer, which spawns a local environment for the function.
class FunctionCompilerOrdinaryOrTemplatedLayer(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler,
    convertHelper: ConvertHelper,
    structCompiler: StructCompiler,
    delegate: IFunctionCompilerDelegate) {
  val middleLayer = new FunctionCompilerMiddleLayer(opts, interner, keywords, nameTranslator, templataCompiler, convertHelper, structCompiler, delegate)

//  // This is for the early stages of Compiler when it's scanning banners to put in
//  // its env. We just want its banner, we don't want to evaluate it.
//  def predictOrdinaryFunctionBanner(
//    // The environment the function was defined in.
//    nearEnv: BuildingFunctionEnvironmentWithClosureds,
//    coutputs: CompilerOutputs):
//  (FunctionBannerT) = {
//    val function = nearEnv.function
//    checkClosureConcernsHandled(nearEnv)
//
//    val inferences =
//      inferCompiler.solveExpectComplete(
//        nearEnv, coutputs, vimpl()/*function.rules*/, function.runeToType, function.range, Vector(), Vector())
//    val runedEnv = addRunedDataToNearEnv(nearEnv, Vector.empty, inferences)
//
//    middleLayer.predictOrdinaryFunctionBanner(
//      runedEnv, coutputs, function)
//  }

  def evaluateOrdinaryFunctionFromNonCallForBanner(
      // The environment the function was defined in.
      nearEnv: BuildingFunctionEnvironmentWithClosureds,
      coutputs: CompilerOutputs,
      callRange: RangeS):
  (FunctionBannerT) = {
    val function = nearEnv.function
    checkClosureConcernsHandled(nearEnv)
    vassert(!function.isTemplate)

    val definitionRules =
      function.rules.filter(InferCompiler.includeRuleInDefinitionSolve)

    val inferences =
      inferCompiler.solveExpectComplete(
        nearEnv, None, coutputs, definitionRules, function.runeToType, function.range, Vector(), Vector())
    val runedEnv = addRunedDataToNearEnv(nearEnv, Vector.empty, inferences)

    middleLayer.getGenericFunctionBannerFromCall(runedEnv, coutputs, callRange, function)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // func main():Int{main()}
  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForPrototype(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosureds,
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
    args: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    val function = nearEnv.function
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)
    vassert(nearEnv.function.isTemplate)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, explicitTemplateArgs.size)

    val initialSends = assembleInitialSendsFromArgs(callRange, function, args)
    val inferredTemplatas =
      inferCompiler.solveComplete(
        nearEnv,
        Some(callingEnv),
        coutputs,
        callSiteRules,
        function.runeToType,
        callRange,
        assembleKnownTemplatas(function, args, explicitTemplateArgs),
        initialSends
      ) match {
        case Err(e) => return (EvaluateFunctionFailure(InferFailure(e)))
        case Ok(i) => (i)
      }

    val runedEnv = addRunedDataToNearEnv(nearEnv, function.genericParameters.map(_.rune.rune), inferredTemplatas)

    val header =
      middleLayer.getOrEvaluateFunctionForHeader(
        runedEnv, coutputs, callRange, function)

    EvaluateFunctionSuccess(PrototypeTemplata(function.range, header.toPrototype))
  }

  private def assembleInitialSendsFromArgs(callRange: RangeS, function: FunctionA, args: Vector[CoordT]):
  Vector[InitialSend] = {
    function.params.map(_.pattern.coordRune.get).zip(args).zipWithIndex
      .map({ case ((paramRune, argTemplata), argIndex) =>
        InitialSend(RuneUsage(callRange, ArgumentRuneS(argIndex)), paramRune, CoordTemplata(argTemplata))
      })
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForBanner(
      // The environment the function was defined in.
      declaringEnv: BuildingFunctionEnvironmentWithClosureds,
      coutputs: CompilerOutputs,
      callingEnv: IEnvironment, // See CSSNCE
      callRange: RangeS,
      alreadySpecifiedTemplateArgs: Vector[ITemplata[ITemplataType]],
      args: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    val function = declaringEnv.function
    // Check preconditions
    checkClosureConcernsHandled(declaringEnv)
    vassert(declaringEnv.function.isTemplate)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, 0)

    val initialSends = assembleInitialSendsFromArgs(callRange, function, args)
    val inferredTemplatas =
      inferCompiler.solveComplete(
        declaringEnv,
        Some(callingEnv),
        coutputs,
        callSiteRules,
        function.runeToType,
        callRange,
        assembleKnownTemplatas(function, args, alreadySpecifiedTemplateArgs),
        initialSends
      ) match {
        case Err(e) => return (EvaluateFunctionFailure(InferFailure(e)))
        case Ok(i) => (i)
      }

    val runedEnv =
      addRunedDataToNearEnv(
        declaringEnv, function.genericParameters.map(_.rune.rune), inferredTemplatas)

    val banner =
      middleLayer.getOrEvaluateFunctionForBanner(
        runedEnv, coutputs, callRange, function)
    (EvaluateFunctionSuccess(banner))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateOrdinaryFunctionFromNonCallForHeader(
      // The environment the function was defined in.
      declaringEnv: BuildingFunctionEnvironmentWithClosureds,
      coutputs: CompilerOutputs):
  (FunctionHeaderT) = {
    val function = declaringEnv.function
    // Check preconditions
    checkClosureConcernsHandled(declaringEnv)
    vassert(!function.isTemplate)

    val definitionRules =
      function.rules.filter(
        InferCompiler.includeRuleInDefinitionSolve)

    val inferences =
      inferCompiler.solveExpectComplete(
        declaringEnv, None, coutputs, definitionRules, function.runeToType, function.range, Vector(), Vector())
    val runedEnv = addRunedDataToNearEnv(declaringEnv, Vector.empty, inferences)

    middleLayer.getOrEvaluateFunctionForHeader(
      runedEnv, coutputs, function.range, function)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateOrdinaryFunctionFromCallForPrototype(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosureds,
    callingEnv: IEnvironment, // See CSSNCE
    coutputs: CompilerOutputs):
  (PrototypeTemplata) = {
    val function = nearEnv.function
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)
    vassert(!function.isTemplate)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, 0)

    val inferences =
      inferCompiler.solveExpectComplete(
        nearEnv, Some(callingEnv), coutputs, callSiteRules, function.runeToType, function.range, Vector(), Vector())
    val runedEnv = addRunedDataToNearEnv(nearEnv, Vector.empty, inferences)

    middleLayer.getOrEvaluateOrdinaryFunctionForPrototype(
      runedEnv, coutputs, function.range, function)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateTemplatedFunctionFromNonCallForHeader(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosureds,
    coutputs: CompilerOutputs):
  (FunctionHeaderT) = {
    val function = nearEnv.function
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)

    // Check preconditions
    function.body match {
      case CodeBodyS(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(nearEnv.function.isTemplate)

    // See IMCBT for why we can look up identifying runes in the environment.
    val initialKnowns =
      function.genericParameters.flatMap(genericParam => {
        nearEnv.lookupNearestWithName(
          interner.intern(RuneNameT(genericParam.rune.rune)), Set(TemplataLookupContext))
          .map(InitialKnown(genericParam.rune, _))
      })

    val definitionRules =
      function.rules.filter(
        InferCompiler.includeRuleInDefinitionSolve)

    val inferences =
      inferCompiler.solveExpectComplete(
        nearEnv, None, coutputs, definitionRules, function.runeToType, function.range, initialKnowns, Vector())

    // See FunctionCompiler doc for what outer/runes/inner envs are.
    val runedEnv =
      addRunedDataToNearEnv(
        nearEnv, function.genericParameters.map(_.rune.rune), inferences)

    middleLayer.getOrEvaluateFunctionForHeader(
      runedEnv, coutputs, function.range, function)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // func main():Int{main()}
  def evaluateOrdinaryFunctionFromCallForPrototype(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosureds,
    callingEnv: IEnvironment, // See CSSNCE
    coutputs: CompilerOutputs,
    callRange: RangeS):
  (PrototypeTemplata) = {
    val function = nearEnv.function
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)
    vassert(!function.isTemplate)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, 0)

    val inferences =
      inferCompiler.solveExpectComplete(
        nearEnv, Some(callingEnv), coutputs, callSiteRules, function.runeToType, function.range, Vector(), Vector())
    val runedEnv = addRunedDataToNearEnv(nearEnv, Vector.empty, inferences)

    middleLayer.getOrEvaluateOrdinaryFunctionForPrototype(
      runedEnv, coutputs, callRange, function)
  }


  // This is called while we're trying to figure out what functionSs to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      // The environment the function was defined in.
      nearEnv: BuildingFunctionEnvironmentWithClosureds,
      coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
      explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
      args: Vector[CoordT]):
  (IEvaluateFunctionResult[FunctionBannerT]) = {
    val function = nearEnv.function
    // Check preconditions
    function.body match {
      case CodeBodyS(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }
    vassert(nearEnv.function.isTemplate)

    val callSiteRules =
      TemplataCompiler.assembleCallSiteRules(
        function.rules, function.genericParameters, explicitTemplateArgs.size)

    val initialSends = assembleInitialSendsFromArgs(callRange, function, args)
    val initialKnowns = assembleKnownTemplatas(function, args, explicitTemplateArgs)
    val inferences =
      inferCompiler.solveComplete(
        nearEnv,
        Some(callingEnv),
        coutputs,
        callSiteRules,
        function.runeToType,
        callRange,
        initialKnowns,
        initialSends) match {
      case Err(e) => return EvaluateFunctionFailure(InferFailure(e))
      case Ok(inferredTemplatas) => inferredTemplatas
    }

    // See FunctionCompiler doc for what outer/runes/inner envs are.
    val runedEnv = addRunedDataToNearEnv(nearEnv, function.genericParameters.map(_.rune.rune), inferences)

    val banner =
      middleLayer.getGenericFunctionBannerFromCall(
        runedEnv, coutputs, callRange, function)

    (EvaluateFunctionSuccess(banner))
  }

  private def assembleKnownTemplatas(
    function: FunctionA,
    args: Vector[CoordT],
    explicitTemplateArgs: Vector[ITemplata[ITemplataType]]):
  Vector[InitialKnown] = {
    // Sometimes we look for an overload for a given override, assemble knowns from that here
//    args.zip(function.params).collect({
//      case (ParamFilter(_, Some(OverrideT(argOverrideKind))), ParameterS(AtomSP(_, _, Some(OverrideSP(_, paramOverrideRune)), _, _))) => {
//        InitialKnown(paramOverrideRune, KindTemplata(argOverrideKind))
//      }
//    }) ++
    function.genericParameters.zip(explicitTemplateArgs).map({
      case (genericParam, explicitArg) => {
        InitialKnown(genericParam.rune, explicitArg)
      }
    })
  }

  private def checkClosureConcernsHandled(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosureds
  ): Unit = {
    val function = nearEnv.function
    function.body match {
      case CodeBodyS(body1) => {
        body1.closuredNames.foreach(name => {
          vassert(nearEnv.variables.exists(_.id.last == nameTranslator.translateNameStep(name)))
        })
      }
      case _ =>
    }
  }

  // IOW, add the necessary data to turn the near env into the runed env.
  private def addRunedDataToNearEnv(
    nearEnv: BuildingFunctionEnvironmentWithClosureds,
    identifyingRunes: Vector[IRuneS],
    templatasByRune: Map[IRuneS, ITemplata[ITemplataType]]
  ): BuildingFunctionEnvironmentWithClosuredsAndTemplateArgs = {
    val BuildingFunctionEnvironmentWithClosureds(globalEnv, parentEnv, fullName, templatas, function, variables) = nearEnv

    val identifyingTemplatas = identifyingRunes.map(templatasByRune)
    val newName =
      FullNameT(
        fullName.packageCoord,
        fullName.initSteps,
        BuildingFunctionNameWithClosuredsAndTemplateArgsT(
          fullName.last, identifyingTemplatas))

    val newEntries =
      templatas.addEntries(
        interner,
        templatasByRune.toVector
          .map({ case (k, v) => (interner.intern(RuneNameT(k)), TemplataEnvEntry(v)) }))

    BuildingFunctionEnvironmentWithClosuredsAndTemplateArgs(
      globalEnv, parentEnv, newName, newEntries, function, variables)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // func main():Int{main()}
  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateGenericFunctionFromCallForPrototype(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosureds,
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: RangeS,
    explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
    args: Vector[CoordT]):
  (IEvaluateFunctionResult[PrototypeTemplata]) = {
    val function = nearEnv.function
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, explicitTemplateArgs.size)

    val initialSends = assembleInitialSendsFromArgs(callRange, function, args)
    val inferredTemplatas =
      inferCompiler.solveComplete(
        nearEnv,
        Some(callingEnv),
        coutputs,
        callSiteRules,
        function.runeToType,
        callRange,
        assembleKnownTemplatas(function, args, explicitTemplateArgs),
        initialSends
      ) match {
        case Err(e) => return (EvaluateFunctionFailure(InferFailure(e)))
        case Ok(i) => (i)
      }

    val runedEnv = addRunedDataToNearEnv(nearEnv, function.genericParameters.map(_.rune.rune), inferredTemplatas)

    val prototype =
      middleLayer.getGenericFunctionPrototypeFromCall(
        runedEnv, coutputs, callRange, function)

    EvaluateFunctionSuccess(PrototypeTemplata(function.range, prototype))
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  def evaluateGenericFunctionFromNonCall(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosureds,
    coutputs: CompilerOutputs):
  (FunctionHeaderT) = {
    val function = nearEnv.function
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)

    val functionTemplateFullName =
      nearEnv.parentEnv.fullName.addStep(
        nameTranslator.translateFunctionNameToTemplateName(nearEnv.function.name))

    val definitionRules =
      function.rules.filter(
        InferCompiler.includeRuleInDefinitionSolve)

    // This is temporary, to support specialization like:
    //   extern("vale_runtime_sized_array_mut_new")
    //   func Array<M, E>(size int) []<M>E
    //   where M Mutability = mut, E Ref;
    // In the future we might need to outlaw specialization, unsure.
    val preliminaryInferences =
      inferCompiler.solve(
        nearEnv, Some(nearEnv), coutputs, definitionRules, function.runeToType, function.range, Vector(), Vector()) match {
        case f @ FailedSolve(_, _, err) => {
          throw CompileErrorExceptionT(typing.TypingPassSolverError(function.range, f))
        }
        case IncompleteSolve(_, _, _, incompleteConclusions) => incompleteConclusions
        case CompleteSolve(conclusions) => conclusions
      }
    // Now we can use preliminaryInferences to know whether or not we need a placeholder for an identifying rune.

    val initialKnowns =
      function.genericParameters.zipWithIndex.flatMap({ case (genericParam, index) =>
        preliminaryInferences.get(genericParam.rune.rune) match {
          case Some(x) => Some(InitialKnown(genericParam.rune, x))
          case None => {
            // Make a placeholder for every argument even if it has a default, see DUDEWCD.
            val runeType = vassertSome(function.runeToType.get(genericParam.rune.rune))
            val templata = templataCompiler.createPlaceholder(coutputs, nearEnv, functionTemplateFullName, index, runeType)
            Some(InitialKnown(genericParam.rune, templata))
          }
        }
      })

    val inferences =
      inferCompiler.solveExpectComplete(
        nearEnv, Some(nearEnv), coutputs, definitionRules, function.runeToType, function.range, initialKnowns, Vector())
    val runedEnv = addRunedDataToNearEnv(nearEnv, function.genericParameters.map(_.rune.rune), inferences)

    middleLayer.getOrEvaluateFunctionForHeader(
      runedEnv, coutputs, function.range, function)
  }
}
