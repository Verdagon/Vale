package dev.vale.typing.function

import dev.vale.{Err, Interner, Keywords, Ok, Profiler, RangeS, StrI, typing, vassert, vassertSome, vcurious, vfail, vimpl, vpass, vregionmut}
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.rules.{IRulexSR, RuneUsage}
import dev.vale.typing.citizen.StructCompiler
import dev.vale.postparsing.patterns._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.postparsing.{IEnvironmentS => _, _}
import dev.vale.typing.OverloadResolver._
import dev.vale.typing._
import dev.vale.typing.ast._
import dev.vale.typing.env._
import dev.vale.typing.function._
import dev.vale.solver.{CompleteSolve, FailedSolve, IncompleteSolve, Solver}
import dev.vale.typing.ast.{FunctionBannerT, FunctionHeaderT, PrototypeT}
import dev.vale.typing.env._
import dev.vale.typing.infer.ITypingPassSolverError
import dev.vale.typing.{CompilerOutputs, ConvertHelper, InferCompiler, InitialKnown, InitialSend, TemplataCompiler, TypingPassOptions}
import dev.vale.typing.names._
import dev.vale.typing.templata.ITemplataT._
import dev.vale.typing.templata._
import dev.vale.typing.types.CoordT

import scala.collection.immutable.HashMap
//import dev.vale.typingpass.infer.{InferSolveFailure, InferSolveSuccess}
import dev.vale.vwat

import scala.collection.immutable.{List, Set}

// When typingpassing a function, these things need to happen:
// - Spawn a local environment for the function
// - Add any closure args to the environment
// - Incorporate any template arguments into the environment
// There's a layer to take care of each of these things.
// This file is the outer layer, which spawns a local environment for the function.
class FunctionCompilerSolvingLayer(
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

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // func main():Int{main()}
  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForPrototype(
    // The environment the function was defined in.
    outerEnv: BuildingFunctionEnvironmentWithClosuredsT,
    coutputs: CompilerOutputs,
    originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    explicitTemplateArgs: Vector[ITemplataT[ITemplataType]],
    contextRegion: RegionT,
    args: Vector[CoordT]):
  (IEvaluateFunctionResult) = {
    val function = outerEnv.function
    // Check preconditions
    checkClosureConcernsHandled(outerEnv)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, explicitTemplateArgs.size)

    val initialSends = assembleInitialSendsFromArgs(callRange.head, function, args.map(Some(_)))
    val (inferredTemplatas, instantiationBoundParams) =
      inferCompiler.solveForDefiningStart(
        InferEnv(originalCallingEnv, callRange, callLocation, outerEnv, contextRegion),
        coutputs,
        callSiteRules,
        function.runeToType,
        callRange,
        callLocation,
        assembleKnownTemplatas(function, explicitTemplateArgs),
        initialSends,
        Vector()
      ) match {
        case Err(e) => throw CompileErrorExceptionT(TypingPassDefiningError(callRange, e))
        case Ok(i) => (i)
      }

    // DO NOT SUBMIT consider using the env from the above solveForDefining
    val runedEnv =
      addRunedDataToNearEnvForDefining(
        coutputs,
        outerEnv,
        function.genericParameters.map(_.rune.rune),
        inferredTemplatas,
        instantiationBoundParams)

    inferCompiler.solveForDefiningEnd(
      coutputs,
      callSiteRules,
      instantiationBoundParams,
      runedEnv
    ) match {
      case Err(e) => throw CompileErrorExceptionT(TypingPassDefiningError(callRange, e))
      case Ok(i) => (i)
    }

    val header =
      middleLayer.getOrEvaluateFunctionForHeader(
        outerEnv, runedEnv, coutputs, callRange, callLocation, function, instantiationBoundParams)

    // Lambdas cant have bounds, right?
    vcurious(instantiationBoundParams.runeToBoundPrototype.isEmpty)
    vcurious(instantiationBoundParams.runeToCitizenRuneToReachablePrototype.isEmpty)
    vcurious(instantiationBoundParams.runeToBoundImpl.isEmpty)
    val instantiationBoundArgs =
      InstantiationBoundArgumentsT[IFunctionNameT, IImplNameT](
        HashMap[IRuneS, PrototypeT[IFunctionNameT]](instantiationBoundParams.runeToBoundPrototype.toSeq: _*),
        HashMap[IRuneS, InstantiationReachableBoundArgumentsT[IFunctionNameT]](
        instantiationBoundParams.runeToCitizenRuneToReachablePrototype.map({ case (x, InstantiationReachableBoundArgumentsT(y)) =>
          x -> InstantiationReachableBoundArgumentsT[IFunctionNameT](y)
        }).toSeq: _*),
        HashMap[IRuneS, IdT[IImplNameT]](instantiationBoundParams.runeToBoundImpl.toSeq: _*))
    coutputs.addInstantiationBounds(
      opts.globalOptions.sanityCheck,
      interner, outerEnv.denizenTemplateId,
      header.id, instantiationBoundArgs)
    EvaluateFunctionSuccess(PrototypeTemplataT(header.toPrototype), inferredTemplatas, instantiationBoundArgs)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateTemplatedFunctionFromCallForBanner(
      // The environment the function was defined in.
      declaringEnv: BuildingFunctionEnvironmentWithClosuredsT,
      coutputs: CompilerOutputs,
      originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
      callRange: List[RangeS],
      callLocation: LocationInDenizen,
      alreadySpecifiedTemplateArgs: Vector[ITemplataT[ITemplataType]],
    contextRegion: RegionT,
      args: Vector[CoordT]):
  (IEvaluateFunctionResult) = {
    val function = declaringEnv.function
    // Check preconditions
    checkClosureConcernsHandled(declaringEnv)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, 0)

    val initialSends = assembleInitialSendsFromArgs(callRange.head, function, args.map(Some(_)))
    val (inferredTemplatas, instantiationBoundParams) = {
      // We could probably just solveForResolving (see DBDAR) but seems more future-proof to solveForDefining.
      inferCompiler.solveForDefiningStart(
        InferEnv(originalCallingEnv, callRange, callLocation, declaringEnv, contextRegion),
        coutputs,
        callSiteRules,
        function.runeToType,
        callRange,
        callLocation,
        assembleKnownTemplatas(function, alreadySpecifiedTemplateArgs),
        initialSends,
        Vector()
      ) match {
        case Err(e) => return EvaluateFunctionFailure(e)
        case Ok(i) => (i)
      }
    }

    // DO NOT SUBMIT consider using the env from the above solveForDefining
    // ...OR maybe split the above solveForDefining so we make our own environment then it does its own checks?
    val runedEnv =
    addRunedDataToNearEnvForDefining(
        coutputs,
        declaringEnv,
        function.genericParameters.map(_.rune.rune),
        inferredTemplatas,
        instantiationBoundParams)

    inferCompiler.solveForDefiningEnd(
      coutputs,
      callSiteRules,
      instantiationBoundParams,
      runedEnv
    ) match {
      case Err(e) => throw CompileErrorExceptionT(TypingPassDefiningError(callRange, e))
      case Ok(i) => (i)
    }

    val prototype =
      middleLayer.getOrEvaluateTemplatedFunctionForBanner(
        declaringEnv, runedEnv, coutputs, callRange, callLocation, function, instantiationBoundParams)

    // Lambdas cant have bounds, right?
    vcurious(instantiationBoundParams.runeToBoundPrototype.isEmpty)
    vcurious(instantiationBoundParams.runeToCitizenRuneToReachablePrototype.isEmpty)
    vcurious(instantiationBoundParams.runeToBoundImpl.isEmpty)
    val instantiationBoundArgs =
      InstantiationBoundArgumentsT[IFunctionNameT, IImplNameT](
        HashMap[IRuneS, PrototypeT[IFunctionNameT]](instantiationBoundParams.runeToBoundPrototype.toSeq: _*),
        HashMap[IRuneS, InstantiationReachableBoundArgumentsT[IFunctionNameT]](
        instantiationBoundParams.runeToCitizenRuneToReachablePrototype.map({ case (x, InstantiationReachableBoundArgumentsT(y)) =>
          x -> InstantiationReachableBoundArgumentsT[IFunctionNameT](y)
        }).toSeq: _*),
        HashMap[IRuneS, IdT[IImplNameT]](instantiationBoundParams.runeToBoundImpl.toSeq: _*))
    coutputs.addInstantiationBounds(
      opts.globalOptions.sanityCheck,
      interner, originalCallingEnv.denizenTemplateId,
      prototype.prototype.id, instantiationBoundArgs)
    EvaluateFunctionSuccess(prototype, inferredTemplatas, instantiationBoundArgs)
  }

  // This is called while we're trying to figure out what functionSs to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerFromCall(
      // The environment the function was defined in.
      nearEnv: BuildingFunctionEnvironmentWithClosuredsT,
      coutputs: CompilerOutputs,
    originalCallingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
      explicitTemplateArgs: Vector[ITemplataT[ITemplataType]],
    contextRegion: RegionT,
      args: Vector[CoordT]):
  (IEvaluateFunctionResult) = {
    val function = nearEnv.function
    // Check preconditions
    function.body match {
      case CodeBodyS(body1) => vassert(body1.closuredNames.isEmpty)
      case _ =>
    }

    val callSiteRules =
      TemplataCompiler.assembleCallSiteRules(
        function.rules, function.genericParameters, explicitTemplateArgs.size)

    val initialSends = assembleInitialSendsFromArgs(callRange.head, function, args.map(Some(_)))
    val initialKnowns = assembleKnownTemplatas(function, explicitTemplateArgs)
    val (inferences, instantiationBoundParams) =
    // We could probably just solveForResolving (see DBDAR) but seems more future-proof to solveForDefining.
      inferCompiler.solveForDefiningStart(
        InferEnv(originalCallingEnv, callRange, callLocation, nearEnv, contextRegion),
        coutputs,
        callSiteRules,
        function.runeToType,
        callRange,
        callLocation,
        initialKnowns,
        initialSends,
        Vector()) match {
      case Err(e) => return EvaluateFunctionFailure(e)
      case Ok(inferredTemplatas) => inferredTemplatas
    }

    // See FunctionCompiler doc for what outer/runes/inner envs are.
    // DO NOT SUBMIT consider using the env from the above solveForDefining
    val runedEnv =
    addRunedDataToNearEnvForDefining(
        coutputs,
        nearEnv,
        function.genericParameters.map(_.rune.rune),
        inferences,
        instantiationBoundParams)

    inferCompiler.solveForDefiningEnd(
      coutputs,
      callSiteRules,
      instantiationBoundParams,
      runedEnv
    ) match {
      case Err(e) => throw CompileErrorExceptionT(TypingPassDefiningError(callRange, e))
      case Ok(i) => (i)
    }

    val prototypeTemplata =
      middleLayer.getOrEvaluateTemplatedFunctionForBanner(
        nearEnv, runedEnv, coutputs, callRange, callLocation, function, instantiationBoundParams)

    // Lambdas cant have bounds, right?
    vcurious(instantiationBoundParams.runeToBoundPrototype.isEmpty)
    vcurious(instantiationBoundParams.runeToCitizenRuneToReachablePrototype.isEmpty)
    vcurious(instantiationBoundParams.runeToBoundImpl.isEmpty)
    val instantiationBoundArgs =
      InstantiationBoundArgumentsT[IFunctionNameT, IImplNameT](
        HashMap[IRuneS, PrototypeT[IFunctionNameT]](instantiationBoundParams.runeToBoundPrototype.toSeq: _*),
        HashMap[IRuneS, InstantiationReachableBoundArgumentsT[IFunctionNameT]](
        instantiationBoundParams.runeToCitizenRuneToReachablePrototype.map({ case (x, InstantiationReachableBoundArgumentsT(y)) =>
          x -> InstantiationReachableBoundArgumentsT[IFunctionNameT](y)
        }).toSeq: _*),
        HashMap[IRuneS, IdT[IImplNameT]](instantiationBoundParams.runeToBoundImpl.toSeq: _*))
    coutputs.addInstantiationBounds(
      opts.globalOptions.sanityCheck,
      interner, originalCallingEnv.denizenTemplateId,
      prototypeTemplata.prototype.id, instantiationBoundArgs)
    EvaluateFunctionSuccess(prototypeTemplata, inferences, instantiationBoundArgs)
  }

  private def assembleKnownTemplatas(
    function: FunctionA,
    explicitTemplateArgs: Vector[ITemplataT[ITemplataType]]):
  Vector[InitialKnown] = {
    function.genericParameters.zip(explicitTemplateArgs).map({
      case (genericParam, explicitArg) => {
        InitialKnown(genericParam.rune, explicitArg)
      }
    })
  }

  private def checkClosureConcernsHandled(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosuredsT
  ): Unit = {
    val function = nearEnv.function
    function.body match {
      case CodeBodyS(body1) => {
        body1.closuredNames.foreach(name => {
          vassert(nearEnv.variables.exists(_.name == nameTranslator.translateNameStep(name)))
        })
      }
      case _ =>
    }
  }

  // IOW, add the necessary data to turn the near env into the runed env.
  private def addRunedDataToNearEnvForResolving(
    nearEnv: BuildingFunctionEnvironmentWithClosuredsT,
    identifyingRunes: Vector[IRuneS],
    templatasByRune: Map[IRuneS, ITemplataT[ITemplataType]],
    reachableBoundsFromParamsAndReturn: Vector[PrototypeTemplataT[IFunctionNameT]]
    // I suspect we'll eventually need some impl bounds here
  ): BuildingFunctionEnvironmentWithClosuredsAndTemplateArgsT = {
    val BuildingFunctionEnvironmentWithClosuredsT(globalEnv, parentEnv, id, templatas, function, variables, isRootCompilingDenizen) = nearEnv

    val identifyingTemplatas = identifyingRunes.map(templatasByRune)

    val newEntries =
      templatas.addEntries(
        interner,
        reachableBoundsFromParamsAndReturn.zipWithIndex.toVector
          .map({ case (t, i) => (interner.intern(ReachablePrototypeNameT(i)), TemplataEnvEntry(t)) }) ++
        templatasByRune.toVector
          .map({ case (k, v) => (interner.intern(RuneNameT(k)), TemplataEnvEntry(v)) }))

    val defaultRegion = RegionT()

    BuildingFunctionEnvironmentWithClosuredsAndTemplateArgsT(
      globalEnv,
      parentEnv,
      id,
      identifyingTemplatas,
      newEntries,
      function,
      variables,
      isRootCompilingDenizen,
      defaultRegion)
  }

  // IOW, add the necessary data to turn the near env into the runed env.
  private def addRunedDataToNearEnvForDefining(
      coutputs: CompilerOutputs,
      nearEnv: BuildingFunctionEnvironmentWithClosuredsT,
      identifyingRunes: Vector[IRuneS],
      templatasByRune: Map[IRuneS, ITemplataT[ITemplataType]],
      instantiationBoundParams: InstantiationBoundArgumentsT[FunctionBoundNameT, ImplBoundNameT]
  ): BuildingFunctionEnvironmentWithClosuredsAndTemplateArgsT = {
    val BuildingFunctionEnvironmentWithClosuredsT(globalEnv, parentEnv, id, templatas, function, variables, isRootCompilingDenizen) = nearEnv

    val identifyingTemplatas = identifyingRunes.map(templatasByRune)

    val newEntries =
      templatas.addEntries(
        interner,
        inferCompiler.declareBoundsAndMakeEnvironmentTemplatas(coutputs, templatasByRune, instantiationBoundParams))

    val defaultRegion = RegionT()

    BuildingFunctionEnvironmentWithClosuredsAndTemplateArgsT(
      globalEnv,
      parentEnv,
      id,
      identifyingTemplatas,
      newEntries,
      function,
      variables,
      isRootCompilingDenizen,
      defaultRegion)
  }

  // We would want only the prototype instead of the entire header if, for example,
  // we were calling the function. This is necessary for a recursive function like
  // func main():Int{main()}
  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env is the environment the templated function was made in
  def evaluateGenericFunctionFromCallForPrototype(
    // The environment the function was defined in.
    outerEnv: BuildingFunctionEnvironmentWithClosuredsT,
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    explicitTemplateArgs: Vector[ITemplataT[ITemplataType]],
    contextRegion: RegionT,
    args: Vector[Option[CoordT]]):
  (IResolveFunctionResult) = {
    val function = outerEnv.function
    // Check preconditions
    checkClosureConcernsHandled(outerEnv)

    val callSiteRules =
        TemplataCompiler.assembleCallSiteRules(
            function.rules, function.genericParameters, explicitTemplateArgs.size)

    val initialSends = assembleInitialSendsFromArgs(callRange.head, function, args)


    val envs = InferEnv(callingEnv, callRange, callLocation, outerEnv, contextRegion)
    val rules = callSiteRules
    val runeToType = function.runeToType
    val invocationRange = callRange
    val initialKnowns = assembleKnownTemplatas(function, explicitTemplateArgs)
    val includeReachableBoundsForRunes =
      function.params.flatMap(_.pattern.coordRune.map(_.rune)) ++ function.maybeRetCoordRune.map(_.rune)

    val solver =
      inferCompiler.makeSolver(envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends)

    var loopCheck = function.genericParameters.size + 1

    // Incrementally solve and add default generic parameters (and context region).
    inferCompiler.incrementallySolve(
      envs, coutputs, solver,
      (solver) => {
        if (loopCheck == 0) {
          throw CompileErrorExceptionT(RangedInternalErrorT(callRange, "Infinite loop detected in incremental call solve!"))
        }
        loopCheck = loopCheck - 1

        TemplataCompiler.getFirstUnsolvedIdentifyingRune(
          function.genericParameters,
          (rune) => solver.getConclusion(rune).nonEmpty) match {
          case None => false
          case Some((genericParam, index)) => {
            // This unsolved rune better be one we didn't explicitly hand in already.
            vassert(index >= explicitTemplateArgs.size)

            genericParam.default match {
              case Some(defaultRules) => {
                solver.addRules(defaultRules.rules)
                true
              }
              case None => {
                // There are no defaults for this.
                false
              }
            }
          }
        }
      }) match {
      case Err(f@FailedCompilerSolve(_, _, _)) => {
        return (ResolveFunctionFailure(ResolvingSolveFailedOrIncomplete(f)))
      }
      case Ok(true) =>
      case Ok(false) => // Incomplete, will be detected as IncompleteCompilerSolve below.
    }

    val CompleteResolveSolve(inferredTemplatas, instantiationBoundParams) =
      inferCompiler.checkResolvingConclusionsAndResolve(
        envs, coutputs, invocationRange, callLocation, runeToType, rules, includeReachableBoundsForRunes, solver) match {
        case Err(e) => return (ResolveFunctionFailure(e))
        case Ok(i) => (i)
      }

    val runedEnv =
      addRunedDataToNearEnvForResolving(
        outerEnv,
        function.genericParameters.map(_.rune.rune),
        inferredTemplatas,
        instantiationBoundParams.runeToCitizenRuneToReachablePrototype.values
            .flatMap(_.citizenRuneToReachablePrototype.values).toVector.map(PrototypeTemplataT(_)))

    val prototype =
      middleLayer.getGenericFunctionPrototypeFromCall(
        runedEnv, coutputs, callRange, function)

    coutputs.addInstantiationBounds(
      opts.globalOptions.sanityCheck,
      interner, callingEnv.rootCompilingDenizenEnv.denizenTemplateId,
      prototype.id, instantiationBoundParams)

    ResolveFunctionSuccess(PrototypeTemplataT(prototype), inferredTemplatas)
  }

  def evaluateGenericVirtualDispatcherFunctionForPrototype(
    // The environment the function was defined in.
    nearEnv: BuildingFunctionEnvironmentWithClosuredsT,
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    args: Vector[Option[CoordT]]):
  IDefineFunctionResult = {
    val function = nearEnv.function
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)

    val functionDefinitionRules =
      function.rules.filter(InferCompiler.includeRuleInDefinitionSolve)

    val initialSends = assembleInitialSendsFromArgs(callRange.head, function, args)

    // This is so that we can feed in the self interface to see what it indirectly determines.
    // It will turn a:
    //   func map<T, F>(self Opt<T>, f F, t T) { ... }
    // into a:
    //   func map<F>(self Opt<$0>, f F, t $0) { ... }
    val preliminaryEnvs = InferEnv(callingEnv, callRange, callLocation, nearEnv, RegionT())
    val preliminarySolver =
      inferCompiler.makeSolver(
        preliminaryEnvs,
        coutputs,
        functionDefinitionRules,
        function.runeToType,
        function.range :: callRange,
        Vector(),
        initialSends)
    inferCompiler.continue(preliminaryEnvs, coutputs, preliminarySolver) match {
      case Ok(()) =>
      case Err(f) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(function.range :: callRange, f))
      }
    }

    // Skip checking that the conclusions are all there, because we don't assume that they will all be there. We expect
    // an incomplete solve.
    val preliminaryInferences = preliminarySolver.userifyConclusions().toMap
    // Now we can use preliminaryInferences to know whether or not we need a placeholder for an
    // identifying rune.
    // Our
    //   func map<F>(self Opt<$0>, f F) { ... }
    // will need one placeholder, for F.

    val placeholderInitialKnownsFromFunction =
      function.genericParameters.zipWithIndex.flatMap({ case (genericParam, index) =>
        preliminaryInferences.get(genericParam.rune.rune) match {
          case Some(x) => Some(InitialKnown(genericParam.rune, x))
          case None => {
            // Make a placeholder for every argument even if it has a default, see DUDEWCD.
//            val runeType = vassertSome(function.runeToType.get(genericParam.rune.rune))
            vimpl()
            val placeholderPureHeight = vregionmut(None)
            val templata =
              templataCompiler.createPlaceholder(
                coutputs, callingEnv, callingEnv.id, genericParam, index, function.runeToType, placeholderPureHeight, false)
            Some(InitialKnown(genericParam.rune, templata))
          }
        }
      })

    // Now that we have placeholders, let's do the rest of the solve, so we can get a full
    // prototype out of it.

    val (inferences, instantiationBoundParams) =
      inferCompiler.solveForDefiningStart(
        InferEnv(callingEnv, callRange, callLocation, nearEnv, RegionT()),
        coutputs,
        functionDefinitionRules,
        function.runeToType,
        function.range :: callRange,
        callLocation,
        placeholderInitialKnownsFromFunction,
        Vector(),
        function.params.flatMap(_.pattern.coordRune.map(_.rune)) ++ function.maybeRetCoordRune.map(_.rune)) match {
        case Err(f) => throw CompileErrorExceptionT(TypingPassDefiningError(function.range :: callRange, f))
        case Ok(c) => c
      }
    // DO NOT SUBMIT consider using the env from the above solveForDefining
    val runedEnv =
      addRunedDataToNearEnvForDefining(
        coutputs,
        nearEnv,
        function.genericParameters.map(_.rune.rune),
        inferences,
        instantiationBoundParams)

    inferCompiler.solveForDefiningEnd(
      coutputs,
      functionDefinitionRules,
      instantiationBoundParams,
      runedEnv
    ) match {
      case Err(e) => throw CompileErrorExceptionT(TypingPassDefiningError(callRange, e))
      case Ok(i) => (i)
    }

    val prototype =
      middleLayer.getGenericFunctionPrototypeFromCall(
        runedEnv, coutputs, callRange, function)

    // Usually when we call a function, we add instantiation bounds. However, we're
    // not calling a function here, we're defining it.
    DefineFunctionSuccess(PrototypeTemplataT(prototype), inferences, instantiationBoundParams)
  }

  def precompileGenericFunction(
      coutputs: CompilerOutputs,
      nearEnv: BuildingFunctionEnvironmentWithClosuredsT,
      parentRanges: List[RangeS],
      callLocation: LocationInDenizen
  ): Unit = {
    val function = nearEnv.function
    val range = function.range :: parentRanges
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)

    val functionTemplateId =
      nearEnv.parentEnv.id.addStep(
        nameTranslator.translateGenericFunctionName(nearEnv.function.name))

    val definitionRules = function.rules.filter(InferCompiler.includeRuleInDefinitionSolve)

    // This is so we can automatically grab the bounds from parameters and returns, see NBIFP.
    val paramAndReturnRunes =
      (function.params.flatMap(_.pattern.coordRune.map(_.rune)) ++ function.maybeRetCoordRune.map(_.rune)).distinct.toVector

    val envs = InferEnv(nearEnv, parentRanges, callLocation, nearEnv, RegionT())
    val solver =
      inferCompiler.makeSolver(
        envs, coutputs, definitionRules, function.runeToType, range, Vector(), Vector())
    // Incrementally solve and add placeholders, see IRAGP.
    inferCompiler.incrementallySolve(
      envs, coutputs, solver,
      // Each step happens after the solver has done all it possibly can. Sometimes this can lead
      // to races, see RRBFS.
      (solver) => {
        TemplataCompiler.getFirstUnsolvedIdentifyingRune(function.genericParameters, (rune) => solver.getConclusion(rune).nonEmpty) match {
          case None => false
          case Some((genericParam, index)) => {
            // Make a placeholder for every argument even if it has a default, see DUDEWCD.
            val placeholderPureHeight = vregionmut(None)
            val templata =
              templataCompiler.createPlaceholder(
                coutputs, nearEnv, functionTemplateId, genericParam, index, function.runeToType, placeholderPureHeight, true)
            solver.manualStep(Map(genericParam.rune.rune -> templata))
            true
          }
        }
      }) match {
      case Err(f@FailedCompilerSolve(_, _, err)) => {
        throw CompileErrorExceptionT(typing.TypingPassSolverError(function.range :: parentRanges, f))
      }
      case Ok(true) =>
      case Ok(false) => // DO NOT SUBMIT i think we can handle it uphere instead, could simplify interpretResults // Incomplete, will be detected in the below checkDefiningConclusionsAndResolve
    }
    val inferences =
      inferCompiler.interpretResults(function.runeToType, solver) match {
        case Err(e) => throw CompileErrorExceptionT(typing.TypingPassSolverError(function.range :: parentRanges, e))
        case Ok(conclusions) => conclusions
      }

    // We declare the function inferences so that we don't have to do the above solve twice, which would be:
    // - Slow, and
    // - Problematic because when we're incrementally adding placeholders, we register them with the coutputs, and the
    //   coutputs doesn't like when we register the same thing twice.
    coutputs.declareFunctionInferences(functionTemplateId, inferences) // DO NOT SUBMIT

    // We don't add these here because we aren't instantiating anything here, we're compiling a
    // function
    // not calling it.
    // coutputs.addInstantiationBounds(header.toPrototype.id, runeToFunctionBound)

    // DO NOT SUBMIT unify with middleLayer.evaluateFunctionParamTypes
    val paramTypes =
      function.params.map(param1 => {
        expectCoordTemplata(inferences(param1.pattern.coordRune.get.rune)).coord
      })
    // val templateArgs = function.genericParameters.map(_.rune.rune).map(inferences)
    // // DO NOT SUBMIT unify with middleLayer.assemblename
    // val functionId =
    //   functionTemplateId.copy(
    //     localName = functionTemplateId.localName.makeFunctionName(interner, keywords, templateArgs, paramTypes))

    // Add it to the overload index
    TemplatasStore.getImpreciseName(interner, nearEnv.id.localName) match {
      case None => {
        // DO NOT SUBMIT
        println("Skipping adding function " + nearEnv.id.localName + " to overload index")
      }
      case Some(impreciseName) => {
        coutputs.addOverload(
          opts.globalOptions.useOverloadIndex,
          impreciseName,
          paramTypes.map({
            case CoordT(_, _, KindPlaceholderT(_)) => None // DO NOT SUBMIT document
            case other => Some(other)
          }),
          FunctionCalleeCandidate(nearEnv.templata))
      }
    }

  }

  def compileGenericFunction(
    coutputs: CompilerOutputs,
    nearEnv: BuildingFunctionEnvironmentWithClosuredsT,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen
  ): FunctionHeaderT = {
    val function = nearEnv.function
    val range = function.range :: parentRanges
    // Check preconditions
    checkClosureConcernsHandled(nearEnv)

    val functionTemplateId =
      nearEnv.parentEnv.id.addStep(
        nameTranslator.translateGenericFunctionName(nearEnv.function.name))

    val definitionRules = function.rules.filter(InferCompiler.includeRuleInDefinitionSolve)

    // This is so we can automatically grab the bounds from parameters and returns, see NBIFP.
    val paramAndReturnRunes =
      (function.params.flatMap(_.pattern.coordRune.map(_.rune)) ++ function.maybeRetCoordRune.map(_.rune)).distinct.toVector
    val envs = InferEnv(nearEnv, parentRanges, callLocation, nearEnv, RegionT())
    val inferences = coutputs.getInferencesForFunction(functionTemplateId)
    val instantiationBoundParams =
      inferCompiler.checkDefiningConclusionsAndResolveStart(
        envs, coutputs, definitionRules, paramAndReturnRunes, inferences)
    // checkDefiningConclusionsAndResolve added bounds to the overload index. DO NOT SUBMIT rename that func maybe
    // We add these so that when we try to call a bound from inside the function, they'll be in the
    // overload index and can be found.

    // DO NOT SUBMIT consider using the env from the above checkDefiningConclusionsAndResolve
    val runedEnv =
      addRunedDataToNearEnvForDefining(
        coutputs,
        nearEnv, function.genericParameters.map(_.rune.rune), inferences,
        instantiationBoundParams)

    inferCompiler.checkDefiningConclusionsAndResolveEnd(coutputs, definitionRules, instantiationBoundParams, runedEnv) match {
      case Err(f) => throw CompileErrorExceptionT(TypingPassDefiningError(range, DefiningResolveConclusionError(f)))
      case Ok(c) => c
    }

    val header =
      middleLayer.getOrEvaluateFunctionForHeader(
        nearEnv, runedEnv, coutputs, parentRanges, callLocation, function, instantiationBoundParams)

    // We don't add these here because we aren't instantiating anything here, we're compiling a
    // function
    // not calling it.
    // coutputs.addInstantiationBounds(header.toPrototype.id, runeToFunctionBound)

    header
  }

  private def assembleInitialSendsFromArgs(callRange: RangeS, function: FunctionA, args: Vector[Option[CoordT]]):
  Vector[InitialSend] = {
    function.params.map(_.pattern.coordRune.get).zip(args).zipWithIndex
      .flatMap({
        case ((_, None), _) => None
        case ((paramRune, Some(argTemplata)), argIndex) => {
          Some(InitialSend(RuneUsage(callRange, ArgumentRuneS(argIndex)), paramRune, CoordTemplataT(argTemplata)))
        }
      })
  }
}
