package dev.vale.typing

import dev.vale.parsing.ast.MutableP
import dev.vale.postparsing.{TemplateTemplataType, _}
import dev.vale.postparsing.rules.{IRulexSR, RuneParentEnvLookupSR, RuneUsage}
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.function.DestructorCompiler
import dev.vale.typing.types._
import dev.vale.{CodeLocationS, Err, Interner, Keywords, Ok, PackageCoordinate, Profiler, RangeS, Result, StrI, vassert, vassertOne, vassertSome, vcurious, vimpl, vregion, vregionmut, vwat}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import OverloadResolver.{FindFunctionFailure, InferFailure}
import dev.vale.highertyping.HigherTypingPass.explicifyLookups
import dev.vale.typing.ast.{DestroyImmRuntimeSizedArrayTE, DestroyStaticSizedArrayIntoFunctionTE, FunctionCallTE, NewImmRuntimeSizedArrayTE, ReferenceExpressionTE, RuntimeSizedArrayLookupTE, StaticArrayFromCallableTE, StaticArrayFromValuesTE, StaticSizedArrayLookupTE}
import dev.vale.typing.env.{CitizenEnvironment, FunctionEnvironmentBox, GlobalEnvironment, IEnvironment, IInDenizenEnvironment, NodeEnvironmentBox, NodeEnvironmentT, PackageEnvironment, TemplataEnvEntry, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.StructCompilerCore
import dev.vale.typing.function.FunctionCompiler.{EvaluateFunctionFailure, EvaluateFunctionSuccess, StampFunctionSuccess}
import dev.vale.typing.types._
import dev.vale.typing.templata._

import scala.collection.immutable.{List, Set}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ArrayCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    inferCompiler: InferCompiler,
    overloadResolver: OverloadResolver,
    destructorCompiler: DestructorCompiler,
    templataCompiler: TemplataCompiler) {

  val runeTypeSolver = new RuneTypeSolver(interner)

  vassert(overloadResolver != null)

  def evaluateStaticSizedArrayFromCallable(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironment,
    defaultRegionRune: IRuneS,
    region: ITemplataT[RegionTemplataType],
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    rulesWithImplicitlyCoercingLookupsS: Vector[IRulexSR],
    maybeElementTypeRuneA: Option[IRuneS],
    sizeRuneA: IRuneS,
    mutabilityRune: IRuneS,
    variabilityRune: IRuneS,
    callableTE: ReferenceExpressionTE,
    verifyConclusions: Boolean):
  StaticArrayFromCallableTE = {
//    val builtinNamespaceCoord =
//      interner.intern(PackageCoordinate(keywords.emptyString, Vector.empty))
//    val declaringEnv =
//      PackageEnvironment.makeTopLevelEnvironment(callingEnv.globalEnv, builtinNamespaceCoord)

    val runeTypingEnv =
      new IRuneTypeSolverEnv {
        override def lookup(range: RangeS, nameS: IImpreciseNameS):
        Result[IRuneTypeSolverLookupResult, IRuneTypingLookupFailedError] = {
          vimpl()
//          vassertOne(callingEnv.lookupNearestWithImpreciseName(nameS, Set(TemplataLookupContext))).tyype
        }
      }

    val runeAToTypeWithImplicitlyCoercingLookupsS =
      runeTypeSolver.solve(
        opts.globalOptions.sanityCheck,
        opts.globalOptions.useOptimizedSolver,
        runeTypingEnv,
        parentRanges,
        false,
        rulesWithImplicitlyCoercingLookupsS,
        List(),
        true,
        Map()) match {
        case Ok(r) => r
        case Err(e) => throw CompileErrorExceptionT(HigherTypingInferError(parentRanges, e))
      }

    val runeAToType =
      mutable.HashMap[IRuneS, ITemplataType]((runeAToTypeWithImplicitlyCoercingLookupsS.toSeq): _*)
    // We've now calculated all the types of all the runes, but the LookupSR rules are still a bit
    // loose. We intentionally ignored the types of the things they're looking up, so we could know
    // what types we *expect* them to be, so we could coerce.
    // That coercion is good, but lets make it more explicit.
    val ruleBuilder = ArrayBuffer[IRulexSR]()
    explicifyLookups(runeTypingEnv, runeAToType, ruleBuilder, rulesWithImplicitlyCoercingLookupsS) match {
      case Err(RuneTypingTooManyMatchingTypes(range, name)) => throw CompileErrorExceptionT(TooManyTypesWithNameT(range :: parentRanges, name))
      case Err(RuneTypingCouldntFindType(range, name)) => throw CompileErrorExceptionT(CouldntFindTypeT(range :: parentRanges, name))
      case Ok(()) =>
    }
    val rulesA = ruleBuilder.toVector

    vimpl() // update to also not use solveExpectComplete
    val CompleteCompilerSolve(_, templatas, _, Vector()) =
      inferCompiler.solveExpectComplete(
        InferEnv(callingEnv, parentRanges, callLocation, callingEnv, region),
        coutputs,
        rulesA,
        runeAToType.toMap,
        parentRanges,
        callLocation,
        Vector(),
        Vector(),
        true,
        true,
        Vector())

    val size = ITemplataT.expectInteger(vassertSome(templatas.get(sizeRuneA)))
    val mutability = ITemplataT.expectMutability(vassertSome(templatas.get(mutabilityRune)))
    val variability = ITemplataT.expectVariability(vassertSome(templatas.get(variabilityRune)))
    val prototype =
      overloadResolver.getArrayGeneratorPrototype(
        coutputs, callingEnv, parentRanges, callLocation, callableTE, region, true)
    val ssaMT = resolveStaticSizedArray(mutability, variability, size, prototype.returnType, region)

    maybeElementTypeRuneA.foreach(elementTypeRuneA => {
      val expectedElementType = getArrayElementType(templatas, elementTypeRuneA)
      if (prototype.returnType != expectedElementType) {
        throw CompileErrorExceptionT(UnexpectedArrayElementType(parentRanges, expectedElementType, prototype.returnType))
      }
    })

    val expr2 = ast.StaticArrayFromCallableTE(ssaMT, region, callableTE, prototype)
    expr2
  }

  def evaluateRuntimeSizedArrayFromCallable(
    coutputs: CompilerOutputs,
    callingEnv: NodeEnvironmentT,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    defaultRegionRune: IRuneS,
    region: ITemplataT[RegionTemplataType],
    rulesWithImplicitlyCoercingLookupsS: Vector[IRulexSR],
    maybeElementTypeRune: Option[IRuneS],
    mutabilityRune: IRuneS,
    sizeTE: ReferenceExpressionTE,
    maybeCallableTE: Option[ReferenceExpressionTE],
    verifyConclusions: Boolean):
  ReferenceExpressionTE = {

    val runeTypingEnv =
      new IRuneTypeSolverEnv {
        override def lookup(
          range: RangeS,
          nameS: IImpreciseNameS
        ): Result[IRuneTypeSolverLookupResult, IRuneTypingLookupFailedError] = {
          // DO NOT SUBMIT merge with other lookup overrides. maybe make some kind of adapter.
          callingEnv.lookupNearestWithImpreciseName(nameS, Set(TemplataLookupContext)) match {
            case Some(CitizenDefinitionTemplataT(environment, a)) => {
              Ok(CitizenRuneTypeSolverLookupResult(a.tyype, a.genericParameters))
            }
            case Some(x) => Ok(TemplataLookupResult(x.tyype))
            case None => Err(RuneTypingCouldntFindType(range, nameS))
          }
//          name match {
//            case CodeNameS(n) if n == keywords.int => Ok(PrimitiveRuneTypeSolverLookupResult(KindTemplataType()))
//            case other => vwat(other)
//          }
        }
      }

    val runeAToTypeWithImplicitlyCoercingLookupsS =
      runeTypeSolver.solve(
        opts.globalOptions.sanityCheck,
        opts.globalOptions.useOptimizedSolver,
        runeTypingEnv,
        parentRanges,
        false,
        rulesWithImplicitlyCoercingLookupsS,
        List(),
        true,
        Map(mutabilityRune -> MutabilityTemplataType(), defaultRegionRune -> RegionTemplataType()) ++
          maybeElementTypeRune.map(_ -> CoordTemplataType())) match {
        case Ok(r) => r
        case Err(e) => throw CompileErrorExceptionT(HigherTypingInferError(parentRanges, e))
      }

    val runeAToType =
      mutable.HashMap[IRuneS, ITemplataType]((runeAToTypeWithImplicitlyCoercingLookupsS.toSeq): _*)
    // We've now calculated all the types of all the runes, but the LookupSR rules are still a bit
    // loose. We intentionally ignored the types of the things they're looking up, so we could know
    // what types we *expect* them to be, so we could coerce.
    // That coercion is good, but lets make it more explicit.
    val ruleBuilder = ArrayBuffer[IRulexSR]()
    explicifyLookups(runeTypingEnv, runeAToType, ruleBuilder, rulesWithImplicitlyCoercingLookupsS) match {
      case Err(RuneTypingTooManyMatchingTypes(range, name)) => throw CompileErrorExceptionT(TooManyTypesWithNameT(range :: parentRanges, name))
      case Err(RuneTypingCouldntFindType(range, name)) => throw CompileErrorExceptionT(CouldntFindTypeT(range :: parentRanges, name))
      case Ok(()) =>
    }
    val rulesA = ruleBuilder.toVector

    // Elsewhere we do some incremental solving to fill in default generic param values like the
    // context region, but here I think we can just feed it in directly. There's syntactically no
    // way for the user to hand it in as a generic param.
    val initialKnowns = Vector(InitialKnown(RuneUsage(parentRanges.head, defaultRegionRune), region))

//    val CompleteCompilerSolve(_, templatas, _, Vector()) =
//      inferCompiler.solveExpectComplete(
//        InferEnv(callingEnv, parentRanges, callLocation, callingEnv, region),
//        coutputs, rulesA, runeAToType.toMap, parentRanges,
//        callLocation, initialKnowns, Vector(), true, true, Vector())
    val rules = rulesA
    val runeToType = runeAToType.toMap
    val invocationRange = parentRanges
    val initialSends = Vector()

    val envs = InferEnv(callingEnv, parentRanges, callLocation,callingEnv, region)
    val solver =
      inferCompiler.makeSolver(
        envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends)

    // Incrementally solve and add default generic parameters (and context region).
    inferCompiler.incrementallySolve(
      envs, coutputs, solver,
      (solver) => {
        if (solver.getConclusion(defaultRegionRune).isEmpty) {
          solver.manualStep(Map(defaultRegionRune -> region))
          true
        } else {
          false
        }
      }) match {
      case Err(f @ FailedCompilerSolve(_, _, err)) => {
        throw CompileErrorExceptionT(TypingPassSolverError(invocationRange, f))
      }
      case Ok(true) =>
      case Ok(false) => // Incomplete, will be detected as IncompleteCompilerSolve below.
    }

    val CompleteCompilerSolve(_, templatas, runeToFunctionBound, reachableBounds) =
      (inferCompiler.interpretResults(envs, coutputs, invocationRange, callLocation, runeToType, rules, verifyConclusions, true, Vector(), solver) match {
        case f @ FailedCompilerSolve(_, _, _) => Err(f)
        case i @ IncompleteCompilerSolve(_, _, _, _) => Err(i)
        case c @ CompleteCompilerSolve(_, _, _, _) => Ok(c)
      }) match {
        case Err(e) => throw CompileErrorExceptionT(TypingPassSolverError(invocationRange, e))
        case Ok(i) => (i)
      }

    val mutability = ITemplataT.expectMutability(vassertSome(templatas.get(mutabilityRune)))

//    val variability = getArrayVariability(templatas, variabilityRune)

    if (maybeElementTypeRune.isEmpty) {
      // Temporary until we can figure out MSAE.
      throw CompileErrorExceptionT(RangedInternalErrorT(parentRanges, "Must specify element for arrays."))
    }

    mutability match {
      case PlaceholderTemplataT(_, MutabilityTemplataType()) => vimpl()
      case MutabilityTemplataT(ImmutableT) => {
        val callableTE =
          maybeCallableTE match {
            case None => {
              throw CompileErrorExceptionT(NewImmRSANeedsCallable(parentRanges))
            }
            case Some(c) => c
          }

        val prototype =
          overloadResolver.getArrayGeneratorPrototype(
            coutputs, callingEnv, parentRanges, callLocation, callableTE, region, true)
        val rsaMT = resolveRuntimeSizedArray(prototype.returnType, mutability, region)

        maybeElementTypeRune.foreach(elementTypeRuneA => {
          val expectedElementType = getArrayElementType(templatas, elementTypeRuneA)
          if (prototype.returnType != expectedElementType) {
            throw CompileErrorExceptionT(UnexpectedArrayElementType(parentRanges, expectedElementType, prototype.returnType))
          }
        })

        NewImmRuntimeSizedArrayTE(rsaMT, region, sizeTE, callableTE, prototype)
      }
      case MutabilityTemplataT(MutableT) => {
        val StampFunctionSuccess(maybeNewRegion, prototype, conclusions) =
          overloadResolver.findFunction(
            callingEnv
              .addEntries(
                interner,
                Vector(
                  (interner.intern(RuneNameT(CodeRuneS(keywords.M))), TemplataEnvEntry(MutabilityTemplataT(MutableT)))) ++
              maybeElementTypeRune.map(e => {
                (interner.intern(RuneNameT(e)), TemplataEnvEntry(CoordTemplataT(getArrayElementType(templatas, e))))
              })),
            coutputs,
            parentRanges,
            callLocation,
            interner.intern(CodeNameS(keywords.Array)),
            Vector(
              RuneParentEnvLookupSR(parentRanges.head, RuneUsage(parentRanges.head, CodeRuneS(keywords.M)))) ++
            maybeElementTypeRune.map(e => {
              RuneParentEnvLookupSR(parentRanges.head, RuneUsage(parentRanges.head, e))
            }),
            Vector(CodeRuneS(keywords.M)) ++ maybeElementTypeRune,
            region,
            Vector(sizeTE.result.coord) ++
              maybeCallableTE.map(c => c.result.coord),
            Vector(),
            true,
            true) match {
            case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(parentRanges, e))
            case Ok(x) => x
          }
        vcurious(maybeNewRegion.isEmpty)

        val elementType =
          prototype.prototype.returnType.kind match {
            case RuntimeSizedArrayTT(IdT(_, _, RuntimeSizedArrayNameT(_, RawArrayNameT(mutability, elementType, _)))) => {
              if (mutability != MutabilityTemplataT(MutableT)) {
                throw CompileErrorExceptionT(RangedInternalErrorT(parentRanges, "Array function returned wrong mutability!"))
              }
              elementType
            }
            case _ => {
              throw CompileErrorExceptionT(RangedInternalErrorT(parentRanges, "Array function returned wrong type!"))
            }
          }
        maybeElementTypeRune.foreach(elementTypeRuneA => {
          val expectedElementType = getArrayElementType(templatas, elementTypeRuneA)
          if (elementType != expectedElementType) {
            throw CompileErrorExceptionT(
              UnexpectedArrayElementType(parentRanges, expectedElementType, prototype.prototype.returnType))
          }
        })
        vassert(coutputs.getInstantiationBounds(prototype.prototype.id).nonEmpty)
        val callTE =
          FunctionCallTE(prototype.prototype, Vector(sizeTE) ++ maybeCallableTE)
        callTE
        //        throw CompileErrorExceptionT(RangedInternalErrorT(range, "Can't construct a mutable runtime array from a callable!"))
      }
    }
  }

  def evaluateStaticSizedArrayFromValues(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironment,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    rulesWithImplicitlyCoercingLookupsS: Vector[IRulexSR],
    maybeElementTypeRuneA: Option[IRuneS],
    sizeRuneA: IRuneS,
    mutabilityRuneA: IRuneS,
    variabilityRuneA: IRuneS,
    exprs2: Vector[ReferenceExpressionTE],
    defaultRegionRune: IRuneS,
    region: ITemplataT[RegionTemplataType],
    verifyConclusions: Boolean):
  StaticArrayFromValuesTE = {

    val runeTypingEnv =
      new IRuneTypeSolverEnv {
        override def lookup(
          range: RangeS,
          name: IImpreciseNameS
        ): Result[IRuneTypeSolverLookupResult, IRuneTypingLookupFailedError] = {
          vimpl()
//          Ok(
//            TemplataLookupResult(
//              vassertSome(callingEnv.lookupNearestWithImpreciseName(name, Set(TemplataLookupContext))).tyype))
        }
      }

    val runeAToTypeWithImplicitlyCoercingLookupsS =
      runeTypeSolver.solve(
        opts.globalOptions.sanityCheck,
        opts.globalOptions.useOptimizedSolver,
        runeTypingEnv,
        parentRanges,
        false,
        rulesWithImplicitlyCoercingLookupsS,
        List(),
        true,
        Map[IRuneS, ITemplataType](
          sizeRuneA -> IntegerTemplataType(),
          mutabilityRuneA -> MutabilityTemplataType(),
          variabilityRuneA -> VariabilityTemplataType(),
          defaultRegionRune -> RegionTemplataType()) ++
          (maybeElementTypeRuneA match {
            case Some(rune) => Map(rune -> CoordTemplataType())
            case None => Map()
          })) match {
        case Ok(r) => r
        case Err(e) => throw CompileErrorExceptionT(HigherTypingInferError(parentRanges, e))
      }
    val memberTypes = exprs2.map(_.result.coord).toSet
    if (memberTypes.size > 1) {
      throw CompileErrorExceptionT(ArrayElementsHaveDifferentTypes(parentRanges, memberTypes))
    }
    val memberType = memberTypes.head

    val runeAToType =
      mutable.HashMap[IRuneS, ITemplataType]((runeAToTypeWithImplicitlyCoercingLookupsS.toSeq): _*)
    // We've now calculated all the types of all the runes, but the LookupSR rules are still a bit
    // loose. We intentionally ignored the types of the things they're looking up, so we could know
    // what types we *expect* them to be, so we could coerce.
    // That coercion is good, but lets make it more explicit.
    val ruleBuilder = ArrayBuffer[IRulexSR]()
    explicifyLookups(runeTypingEnv, runeAToType, ruleBuilder, rulesWithImplicitlyCoercingLookupsS) match {
      case Err(RuneTypingTooManyMatchingTypes(range, name)) => throw CompileErrorExceptionT(TooManyTypesWithNameT(range :: parentRanges, name))
      case Err(RuneTypingCouldntFindType(range, name)) => throw CompileErrorExceptionT(CouldntFindTypeT(range :: parentRanges, name))
      case Ok(()) =>
    }
    val rulesA = ruleBuilder.toVector

    val initialKnowns =
      Vector(
        InitialKnown(
          RuneUsage(vassertSome(parentRanges.headOption), defaultRegionRune),
          region))

//    val CompleteCompilerSolve(_, templatas, _, Vector()) =
//      inferCompiler.solveExpectComplete(
//        envs,
//        coutputs, rulesA, runeAToType.toMap, parentRanges,
//        callLocation, initialKnowns, Vector(), true, true, Vector())
    val rules = rulesA
    val runeToType = runeAToType.toMap
    val invocationRange = parentRanges
    val initialSends = Vector()

    val envs = InferEnv(callingEnv, parentRanges, callLocation,callingEnv, region)
    val solver =
      inferCompiler.makeSolver(
        envs, coutputs, rules, runeToType, invocationRange, initialKnowns, initialSends)

    // Incrementally solve and add default generic parameters (and context region).
    inferCompiler.incrementallySolve(
      envs, coutputs, solver,
      (solver) => {
        if (solver.getConclusion(defaultRegionRune).isEmpty) {
          solver.manualStep(Map(defaultRegionRune -> region))
          true
        } else {
          false
        }
      }) match {
      case Err(f @ FailedCompilerSolve(_, _, err)) => {
        throw CompileErrorExceptionT(TypingPassSolverError(invocationRange, f))
      }
      case Ok(true) =>
      case Ok(false) => // Incomplete, will be detected as IncompleteCompilerSolve below.
    }

    val CompleteCompilerSolve(_, templatas, runeToFunctionBound, reachableBounds) =
      (inferCompiler.interpretResults(envs, coutputs, invocationRange, callLocation, runeToType, rules, verifyConclusions, true, Vector(), solver) match {
        case f @ FailedCompilerSolve(_, _, _) => Err(f)
        case i @ IncompleteCompilerSolve(_, _, _, _) => Err(i)
        case c @ CompleteCompilerSolve(_, _, _, _) => Ok(c)
      }) match {
        case Err(e) => throw CompileErrorExceptionT(TypingPassSolverError(invocationRange, e))
        case Ok(i) => (i)
      }


    maybeElementTypeRuneA.foreach(elementTypeRuneA => {
      val expectedElementType = getArrayElementType(templatas, elementTypeRuneA)
      if (memberType != expectedElementType) {
        throw CompileErrorExceptionT(UnexpectedArrayElementType(parentRanges, expectedElementType, memberType))
      }
    })

//    val size = getArraySize(templatas, sizeRuneA)
    val mutability = ITemplataT.expectMutability(vassertSome(templatas.get(mutabilityRuneA)))
    val variability = ITemplataT.expectVariability(vassertSome(templatas.get(variabilityRuneA)))

    val staticSizedArrayType = resolveStaticSizedArray(mutability, variability, IntegerTemplataT(exprs2.size), memberType, region)
    val ownership =
      staticSizedArrayType.mutability match {
        case MutabilityTemplataT(MutableT) => OwnT
        case MutabilityTemplataT(ImmutableT) => ShareT
        case PlaceholderTemplataT(_, MutabilityTemplataType()) => OwnT
      }

    val ssaCoord = CoordT(ownership, region, staticSizedArrayType)

    val finalExpr =
      StaticArrayFromValuesTE(
        exprs2, ssaCoord, staticSizedArrayType)
    (finalExpr)
  }

  def evaluateDestroyStaticSizedArrayIntoCallable(
    coutputs: CompilerOutputs,
    fate: FunctionEnvironmentBox,
    range: List[RangeS],
    callLocation: LocationInDenizen,
    arrTE: ReferenceExpressionTE,
    callableTE: ReferenceExpressionTE,
    contextRegion: ITemplataT[RegionTemplataType]):
  DestroyStaticSizedArrayIntoFunctionTE = {
    val arrayTT =
      arrTE.result.coord match {
        case CoordT(_, region, s @ contentsStaticSizedArrayTT(_, _, _, _, _)) => s
        case other => {
          throw CompileErrorExceptionT(RangedInternalErrorT(range, "Destroying a non-array with a callable! Destroying: " + other))
        }
      }

    val prototype =
      overloadResolver.getArrayConsumerPrototype(
        coutputs, fate, range, callLocation, callableTE, arrayTT.elementType, contextRegion, true)

    ast.DestroyStaticSizedArrayIntoFunctionTE(
      arrTE,
      arrayTT,
      callableTE,
      prototype)
  }

  def evaluateDestroyRuntimeSizedArrayIntoCallable(
    coutputs: CompilerOutputs,
    fate: FunctionEnvironmentBox,
    range: List[RangeS],
    callLocation: LocationInDenizen,
    arrTE: ReferenceExpressionTE,
    callableTE: ReferenceExpressionTE,
    contextRegion: ITemplataT[RegionTemplataType]):
  DestroyImmRuntimeSizedArrayTE = {
    val arrayTT =
      arrTE.result.coord match {
        case CoordT(_, region, s @ contentsRuntimeSizedArrayTT(_, _, _)) => s
        case other => {
          throw CompileErrorExceptionT(RangedInternalErrorT(range, "Destroying a non-array with a callable! Destroying: " + other))
        }
      }

    // assert that region is mutable
    vimpl()

    arrayTT.mutability match {
      case PlaceholderTemplataT(_, MutabilityTemplataType()) => {
        throw CompileErrorExceptionT(RangedInternalErrorT(range, "Can't destroy an array whose mutability we don't know!"))
      }
      case MutabilityTemplataT(ImmutableT) =>
      case MutabilityTemplataT(MutableT) => {
        throw CompileErrorExceptionT(RangedInternalErrorT(range, "Can't destroy a mutable array with a callable!"))
      }
    }

    val prototype =
      overloadResolver.getArrayConsumerPrototype(
        coutputs, fate, range, callLocation, callableTE, arrayTT.elementType, contextRegion, true)

//    val freePrototype =
//      destructorCompiler.getFreeFunction(
//        coutputs, fate, range, arrTE.result.reference)
//        .function.prototype
//    vassert(coutputs.getInstantiationBounds(freePrototype.fullName).nonEmpty)

    ast.DestroyImmRuntimeSizedArrayTE(
      arrTE,
      arrayTT,
      callableTE,
      prototype)
  }

  def compileStaticSizedArray(
    globalEnv: GlobalEnvironment,
    coutputs: CompilerOutputs):
  Unit = {
    val builtinPackage = PackageCoordinate.BUILTIN(interner, keywords)
    val templateFullName =
      IdT(builtinPackage, Vector.empty, interner.intern(StaticSizedArrayTemplateNameT()))

    // We declare the function into the environment that we use to compile the
    // struct, so that those who use the struct can reach into its environment
    // and see the function and use it.
    // See CSFMSEO and SAFHE.
    val arrayOuterEnv =
      CitizenEnvironment(
        globalEnv,
        PackageEnvironment(
          globalEnv, templateFullName, globalEnv.nameToTopLevelEnvironment.values.toVector),
        templateFullName,
        templateFullName,
        TemplatasStore(templateFullName, Map(), Map()))
    coutputs.declareType(templateFullName)
    coutputs.declareTypeOuterEnv(templateFullName, arrayOuterEnv)

    val TemplateTemplataType(types, _) = StaticSizedArrayTemplateTemplataT().tyype
    val Vector(IntegerTemplataType(), MutabilityTemplataType(), VariabilityTemplataType(), CoordTemplataType(), RegionTemplataType()) = types
    val sizePlaceholder =
      templataCompiler.createNonKindNonRegionPlaceholderInner(
        templateFullName, 0, CodeRuneS(interner.intern(StrI("N"))), IntegerTemplataType())
    val mutabilityPlaceholder =
      templataCompiler.createNonKindNonRegionPlaceholderInner(
        templateFullName, 1, CodeRuneS(interner.intern(StrI("M"))), MutabilityTemplataType())
    val variabilityPlaceholder =
      templataCompiler.createNonKindNonRegionPlaceholderInner(
        templateFullName, 2, CodeRuneS(interner.intern(StrI("V"))), VariabilityTemplataType())
    val elementPlaceholder =
      templataCompiler.createCoordPlaceholderInner(
        coutputs, arrayOuterEnv, templateFullName, 3, CodeRuneS(interner.intern(StrI("E"))), 0, false, true)
    val regionPlaceholder =
      templataCompiler.createRegionPlaceholderInner(
        templateFullName, 4, CodeRuneS(interner.intern(StrI("Z"))), 0)

    val placeholders =
      Vector(
        sizePlaceholder,
        mutabilityPlaceholder,
        variabilityPlaceholder,
        elementPlaceholder,
        regionPlaceholder)

    val fullName = templateFullName.copy(localName = templateFullName.localName.makeCitizenName(interner, placeholders))
    vassert(TemplataCompiler.getTemplate(fullName) == templateFullName)

    val arrayInnerEnv =
      arrayOuterEnv.copy(
        id = fullName,
        templatas = arrayOuterEnv.templatas.copy(templatasStoreName = fullName))
    coutputs.declareTypeInnerEnv(templateFullName, arrayInnerEnv)
  }

  def resolveStaticSizedArray(
    mutability: ITemplataT[MutabilityTemplataType],
    variability: ITemplataT[VariabilityTemplataType],
    size: ITemplataT[IntegerTemplataType],
    type2: CoordT,
    region: ITemplataT[RegionTemplataType]):
  (StaticSizedArrayTT) = {
    interner.intern(StaticSizedArrayTT(
      IdT(
        PackageCoordinate.BUILTIN(interner, keywords),
        Vector(),
        interner.intern(StaticSizedArrayNameT(
          interner.intern(StaticSizedArrayTemplateNameT()),
          size,
          variability,
          interner.intern(RawArrayNameT(mutability, type2, region)))))))
  }

  def compileRuntimeSizedArray(
    globalEnv: GlobalEnvironment,
    coutputs: CompilerOutputs):
  Unit = {
    val builtinPackage = PackageCoordinate.BUILTIN(interner, keywords)
    val templateFullName =
      IdT(builtinPackage, Vector.empty, interner.intern(RuntimeSizedArrayTemplateNameT()))
//    val defaultRegionName =
//      vimpl()
////      templateFullName.addStep(
////        interner.intern(PlaceholderNameT(
////          interner.intern(PlaceholderTemplateNameT(0, DefaultRegionRuneS())))))
//    val defaultRegion = PlaceholderTemplata(defaultRegionName, RegionTemplataType())

    // We declare the function into the environment that we use to compile the
    // struct, so that those who use the struct can reach into its environment
    // and see the function and use it.
    // See CSFMSEO and SAFHE.
    val arrayOuterEnv =
      CitizenEnvironment(
        globalEnv,
        PackageEnvironment(globalEnv, templateFullName, globalEnv.nameToTopLevelEnvironment.values.toVector),
        templateFullName,
        templateFullName,
        TemplatasStore(templateFullName, Map(), Map()))
    coutputs.declareType(templateFullName)
    coutputs.declareTypeOuterEnv(templateFullName, arrayOuterEnv)



    val TemplateTemplataType(types, _) = RuntimeSizedArrayTemplateTemplataT().tyype
    val Vector(MutabilityTemplataType(), CoordTemplataType(), RegionTemplataType()) = types
    val mutabilityPlaceholder =
      templataCompiler.createNonKindNonRegionPlaceholderInner(
        templateFullName, 0, CodeRuneS(interner.intern(StrI("M"))), MutabilityTemplataType())
    val elementPlaceholder =
      templataCompiler.createCoordPlaceholderInner(
        coutputs, arrayOuterEnv, templateFullName, 1, CodeRuneS(interner.intern(StrI("E"))), 0, false, true)
    val regionPlaceholder =
      templataCompiler.createRegionPlaceholderInner(
        templateFullName, 2, CodeRuneS(interner.intern(StrI("Z"))), 0)
    val placeholders =
      Vector(mutabilityPlaceholder, elementPlaceholder, regionPlaceholder)

    val fullName = templateFullName.copy(localName = templateFullName.localName.makeCitizenName(interner, placeholders))

    val arrayInnerEnv =
      arrayOuterEnv.copy(
        id = fullName,
        templatas = arrayOuterEnv.templatas.copy(templatasStoreName = fullName))
    coutputs.declareTypeInnerEnv(templateFullName, arrayInnerEnv)
  }

  def resolveRuntimeSizedArray(
    type2: CoordT,
    mutability: ITemplataT[MutabilityTemplataType],
    region: ITemplataT[RegionTemplataType]):
  (RuntimeSizedArrayTT) = {
    interner.intern(RuntimeSizedArrayTT(
      IdT(
        PackageCoordinate.BUILTIN(interner, keywords),
        Vector(),
        interner.intern(RuntimeSizedArrayNameT(
          interner.intern(RuntimeSizedArrayTemplateNameT()),
          interner.intern(RawArrayNameT(mutability, type2, region)))))))
  }

  private def getArraySize(templatas: Map[IRuneS, ITemplataT[ITemplataType]], sizeRuneA: IRuneS): Int = {
    val IntegerTemplataT(m) = vassertSome(templatas.get(sizeRuneA))
    m.toInt
  }
  private def getArrayElementType(templatas: Map[IRuneS, ITemplataT[ITemplataType]], typeRuneA: IRuneS): CoordT = {
    val CoordTemplataT(m) = vassertSome(templatas.get(typeRuneA))
    m
  }

  def lookupInStaticSizedArray(
    range: RangeS,
    containerExpr2: ReferenceExpressionTE,
    indexExpr2: ReferenceExpressionTE,
    at: StaticSizedArrayTT):
  StaticSizedArrayLookupTE = {
    val contentsStaticSizedArrayTT(size, mutability, variabilityTemplata, memberType, selfRegion) = at
    val variability =
      variabilityTemplata match {
        case PlaceholderTemplataT(_, _) => FinalT
        case VariabilityTemplataT(variability) => variability
      }
    StaticSizedArrayLookupTE(range, containerExpr2, indexExpr2, memberType,  variability)
  }

  def lookupInUnknownSizedArray(
    parentRanges: List[RangeS],
    range: RangeS,
    containerExpr2: ReferenceExpressionTE,
    indexExpr2: ReferenceExpressionTE,
    rsa: RuntimeSizedArrayTT
  ): RuntimeSizedArrayLookupTE = {
    val contentsRuntimeSizedArrayTT(mutability, memberType, selfRegion) = rsa
    vregionmut(selfRegion)
    if (indexExpr2.result.coord.kind != IntT(32)) {
      throw CompileErrorExceptionT(IndexedArrayWithNonInteger(range :: parentRanges, indexExpr2.result.coord))
    }
    val variability =
      mutability match {
        case PlaceholderTemplataT(_, MutabilityTemplataType()) => FinalT
        case MutabilityTemplataT(ImmutableT) => FinalT
        case MutabilityTemplataT(MutableT) => VaryingT
      }
    RuntimeSizedArrayLookupTE(range, containerExpr2, rsa, indexExpr2, variability)
  }

}
