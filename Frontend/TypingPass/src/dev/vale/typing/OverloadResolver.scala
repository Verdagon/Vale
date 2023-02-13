package dev.vale.typing

import dev.vale.{Accumulator, Err, Interner, Keywords, Ok, Profiler, RangeS, Result, StrI, vassert, vassertSome, vcurious, vfail, vimpl, vpass, vregionmut, vwat}
import dev.vale.postparsing._
import dev.vale.postparsing.rules.{DefinitionFuncSR, IRulexSR, RuneParentEnvLookupSR, RuneUsage}
import dev.vale.solver.IIncompleteOrFailedSolve
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.infer.ITypingPassSolverError
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.postparsing.PostParserErrorHumanizer
import dev.vale.solver.FailedSolve
import OverloadResolver.{Outscored, RuleTypeSolveFailure, SpecificParamDoesntMatchExactly, SpecificParamDoesntSend, SpecificParamRegionDoesntMatch}
import dev.vale.highertyping.HigherTypingPass.explicifyLookups
import dev.vale.parsing.ast.ReadOnlyRegionRuneAttributeP
import dev.vale.typing.ast.{AbstractT, FunctionBannerT, FunctionCalleeCandidate, HeaderCalleeCandidate, ICalleeCandidate, IValidCalleeCandidate, ParameterT, PrototypeT, ReferenceExpressionTE, ValidCalleeCandidate, ValidHeaderCalleeCandidate}
import dev.vale.typing.env.{ExpressionLookupContext, FunctionEnvironmentBox, IDenizenEnvironmentBox, IInDenizenEnvironment, TemplataLookupContext}
import dev.vale.typing.templata._
import dev.vale.typing.ast._
import dev.vale.typing.function.FunctionCompiler.{StampFunctionFailure, StampFunctionSuccess}
import dev.vale.typing.names.{CallEnvNameT, CodeVarNameT, FunctionBoundNameT, FunctionBoundTemplateNameT, FunctionNameT, FunctionTemplateNameT, IdT, RegionPlaceholderNameT}

import scala.collection.immutable.{Map, Set}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
//import dev.vale.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator, RuleTyperSolveFailure, RuleTyperSolveSuccess}
//import dev.vale.postparsing.rules.{EqualsSR, TemplexSR, TypedSR}
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.postparsing.ExplicitTemplateArgRuneS
import OverloadResolver.{IFindFunctionFailureReason, InferFailure, FindFunctionFailure, SpecificParamVirtualityDoesntMatch, WrongNumberOfArguments, WrongNumberOfTemplateArguments}
import dev.vale.typing.env._
import FunctionCompiler.{EvaluateFunctionFailure, EvaluateFunctionSuccess, IEvaluateFunctionResult}
//import dev.vale.typingpass.infer.infer.{InferSolveFailure, InferSolveSuccess}
import dev.vale.Profiler

import scala.collection.immutable.List

object OverloadResolver {

  sealed trait IFindFunctionFailureReason
  case class WrongNumberOfArguments(supplied: Int, expected: Int) extends IFindFunctionFailureReason {
    vpass()

    override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  }
  case class WrongNumberOfTemplateArguments(supplied: Int, expected: Int) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class SpecificParamDoesntSend(index: Int, argument: CoordT, parameter: CoordT) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class SpecificParamDoesntMatchExactly(index: Int, argument: CoordT, parameter: CoordT) extends IFindFunctionFailureReason {
    override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
    vpass()
  }
  case class SpecificParamRegionDoesntMatch(rune: IRuneS, suppliedMutable: Boolean, expectedMutable: Boolean) extends IFindFunctionFailureReason {
    override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
    vpass()
  }
  case class SpecificParamVirtualityDoesntMatch(index: Int) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class Outscored() extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class RuleTypeSolveFailure(reason: RuneTypeSolveError) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class InferFailure(reason: IIncompleteOrFailedCompilerSolve) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }

  case class FindFunctionFailure(
    name: IImpreciseNameS,
    args: Vector[CoordT],
    // All the banners we rejected, and the reason why
    rejectedCalleeToReason: Iterable[(ICalleeCandidate, IFindFunctionFailureReason)]
  ) {
    vpass()
    override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  }

  case class EvaluateFunctionFailure(
    name: IImpreciseNameS,
    args: Vector[CoordT],
    // All the banners we rejected, and the reason why
    rejectedCalleeToReason: Iterable[(IValidCalleeCandidate, IFindFunctionFailureReason)]
  ) {
    vpass()
    override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  }
}

class OverloadResolver(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler,
    functionCompiler: FunctionCompiler) {
  val runeTypeSolver = new RuneTypeSolver(interner)

  def findFunction(
    callingEnv: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    functionName: IImpreciseNameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Vector[IRuneS],
    contextRegion: ITemplata[RegionTemplataType],
    args: Vector[CoordT],
    extraEnvsToLookIn: Vector[IInDenizenEnvironment],
    exact: Boolean,
    verifyConclusions: Boolean):
  Result[StampFunctionSuccess, FindFunctionFailure] = {
    Profiler.frame(() => {
      findPotentialFunction(
        callingEnv,
        coutputs,
        callRange,
        callLocation,
        functionName,
        explicitTemplateArgRulesS,
        explicitTemplateArgRunesS,
        contextRegion,
        args,
        extraEnvsToLookIn,
        exact,
        verifyConclusions) match {
        case Err(f @ FindFunctionFailure(_, _, _)) => Err(f)
        case Ok(potentialBanner) => {
          Ok(
            stampPotentialFunctionForPrototype(
              coutputs, callingEnv, callRange, callLocation, potentialBanner, contextRegion, args, verifyConclusions))
        }
      }
    })
  }

  private def paramsMatch(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironment,
    parentRanges: List[RangeS],
    desiredParams: Vector[CoordT],
    candidateParams: Vector[CoordT],
    exact: Boolean):
  Result[Unit, IFindFunctionFailureReason] = {
    if (desiredParams.size != candidateParams.size) {
      return Err(WrongNumberOfArguments(desiredParams.size, candidateParams.size))
    }
    desiredParams.zip(candidateParams).zipWithIndex.foreach({
      case ((desiredParam, candidateParam), paramIndex) => {
        val desiredTemplata = desiredParam
        val candidateType = candidateParam

        if (exact) {
          if (desiredTemplata != candidateType) {
            return Err(SpecificParamDoesntMatchExactly(paramIndex, desiredTemplata, candidateType))
          }
        } else {
          if (!templataCompiler.isTypeConvertible(coutputs, callingEnv, parentRanges, desiredTemplata, candidateType)) {
            return Err(SpecificParamDoesntSend(paramIndex, desiredTemplata, candidateType))
          }
        }
      }
    })
    // Would have bailed out early if there was a false
    Ok(())
  }

  case class SearchedEnvironment(
    needle: IImpreciseNameS,
    environment: IInDenizenEnvironment,
    matchingTemplatas: Vector[ITemplata[ITemplataType]])

  private def getCandidateBanners(
    env: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    range: List[RangeS],
    functionName: IImpreciseNameS,
    paramFilters: Vector[CoordT],
    extraEnvsToLookIn: Vector[IInDenizenEnvironment],
    searchedEnvs: Accumulator[SearchedEnvironment],
    results: Accumulator[ICalleeCandidate]):
  Unit = {
    getCandidateBannersInner(env, coutputs, range, functionName, searchedEnvs, results)
    getParamEnvironments(coutputs, range, paramFilters)
      .foreach(e => getCandidateBannersInner(e, coutputs, range, functionName, searchedEnvs, results))
    extraEnvsToLookIn
      .foreach(e => getCandidateBannersInner(env, coutputs, range, functionName, searchedEnvs, results))
  }

  private def getCandidateBannersInner(
    env: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    range: List[RangeS],
    functionName: IImpreciseNameS,
    searchedEnvs: Accumulator[SearchedEnvironment],
    results: Accumulator[ICalleeCandidate]):
  Unit = {
    val candidates =
      env.lookupAllWithImpreciseName(functionName, Set(ExpressionLookupContext)).toVector.distinct
    searchedEnvs.add(SearchedEnvironment(functionName, env, candidates))
    candidates.foreach({
      case KindTemplata(OverloadSetT(overloadsEnv, nameInOverloadsEnv)) => {
        getCandidateBannersInner(
          overloadsEnv, coutputs, range, nameInOverloadsEnv, searchedEnvs, results)
      }
      case KindTemplata(sr@StructTT(_)) => {
        val structEnv = coutputs.getOuterEnvForType(range, TemplataCompiler.getStructTemplate(sr.id))
        getCandidateBannersInner(
          structEnv, coutputs, range, interner.intern(CodeNameS(keywords.underscoresCall)), searchedEnvs, results)
      }
      case KindTemplata(sr@InterfaceTT(_)) => {
        val interfaceEnv = coutputs.getOuterEnvForType(range, TemplataCompiler.getInterfaceTemplate(sr.id))
        getCandidateBannersInner(
          interfaceEnv, coutputs, range, interner.intern(CodeNameS(keywords.underscoresCall)), searchedEnvs, results)
      }
      case ExternFunctionTemplata(header) => {
        results.add(HeaderCalleeCandidate(header))
      }
      case PrototypeTemplata(declarationRange, prototype) => {
        vassert(coutputs.getInstantiationBounds(prototype.id).nonEmpty)
        results.add(PrototypeTemplataCalleeCandidate(declarationRange, prototype))
      }
      case ft@FunctionTemplata(_, _) => {
        results.add(FunctionCalleeCandidate(ft))
      }
    })
  }

  private def attemptCandidateBanner(
    callingEnv: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Vector[IRuneS],
    contextRegion: ITemplata[RegionTemplataType],
    args: Vector[CoordT],
    candidate: ICalleeCandidate,
    exact: Boolean,
    verifyConclusions: Boolean):
    //maybeLatestPureBlockLocation: Option[Vector[Int]]):
  Result[IValidCalleeCandidate, IFindFunctionFailureReason] = {
    candidate match {
      case FunctionCalleeCandidate(ft@FunctionTemplata(declaringEnv, function)) => {
        val identifyingRuneTemplataTypes = function.tyype.paramTypes
        if (explicitTemplateArgRunesS.size > identifyingRuneTemplataTypes.size) {
          Err(WrongNumberOfTemplateArguments(explicitTemplateArgRunesS.size, identifyingRuneTemplataTypes.size))
        } else {
          // Now that we know what types are expected, we can FINALLY rule-type these explicitly
          // specified template args! (The rest of the rule-typing happened back in the astronomer,
          // this is the one time we delay it, see MDRTCUT).

          // There might be less explicitly specified template args than there are types, and that's
          // fine. Hopefully the rest will be figured out by the rule evaluator.
          val explicitTemplateArgRuneToType =
          explicitTemplateArgRunesS.zip(identifyingRuneTemplataTypes).toMap


          val runeTypeSolveEnv =
            new IRuneTypeSolverEnv {
              override def lookup(range: RangeS, nameS: IImpreciseNameS):
              Result[IRuneTypeSolverLookupResult, IRuneTypingLookupFailedError] = {
                callingEnv.lookupNearestWithImpreciseName(nameS, Set(TemplataLookupContext)) match {
                  case Some(x) => Ok(TemplataLookupResult(x.tyype))
                  case None => Err(RuneTypingCouldntFindType(range, nameS))
//                        {
//                          throw CompileErrorExceptionT(
//                            RangedInternalErrorT(
//                              callRange,
//                              "Couldn't find a: " + PostParserErrorHumanizer.humanizeImpreciseName(nameS)))
//                        }
                }
              }
            }

          // And now that we know the types that are expected of these template arguments, we can
          // run these template argument templexes through the solver so it can evaluate them in
          // context of the current environment and spit out some templatas.
          runeTypeSolver.solve(
            opts.globalOptions.sanityCheck,
            opts.globalOptions.useOptimizedSolver,
            runeTypeSolveEnv,
            callRange,
            false,
            explicitTemplateArgRulesS,
            explicitTemplateArgRunesS,
            true,
            explicitTemplateArgRuneToType) match {
            case Err(e@RuneTypeSolveError(_, _)) => {
              Err(RuleTypeSolveFailure(e))
            }
            case Ok(runeAToTypeWithImplicitlyCoercingLookupsS) => {
              // rulesA is the equals rules, but rule typed. Now we'll run them through the solver to get
              // some actual templatas.

              val runeTypeSolveEnv =
                new IRuneTypeSolverEnv {
                  override def lookup(range: RangeS, nameS: IImpreciseNameS):
                  Result[IRuneTypeSolverLookupResult, IRuneTypingLookupFailedError] = {
                    // DO NOT SUBMIT merge with other lookup overrides. maybe make some kind of adapter.
                    callingEnv.lookupNearestWithImpreciseName(nameS, Set(TemplataLookupContext)) match {
                      case Some(CitizenDefinitionTemplata(environment, a)) => {
                        Ok(CitizenRuneTypeSolverLookupResult(a.tyype, a.genericParameters))
                      }
                      case Some(x) => Ok(TemplataLookupResult(x.tyype))
                      case None => Err(RuneTypingCouldntFindType(range, nameS))
                    }
                  }
                }

              val runeAToType =
                mutable.HashMap[IRuneS, ITemplataType]((runeAToTypeWithImplicitlyCoercingLookupsS.toSeq): _*)
              // We've now calculated all the types of all the runes, but the LookupSR rules are still a bit
              // loose. We intentionally ignored the types of the things they're looking up, so we could know
              // what types we *expect* them to be, so we could coerce.
              // That coercion is good, but lets make it more explicit.
              val ruleBuilder = ArrayBuffer[IRulexSR]()
              explicifyLookups(
                runeTypeSolveEnv,
                runeAToType, ruleBuilder, explicitTemplateArgRulesS) match {
                case Err(RuneTypingTooManyMatchingTypes(range, name)) => throw CompileErrorExceptionT(TooManyTypesWithNameT(range :: callRange, name))
                case Err(RuneTypingCouldntFindType(range, name)) => throw CompileErrorExceptionT(CouldntFindTypeT(range :: callRange, name))
                case Ok(()) =>
              }
              val rulesWithoutImplicitCoercionsA = ruleBuilder.toVector

              // We preprocess out the rune parent env lookups, see MKRFA.
              val (initialKnownsWithoutDefaultRegion, rulesWithoutRuneParentEnvLookups) =
                rulesWithoutImplicitCoercionsA.foldLeft((Vector[InitialKnown](), Vector[IRulexSR]()))({
                  case ((previousConclusions, remainingRules), RuneParentEnvLookupSR(_, rune)) => {
                    val templata =
                      vassertSome(
                        callingEnv.lookupNearestWithImpreciseName(
                          interner.intern(RuneNameS(rune.rune)), Set(TemplataLookupContext)))
                    val newConclusions = previousConclusions :+ InitialKnown(rune, templata)
                    (newConclusions, remainingRules)
                  }
                  case ((previousConclusions, remainingRules), rule) => {
                    (previousConclusions, remainingRules :+ rule)
                  }
                })
              // If the rules ask for a default region, here's where we supply it
              val initialKnowns =
//                if (runeAToType.contains(DefaultRegionRuneS())) {
//                  initialKnownsWithoutDefaultRegion :+
//                    InitialKnown(
//                      RuneUsage(callRange.head, DefaultRegionRuneS()),
//                      contextRegion)
//                } else {
                  initialKnownsWithoutDefaultRegion
//                }

              // We only want to solve the template arg runes
              inferCompiler.solveComplete(
                InferEnv(callingEnv, callRange, callLocation, declaringEnv, contextRegion),
                coutputs,
                rulesWithoutRuneParentEnvLookups,
                explicitTemplateArgRuneToType ++ runeAToType,
                callRange,
                callLocation,
                initialKnowns,
                Vector(),
                true,
                false,
                Vector()) match {
                case (Err(e)) => {
                  Err(InferFailure(e))
                }
                case (Ok(CompleteCompilerSolve(_, explicitRuneSToTemplata, _, Vector()))) => {
                  val explicitlySpecifiedTemplateArgTemplatas =
                    explicitTemplateArgRunesS.map(explicitRuneSToTemplata)

                  if (ft.function.isLambda()) {
                    // We pass in our env because the callee needs to see functions declared here, see CSSNCE.
                    functionCompiler.evaluateTemplatedFunctionFromCallForPrototype(
                      coutputs, callingEnv, callRange, callLocation, ft, explicitlySpecifiedTemplateArgTemplatas.toVector, contextRegion, args) match {
                      case (EvaluateFunctionFailure(reason)) => Err(reason)
                      case (EvaluateFunctionSuccess(prototype, conclusions)) => {
                        paramsMatch(coutputs, callingEnv, callRange, args, prototype.prototype.paramTypes, exact) match {
                          case Err(rejectionReason) => Err(rejectionReason)
                          case Ok(()) => {
                            vassert(coutputs.getInstantiationBounds(prototype.prototype.id).nonEmpty)
                            vregionmut() // check regions?
                            Ok(ast.ValidPrototypeTemplataCalleeCandidate(vimpl(), prototype))
                          }
                        }
                      }
                    }
                  } else {
                    val isPure = ft.function.attributes.collectFirst({ case PureS => }).nonEmpty
                    val (maybeNewRegion, maybeLatestPureBlockLocation) =
                      if (isPure) {
                        val calleeContextRegion =
                          PlaceholderTemplata(
                            callingEnv.denizenId
                              .addStep(
                                interner.intern(RegionPlaceholderNameT(
                                  -1,
                                  PureCallRegionRuneS(callLocation),
                                  callLocation,
                                  true))),
                            RegionTemplataType())
                        val newLatestPureBlockLocation = callLocation
                        (Some(calleeContextRegion), Some(newLatestPureBlockLocation))
                      } else {
                        (None, callingEnv.maybeLatestPureBlockLocation)
                      }
                    val calleeContextRegion = maybeNewRegion.getOrElse(contextRegion)
                    // We pass in our env because the callee needs to see functions declared here, see CSSNCE.
                    functionCompiler.evaluateGenericLightFunctionFromCallForPrototype(
                      coutputs, callRange, callLocation, callingEnv, ft, explicitlySpecifiedTemplateArgTemplatas.toVector, calleeContextRegion, args) match {
                      case (EvaluateFunctionFailure(reason)) => Err(reason)
                      case (EvaluateFunctionSuccess(prototype, conclusions)) => {
                        paramsMatch(coutputs, callingEnv, callRange, args, prototype.prototype.paramTypes, exact) match {
                          case Err(rejectionReason) => Err(rejectionReason)
                          case Ok(()) => {
                            vassert(coutputs.getInstantiationBounds(prototype.prototype.id).nonEmpty)
                          }
                        }
                        checkRegions(ft, maybeLatestPureBlockLocation, conclusions) match {
                          case Err(e) => return Err(e)
                          case Ok(()) =>
                        }
                        Ok(ast.ValidPrototypeTemplataCalleeCandidate(maybeNewRegion, prototype))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      case HeaderCalleeCandidate(header) => {
        vregionmut() // what if its a pure call

        paramsMatch(coutputs, callingEnv, callRange, args, header.paramTypes, exact) match {
          case Ok(_) => {
            Ok(ValidHeaderCalleeCandidate(header))
          }
          case Err(fff) => Err(fff)
        }
      }
      case PrototypeTemplataCalleeCandidate(declarationRange, prototype) => {
        vregionmut() // what if its a pure call

        // We get here if we're considering a function that's being passed in as a bound.
        vcurious(prototype.id.localName.templateArgs.isEmpty)
        val substituter =
          TemplataCompiler.getPlaceholderSubstituter(
            interner, keywords, prototype.id,
//            // This is a bound function, and they don't have generic parameters of their own,
//            // they're phrased in terms of the containing function. see CFCHR.
//            Vector(),
            // These types are phrased in terms of the calling denizen already, so we can grab their
            // bounds.
            InheritBoundsFromTypeItself)
        val params = prototype.id.localName.parameters.map(paramType => {
          substituter.substituteForCoord(coutputs, paramType)
        })
        paramsMatch(coutputs, callingEnv, callRange, args, params, exact) match {
          case Ok(_) => {
            // This can be for example:
            //   func bork<T>(a T) where func drop(T)void {
            //     drop(a);
            //   }
            // We're calling a function that came from a bound.
            // Function bounds (like the `func drop(T)void` don't have bounds themselves)
            // so we just supply an empty map here.
            val bounds = Map[IRuneS, PrototypeTemplata]()

            vassert(coutputs.getInstantiationBounds(prototype.id).nonEmpty)
            Ok(ValidPrototypeTemplataCalleeCandidate(vimpl(), PrototypeTemplata(declarationRange, prototype)))
          }
          case Err(fff) => Err(fff)
        }
      }
    }
  }

  private def checkRegions(
    ft: FunctionTemplata,
    maybeLatestPureBlockLocation: Option[LocationInDenizen],
    conclusions: Map[IRuneS, ITemplata[ITemplataType]]):
  Result[Unit, IFindFunctionFailureReason] = {
    // The only place we can specify a region's mutability in the receiving function
    // is in the generic parameter declaration. So all we need to do is loop
    // through the generic parameter declarations and make sure the region we
    // give for them has the right mutability.
    ft.function.genericParameters.foreach(genericParam => {
      genericParam.tyype match {
        case RegionGenericParameterTypeS(mutability) => {
          mutability match {
            case ReadOnlyRegionS => {
              // Do nothing, it can receive any region mutability
            }
            case ReadWriteRegionS | ImmutableRegionS => {
              val expectedMutable = (mutability == ReadWriteRegionS)
              conclusions.get(genericParam.rune.rune) match {
                case Some(PlaceholderTemplata(IdT(_, _, RegionPlaceholderNameT(index, rune, originallyIntroducedLocation, originallyMutable)), RegionTemplataType())) => {
                  // Someday we'll also want to check if there were any pure blocks since the
                  // region was created.
                  // Until then, it's only mutable if it originally was and this isn't a pure call.
                  val actuallyMutable =
                  originallyMutable &&
                    (maybeLatestPureBlockLocation match {
                      case None => true
                      case Some(latestPureBlockLocation) => {
                        !originallyIntroducedLocation.before(latestPureBlockLocation)
                      }
                    })

                  if (actuallyMutable != expectedMutable) {
                    // DO NOT SUBMIT test this
                    return Err(SpecificParamRegionDoesntMatch(genericParam.rune.rune, actuallyMutable, expectedMutable))
                  }
                }
                case Some(RegionTemplata(mutable)) => {
                  if (mutable != expectedMutable) {
                    // DO NOT SUBMIT test this
                    return Err(SpecificParamRegionDoesntMatch(genericParam.rune.rune, mutable, expectedMutable))
                  }
                }
                case other => vwat(other)
              }
            }
          }
        }
        case _ =>
      }
    })
    Ok(())
  }

  // Gets all the environments for all the arguments.
  private def getParamEnvironments(coutputs: CompilerOutputs, range: List[RangeS], paramFilters: Vector[CoordT]):
  Vector[IInDenizenEnvironment] = {
    paramFilters.flatMap({ case tyype =>
      (tyype.kind match {
        case sr @ StructTT(_) => Vector(coutputs.getOuterEnvForType(range, TemplataCompiler.getStructTemplate(sr.id)))
        case ir @ InterfaceTT(_) => Vector(coutputs.getOuterEnvForType(range, TemplataCompiler.getInterfaceTemplate(ir.id)))
        case KindPlaceholderT(fullName) => Vector(coutputs.getOuterEnvForType(range, TemplataCompiler.getPlaceholderTemplate(fullName)))
        case _ => Vector.empty
      })
    })
  }

  // Checks to see if there's a function that *could*
  // exist that takes in these parameter types, and returns what the signature *would* look like.
  // Only considers when arguments match exactly.
  // If given something in maybeSuperInterfaceRef2, it will search for a function that
  // overrides that interfaceTT in that position. If we ever support multimethods we
  // might need to take a list of these, same length as the arg types... or combine
  // them somehow.
  def findPotentialFunction(
    env: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    functionName: IImpreciseNameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Vector[IRuneS],
    contextRegion: ITemplata[RegionTemplataType],
    args: Vector[CoordT],
    extraEnvsToLookIn: Vector[IInDenizenEnvironment],
    exact: Boolean,
    verifyConclusions: Boolean):
  Result[IValidCalleeCandidate, FindFunctionFailure] = {
    functionName match {
      case CodeNameS(StrI("get")) => {
        vpass()
      }
      case _ =>
    }
    // This is here for debugging, so when we dont find something we can see what envs we searched
    val searchedEnvs = new Accumulator[SearchedEnvironment]()
    val undedupedCandidates = new Accumulator[ICalleeCandidate]()
    getCandidateBanners(
      env, coutputs, callRange, functionName, args, extraEnvsToLookIn, searchedEnvs, undedupedCandidates)
    val candidates = undedupedCandidates.buildArray().distinct
    val attempted =
      candidates.map(candidate => {
        attemptCandidateBanner(
          env, coutputs, callRange, callLocation, explicitTemplateArgRulesS,
          explicitTemplateArgRunesS, contextRegion, args, candidate, exact, verifyConclusions)
          .mapError(e => (candidate -> e))
      })
    val (successes, failedToReason) = Result.split(attempted)

    if (successes.isEmpty) {
      Err(FindFunctionFailure(functionName, args, failedToReason))
    } else if (successes.size == 1) {
      Ok(successes.head)
    } else {
      val (best, outscoreReasonByBanner) =
        narrowDownCallableOverloads(coutputs, env, callRange, successes, args)
      Ok(best)
    }
  }

  // Returns either:
  // - None if banners incompatible
  // - Some(param to needs-conversion)
  private def getBannerParamScores(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironment,
    parentRanges: List[RangeS],
    candidate: IValidCalleeCandidate,
    argTypes: Vector[CoordT]):
  (Option[Vector[Boolean]]) = {
    val initial: Option[Vector[Boolean]] = Some(Vector())
    candidate.paramTypes.zip(argTypes)
      .foldLeft(initial)({
        case (None, _) => None
        case (Some(previous), (paramType, argType)) => {
          if (argType == paramType) {
            Some(previous :+ false)
          } else {
            if (templataCompiler.isTypeConvertible(coutputs, callingEnv, parentRanges, argType, paramType)) {
              Some(previous :+ true)
            } else {
              None
            }
          }
        }
      })
  }

  private def narrowDownCallableOverloads(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironment,
    callRange: List[RangeS],
    unfilteredBanners: Iterable[IValidCalleeCandidate],
    argTypes: Vector[CoordT]):
  (
    IValidCalleeCandidate,
    // Rejection reason by banner
    Map[IValidCalleeCandidate, IFindFunctionFailureReason]) = {

    // Sometimes a banner might come from many different environments (remember,
    // when we do a call, we look in the environments of all the arguments' types).
    // Here we weed out these duplicates.
    val dedupedBanners =
      unfilteredBanners.foldLeft(Vector[IValidCalleeCandidate]())({
        case (potentialBannerByBannerSoFar, currentPotentialBanner) => {
          if (potentialBannerByBannerSoFar.exists(_.range == currentPotentialBanner.range)) {
            potentialBannerByBannerSoFar
          } else {
            potentialBannerByBannerSoFar :+ currentPotentialBanner
          }
        }
      })

    // If there are multiple overloads with the same exact parameter list,
    // then get rid of the templated ones; ordinary ones get priority.
    val banners =
      dedupedBanners.groupBy(_.paramTypes).values.flatMap({ potentialBannersWithSameParamTypes =>
        val ordinaryBanners =
          potentialBannersWithSameParamTypes.filter({
            case ValidCalleeCandidate(_, _, function) => false
            case ValidPrototypeTemplataCalleeCandidate(_, prototype) => true
            case ValidHeaderCalleeCandidate(_) => true
          })
        if (ordinaryBanners.isEmpty) {
          // No ordinary banners, so include all the templated ones
          potentialBannersWithSameParamTypes
        } else {
          // There are some ordinary banners, so only consider the ordinary banners
          ordinaryBanners
        }
      }).toVector

    val bannerIndexToScore =
      banners.map(banner => {
        vassertSome(getBannerParamScores(coutputs, callingEnv, callRange, banner, argTypes))
      })

    // For any given parameter:
    // - If all candidates require a conversion, keep going
    //   (This might be a mistake, should we throw an error instead?)
    // - If no candidates require a conversion, keep going
    // - If some candidates require a conversion, disqualify those candidates

    val paramIndexToSurvivingBannerIndices =
      argTypes.indices.map(paramIndex => {
        val bannerIndexToRequiresConversion =
          bannerIndexToScore.zipWithIndex.map({
            case (paramIndexToScore, bannerIndex) => paramIndexToScore(paramIndex)
          })
        if (bannerIndexToRequiresConversion.forall(_ == true)) {
          // vfail("All candidates require conversion for param " + paramIndex)
          bannerIndexToScore.indices
        } else if (bannerIndexToRequiresConversion.forall(_ == false)) {
          bannerIndexToScore.indices
        } else {
          val survivingBannerIndices =
            bannerIndexToRequiresConversion.zipWithIndex.filter(_._1).map(_._2)
          survivingBannerIndices
        }
      })
    // Now, each parameter knows what candidates it disqualifies.
    // See if there's exactly one candidate that all parameters agree on.
    val survivingBannerIndices =
      paramIndexToSurvivingBannerIndices.foldLeft(bannerIndexToScore.indices.toVector)({
        case (a, b) => a.intersect(b)
      })

    // Dedupe all bounds by prototype
    val grouped =
      survivingBannerIndices
        .groupBy(index => {
          banners(index) match {
            case ValidPrototypeTemplataCalleeCandidate(_, PrototypeTemplata(_, PrototypeT(IdT(_, _, FunctionBoundNameT(FunctionBoundTemplateNameT(firstHumanName, _), firstTemplateArgs, firstParameters)), firstReturnType))) => {
              Some((firstHumanName, firstParameters, firstReturnType))
            }
            case _ => None
          }
        })
    // If there's a non-bound candidate, then go with it
    val nonPrototypeCandidateIndices = grouped.getOrElse(None, Vector())
    val dedupedCandidateIndices =
      if (nonPrototypeCandidateIndices.nonEmpty) {
        nonPrototypeCandidateIndices
      } else {
        // If all the candidates are bounds, then just pick one of them.
        val prototypeCandidateIndices = (grouped - None).map(_._2.head)
        prototypeCandidateIndices.toVector
      }

    val finalBannerIndex =
      if (dedupedCandidateIndices.size == 0) {
        // This can happen if the parameters don't agree who the best
        // candidates are.
        vfail("No candidate is a clear winner!")
      } else if (dedupedCandidateIndices.size == 1) {
        dedupedCandidateIndices.head
      } else {
        throw CompileErrorExceptionT(
          CouldntNarrowDownCandidates(
            callRange,
            dedupedCandidateIndices.map(banners)
              .map(_.range.getOrElse(RangeS.internal(interner, -296729)))))
      }

    val rejectedBanners =
      banners.zipWithIndex.filter(_._2 != finalBannerIndex).map(_._1)
    val rejectionReasonByBanner =
      rejectedBanners.map((_, Outscored())).toMap

    (banners(finalBannerIndex), rejectionReasonByBanner)
  }

  def stampPotentialFunctionForBanner(
    callingEnv: IDenizenEnvironmentBox,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    potentialBanner: IValidCalleeCandidate,
    contextRegion: ITemplata[RegionTemplataType],
    verifyConclusions: Boolean):
  (PrototypeTemplata) = {
    potentialBanner match {
      case ValidCalleeCandidate(banner, _, ft @ FunctionTemplata(_, _)) => {
//        if (ft.function.isTemplate) {
          val (EvaluateFunctionSuccess(successBanner, conclusions)) =
            functionCompiler.evaluateTemplatedLightFunctionFromCallForPrototype(
              coutputs, callingEnv, callRange, callLocation, ft, Vector.empty, contextRegion, banner.paramTypes);
          successBanner
//        } else {
//          functionCompiler.evaluateOrdinaryFunctionFromNonCallForBanner(
//            coutputs, callRange, ft, verifyConclusions)
//        }
      }
      case ValidHeaderCalleeCandidate(header) => {
        vassert(coutputs.getInstantiationBounds(header.toPrototype.id).nonEmpty)
        PrototypeTemplata(vassertSome(header.maybeOriginFunctionTemplata).function.range, header.toPrototype)
      }
    }
  }

  private def stampPotentialFunctionForPrototype(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironment, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    potentialBanner: IValidCalleeCandidate,
    contextRegion: ITemplata[RegionTemplataType],
    args: Vector[CoordT],
    verifyConclusions: Boolean):
  StampFunctionSuccess = {
    potentialBanner match {
      case ValidCalleeCandidate(header, templateArgs, ft @ FunctionTemplata(_, _)) => {
        if (ft.function.isLambda()) {
//          if (ft.function.isTemplate) {
            functionCompiler.evaluateTemplatedFunctionFromCallForPrototype(
                coutputs,callRange, callLocation, callingEnv, ft, templateArgs, contextRegion, args, verifyConclusions) match {
              case EvaluateFunctionSuccess(prototype, inferences) => StampFunctionSuccess(vimpl(), prototype, inferences)
              case (eff@EvaluateFunctionFailure(_)) => vfail(eff.toString)
            }
//          } else {
//            // debt: look into making FunctionCompiler's methods accept function templatas
//            // so we dont pass in the wrong environment again
//            functionCompiler.evaluateOrdinaryFunctionFromCallForPrototype(
//              coutputs, callingEnv, callRange, ft)
//          }
        } else {
          functionCompiler.evaluateGenericLightFunctionFromCallForPrototype(
            coutputs, callRange, callLocation, callingEnv, ft, templateArgs, contextRegion, args) match {
            case EvaluateFunctionSuccess(prototype, inferences) => {
              StampFunctionSuccess(vimpl(), prototype, inferences)
            }
            case (EvaluateFunctionFailure(fffr)) => {
              throw CompileErrorExceptionT(CouldntEvaluateFunction(callRange, fffr))
            }
          }
        }
      }
      case ValidHeaderCalleeCandidate(header) => {
        val declarationRange = vassertSome(header.maybeOriginFunctionTemplata).function.range
        vassert(coutputs.getInstantiationBounds(header.toPrototype.id).nonEmpty)
        StampFunctionSuccess(vimpl(), PrototypeTemplata(declarationRange, header.toPrototype), Map())
      }
      case ValidPrototypeTemplataCalleeCandidate(maybeNewRegion, prototype) => {
        vassert(coutputs.getInstantiationBounds(prototype.prototype.id).nonEmpty)
        StampFunctionSuccess(maybeNewRegion, prototype, Map())
      }
    }
  }

  def getArrayGeneratorPrototype(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironment,
    range: List[RangeS],
    callLocation: LocationInDenizen,
    callableTE: ReferenceExpressionTE,
    contextRegion: ITemplata[RegionTemplataType],
    verifyConclusions: Boolean):
  PrototypeT = {
    val funcName = interner.intern(CodeNameS(keywords.underscoresCall))
    val paramFilters =
      Vector(
        callableTE.result.underlyingCoord,
        CoordT(ShareT, vimpl(), IntT.i32))
      findFunction(
        callingEnv, coutputs, range, callLocation, funcName, Vector.empty, Vector.empty, contextRegion,
        paramFilters, Vector.empty, false, verifyConclusions) match {
        case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(range, e))
        case Ok(x) => x.prototype.prototype
      }
  }

  def getArrayConsumerPrototype(
    coutputs: CompilerOutputs,
    fate: FunctionEnvironmentBox,
    range: List[RangeS],
    callLocation: LocationInDenizen,
    callableTE: ReferenceExpressionTE,
    elementType: CoordT,
    contextRegion: ITemplata[RegionTemplataType],
    verifyConclusions: Boolean):
  PrototypeT = {
    val funcName = interner.intern(CodeNameS(keywords.underscoresCall))
    val paramFilters =
      Vector(
        callableTE.result.underlyingCoord,
        elementType)
    findFunction(
      fate.snapshot, coutputs, range, callLocation, funcName, Vector.empty, Vector.empty, contextRegion, paramFilters, Vector.empty, false, verifyConclusions) match {
      case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(range, e))
      case Ok(x) => x.prototype.prototype
    }
  }
}
