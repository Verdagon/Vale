package dev.vale.typing

import dev.vale.{Accumulator, Err, Interner, Keywords, Ok, Profiler, RangeS, Result, StrI, vassertSome, vcurious, vfail, vimpl, vpass}
import dev.vale.postparsing._
import dev.vale.postparsing.rules.{DefinitionFuncSR, IRulexSR, RuneParentEnvLookupSR}
import dev.vale.solver.IIncompleteOrFailedSolve
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.infer.ITypingPassSolverError
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.postparsing.PostParserErrorHumanizer
import dev.vale.solver.FailedSolve
import OverloadResolver.{Outscored, RuleTypeSolveFailure, SpecificParamDoesntMatchExactly, SpecificParamDoesntSend}
import dev.vale.typing.ast.{AbstractT, FunctionBannerT, FunctionCalleeCandidate, HeaderCalleeCandidate, ICalleeCandidate, IValidCalleeCandidate, ParameterT, PrototypeT, ReferenceExpressionTE, ValidCalleeCandidate, ValidHeaderCalleeCandidate}
import dev.vale.typing.env.{ExpressionLookupContext, FunctionEnvironmentBox, IEnvironment, IEnvironmentBox, TemplataLookupContext}
import dev.vale.typing.templata._
import dev.vale.typing.ast._
import dev.vale.typing.names.{CallEnvNameT, CodeVarNameT, FullNameT, FunctionNameT, FunctionTemplateNameT}
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
  case class SpecificParamVirtualityDoesntMatch(index: Int) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class Outscored() extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class RuleTypeSolveFailure(reason: RuneTypeSolveError) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }
  case class InferFailure(reason: IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata[ITemplataType], ITypingPassSolverError]) extends IFindFunctionFailureReason { override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious() }

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
    callingEnv: IEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    functionName: IImpreciseNameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    args: Vector[CoordT],
    extraEnvsToLookIn: Vector[IEnvironment],
    exact: Boolean,
    verifyConclusions: Boolean):
  Result[PrototypeTemplata, FindFunctionFailure] = {
    Profiler.frame(() => {
      findPotentialFunction(
        callingEnv,
        coutputs,
        callRange,
        functionName,
        explicitTemplateArgRulesS,
        explicitTemplateArgRunesS,
        args,
        extraEnvsToLookIn,
        exact,
        verifyConclusions) match {
        case Err(e) => return Err(e)
        case Ok(potentialBanner) => {
          Ok(stampPotentialFunctionForPrototype(coutputs, callingEnv, callRange, potentialBanner, args, verifyConclusions))
        }
      }
    })
  }

  private def paramsMatch(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment,
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
    environment: IEnvironment,
    matchingTemplatas: Vector[ITemplata[ITemplataType]])

  private def getCandidateBanners(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    functionName: IImpreciseNameS,
    paramFilters: Vector[CoordT],
    extraEnvsToLookIn: Vector[IEnvironment],
    searchedEnvs: Accumulator[SearchedEnvironment],
    results: Accumulator[ICalleeCandidate]):
  Unit = {
    getCandidateBannersInner(env, coutputs, functionName, searchedEnvs, results)
    getParamEnvironments(coutputs, paramFilters)
      .foreach(e => getCandidateBannersInner(e, coutputs, functionName, searchedEnvs, results))
    extraEnvsToLookIn
      .foreach(e => getCandidateBannersInner(env, coutputs, functionName, searchedEnvs, results))
  }

  private def getCandidateBannersInner(
    env: IEnvironment,
    coutputs: CompilerOutputs,
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
          overloadsEnv, coutputs, nameInOverloadsEnv, searchedEnvs, results)
      }
      case KindTemplata(sr@StructTT(_)) => {
        val structEnv = coutputs.getOuterEnvForType(TemplataCompiler.getStructTemplate(sr.fullName))
        getCandidateBannersInner(
          structEnv, coutputs, interner.intern(CodeNameS(keywords.underscoresCall)), searchedEnvs, results)
      }
      case KindTemplata(sr@InterfaceTT(_)) => {
        val interfaceEnv = coutputs.getOuterEnvForType(TemplataCompiler.getInterfaceTemplate(sr.fullName))
        getCandidateBannersInner(
          interfaceEnv, coutputs, interner.intern(CodeNameS(keywords.underscoresCall)), searchedEnvs, results)
      }
      case ExternFunctionTemplata(header) => {
        results.add(HeaderCalleeCandidate(header))
      }
      case PrototypeTemplata(declarationRange, prototype) => {
        results.add(PrototypeTemplataCalleeCandidate(declarationRange, prototype))
      }
      case ft@FunctionTemplata(_, _) => {
        results.add(FunctionCalleeCandidate(ft))
      }
    })
  }

  private def attemptCandidateBanner(
    callingEnv: IEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    paramFilters: Vector[CoordT],
    candidate: ICalleeCandidate,
    exact: Boolean,
    verifyConclusions: Boolean):
  Result[IValidCalleeCandidate, IFindFunctionFailureReason] = {
    candidate match {
      case FunctionCalleeCandidate(ft@FunctionTemplata(declaringEnv, function)) => {
        // See OFCBT.
        if (ft.function.isTemplate) {
          function.tyype match {
            case TemplateTemplataType(identifyingRuneTemplataTypes, FunctionTemplataType()) => {
              if (explicitTemplateArgRunesS.size > identifyingRuneTemplataTypes.size) {
                throw CompileErrorExceptionT(RangedInternalErrorT(callRange, "Supplied more arguments than there are identifying runes!"))
              }

              // Now that we know what types are expected, we can FINALLY rule-type these explicitly
              // specified template args! (The rest of the rule-typing happened back in the astronomer,
              // this is the one time we delay it, see MDRTCUT).

              // There might be less explicitly specified template args than there are types, and that's
              // fine. Hopefully the rest will be figured out by the rule evaluator.
              val explicitTemplateArgRuneToType =
              explicitTemplateArgRunesS.zip(identifyingRuneTemplataTypes).toMap

              // And now that we know the types that are expected of these template arguments, we can
              // run these template argument templexes through the solver so it can evaluate them in
              // context of the current environment and spit out some templatas.
              runeTypeSolver.solve(
                opts.globalOptions.sanityCheck,
                opts.globalOptions.useOptimizedSolver,
                (nameS: IImpreciseNameS) => {
                  callingEnv.lookupNearestWithImpreciseName(nameS, Set(TemplataLookupContext)) match {
                    case Some(x) => x.tyype
                    case None => {
                      throw CompileErrorExceptionT(
                        RangedInternalErrorT(
                          callRange,
                          "Couldn't find a: " + PostParserErrorHumanizer.humanizeImpreciseName(nameS)))
                    }
                  }
                },
                callRange,
                false,
                explicitTemplateArgRulesS,
                explicitTemplateArgRunesS,
                true,
                explicitTemplateArgRuneToType) match {
                case Err(e@RuneTypeSolveError(_, _)) => {
                  Err(RuleTypeSolveFailure(e))
                }
                case Ok(runeTypeConclusions) => {
                  // rulesA is the equals rules, but rule typed. Now we'll run them through the solver to get
                  // some actual templatas.

                  // We preprocess out the rune parent env lookups, see MKRFA.
                  val (initialKnowns, rulesWithoutRuneParentEnvLookups) =
                    explicitTemplateArgRulesS.foldLeft((Vector[InitialKnown](), Vector[IRulexSR]()))({
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

//                  val callEnv =
//                    GeneralEnvironment.childOf(
//                      interner, callingEnv, callingEnv.fullName.addStep(CallEnvNameT()))

                  // We only want to solve the template arg runes
                  inferCompiler.solveComplete(
                    InferEnv(callingEnv, callRange, declaringEnv),
                    coutputs,
                    rulesWithoutRuneParentEnvLookups,
                    explicitTemplateArgRuneToType ++ runeTypeConclusions,
                    callRange,
                    initialKnowns,
                    Vector(),
                    true,
                    false,
                    false) match {
                    case (Err(e)) => {
                      Err(InferFailure(e))
                    }
                    case (Ok(explicitRuneSToTemplata)) => {
                      val explicitlySpecifiedTemplateArgTemplatas =
                        explicitTemplateArgRunesS.map(explicitRuneSToTemplata)

                      if (ft.function.isLambda()) {
                        // We pass in our env because the callee needs to see functions declared here, see CSSNCE.
                        functionCompiler.evaluateTemplatedFunctionFromCallForBanner(
                          coutputs, callingEnv, callRange, ft, explicitlySpecifiedTemplateArgTemplatas.toVector, paramFilters.map(x => Some(x))) match {
                          case (EvaluateFunctionFailure(reason)) => Err(reason)
                          case (EvaluateFunctionSuccess(banner)) => {
                            paramsMatch(coutputs, callingEnv, callRange, paramFilters, banner.prototype.paramTypes, exact) match {
                              case Err(rejectionReason) => Err(rejectionReason)
                              case Ok(()) => {
                                Ok(ast.ValidPrototypeTemplataCalleeCandidate(banner))
                              }
                            }
                          }
                        }
                      } else {
                        // We pass in our env because the callee needs to see functions declared here, see CSSNCE.
                        functionCompiler.evaluateGenericLightFunctionFromCallForPrototype(
                          coutputs, callRange, callingEnv, ft, explicitlySpecifiedTemplateArgTemplatas.toVector, paramFilters.map(x => Some(x))) match {
                          case (EvaluateFunctionFailure(reason)) => Err(reason)
                          case (EvaluateFunctionSuccess(banner)) => {
                            paramsMatch(coutputs, callingEnv, callRange, paramFilters, banner.prototype.paramTypes, exact) match {
                              case Err(rejectionReason) => Err(rejectionReason)
                              case Ok(()) => {
                                Ok(ast.ValidPrototypeTemplataCalleeCandidate(banner))
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            case FunctionTemplataType() => {
              // So it's not a template, but it's a template in context. We'll still need to
              // feed it into the inferer.
              functionCompiler.evaluateTemplatedFunctionFromCallForBanner(
                coutputs, callingEnv, callRange, ft, Vector.empty, paramFilters.map(x => Some(x))) match {
                case (EvaluateFunctionFailure(reason)) => {
                  Err(reason)
                }
                case (EvaluateFunctionSuccess(banner)) => {
                  paramsMatch(coutputs, callingEnv, callRange, paramFilters, banner.prototype.paramTypes, exact) match {
                    case Err(reason) => Err(reason)
                    case Ok(_) => {
                      Ok(ValidPrototypeTemplataCalleeCandidate(banner))
                    }
                  }
                }
              }
            }
          }
        } else {
          if (ft.function.isLambda()) {
            functionCompiler.evaluateTemplatedFunctionFromCallForPrototype(
                coutputs, callRange, callingEnv, ft, Vector(), paramFilters.map(x => Some(x)), verifyConclusions) match {
              case (EvaluateFunctionFailure(reason)) => {
                Err(reason)
              }
              case (EvaluateFunctionSuccess(banner)) => {
                paramsMatch(coutputs, callingEnv, callRange, paramFilters, banner.prototype.paramTypes, exact) match {
                  case Ok(_) => {
                    Ok(ast.ValidPrototypeTemplataCalleeCandidate(banner))
                  }
                  case Err(reason) => Err(reason)
                }
              }
            }
          } else {
            functionCompiler.evaluateGenericLightFunctionFromCallForPrototype(
              coutputs, callRange, callingEnv, ft, Vector(), paramFilters.map(x => Some(x))) match {
              case (EvaluateFunctionFailure(reason)) => {
                Err(reason)
              }
              case (EvaluateFunctionSuccess(banner)) => {
                paramsMatch(coutputs, callingEnv, callRange, paramFilters, banner.prototype.paramTypes, exact) match {
                  case Ok(_) => Ok(ValidPrototypeTemplataCalleeCandidate(banner))
                  case Err(reason) => Err(reason)
                }
              }
            }
          }
        }
      }
      case HeaderCalleeCandidate(header) => {
        paramsMatch(coutputs, callingEnv, callRange, paramFilters, header.paramTypes, exact) match {
          case Ok(_) => {
            Ok(ValidHeaderCalleeCandidate(header))
          }
          case Err(fff) => Err(fff)
        }
      }
      case PrototypeTemplataCalleeCandidate(declarationRange, prototype) => {
        val substituter = TemplataCompiler.getPlaceholderSubstituter(interner, keywords, prototype.fullName)
        val params = prototype.fullName.last.parameters.map(paramType => {
          substituter.substituteForCoord(paramType)
        })
        paramsMatch(coutputs, callingEnv, callRange, paramFilters, params, exact) match {
          case Ok(_) => {
            Ok(ValidPrototypeTemplataCalleeCandidate(PrototypeTemplata(declarationRange, prototype)))
          }
          case Err(fff) => Err(fff)
        }
      }
    }
  }

  // Gets all the environments for all the arguments.
  private def getParamEnvironments(coutputs: CompilerOutputs, paramFilters: Vector[CoordT]):
  Vector[IEnvironment] = {
    paramFilters.flatMap({ case tyype =>
      (tyype.kind match {
        case sr @ StructTT(_) => Vector(coutputs.getOuterEnvForType(TemplataCompiler.getStructTemplate(sr.fullName)))
        case ir @ InterfaceTT(_) => Vector(coutputs.getOuterEnvForType(TemplataCompiler.getInterfaceTemplate(ir.fullName)))
        case PlaceholderT(fullName) => Vector(coutputs.getOuterEnvForType(TemplataCompiler.getPlaceholderTemplate(fullName)))
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
    env: IEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    functionName: IImpreciseNameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    args: Vector[CoordT],
    extraEnvsToLookIn: Vector[IEnvironment],
    exact: Boolean,
    verifyConclusions: Boolean):
  Result[IValidCalleeCandidate, FindFunctionFailure] = {
    // This is here for debugging, so when we dont find something we can see what envs we searched
    val searchedEnvs = new Accumulator[SearchedEnvironment]()
    val undedupedCandidates = new Accumulator[ICalleeCandidate]()
    getCandidateBanners(
      env, coutputs, functionName, args, extraEnvsToLookIn, searchedEnvs, undedupedCandidates)
    val candidates = undedupedCandidates.buildArray().distinct
    val attempted =
      candidates.map(candidate => {
        attemptCandidateBanner(
          env, coutputs, callRange, explicitTemplateArgRulesS,
          explicitTemplateArgRunesS, args, candidate, exact, verifyConclusions)
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
    callingEnv: IEnvironment,
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
    callingEnv: IEnvironment,
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
            case ValidCalleeCandidate(_, _, function) => !function.function.isTemplate
            case ValidPrototypeTemplataCalleeCandidate(prototype) => true
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

    // If all the candidates are bounds, then just pick one of them.
      val grouped =
        survivingBannerIndices
          .groupBy(index => {
            banners(index) match {
              case ValidPrototypeTemplataCalleeCandidate(PrototypeTemplata(_, PrototypeT(FullNameT(_, _, FunctionNameT(FunctionTemplateNameT(firstHumanName, _), firstTemplateArgs, firstParameters)), firstReturnType))) => Some((firstHumanName, firstParameters, firstReturnType))
              case _ => None
            }
          })
    val nonPrototypeCandidateIndices = grouped.getOrElse(None, Vector())
    val prototypeCandidateIndices = (grouped - None).map(_._2.head)
    val dedupedCandidateIndices = nonPrototypeCandidateIndices ++ prototypeCandidateIndices

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
    callingEnv: IEnvironmentBox,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    potentialBanner: IValidCalleeCandidate,
    verifyConclusions: Boolean):
  (PrototypeTemplata) = {
    potentialBanner match {
      case ValidCalleeCandidate(banner, _, ft @ FunctionTemplata(_, _)) => {
//        if (ft.function.isTemplate) {
          val (EvaluateFunctionSuccess(successBanner)) =
            functionCompiler.evaluateTemplatedLightFunctionFromCallForBanner(
              coutputs, callingEnv, callRange, ft, Vector.empty, banner.paramTypes.map(x => Some(x)));
          (successBanner)
//        } else {
//          functionCompiler.evaluateOrdinaryFunctionFromNonCallForBanner(
//            coutputs, callRange, ft, verifyConclusions)
//        }
      }
      case ValidHeaderCalleeCandidate(header) => {
        PrototypeTemplata(vassertSome(header.maybeOriginFunctionTemplata).function.range, header.toPrototype)
      }
    }
  }

  private def stampPotentialFunctionForPrototype(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: List[RangeS],
    potentialBanner: IValidCalleeCandidate,
    args: Vector[CoordT],
    verifyConclusions: Boolean):
  (PrototypeTemplata) = {
    potentialBanner match {
      case ValidCalleeCandidate(header, templateArgs, ft @ FunctionTemplata(_, _)) => {
        if (ft.function.isLambda()) {
//          if (ft.function.isTemplate) {
            functionCompiler.evaluateTemplatedFunctionFromCallForPrototype(
                coutputs,callRange, callingEnv, ft, templateArgs, args.map(x => Some(x)), verifyConclusions) match {
              case (EvaluateFunctionSuccess(prototype)) => (prototype)
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
            coutputs, callRange, callingEnv, ft, templateArgs, args.map(x => Some(x))) match {
            case (EvaluateFunctionSuccess(prototype)) => (prototype)
            case (EvaluateFunctionFailure(fffr)) => {
              throw CompileErrorExceptionT(CouldntEvaluateFunction(callRange, fffr))
            }
          }
        }
      }
      case ValidHeaderCalleeCandidate(header) => {
        val declarationRange = vassertSome(header.maybeOriginFunctionTemplata).function.range
        PrototypeTemplata(declarationRange, header.toPrototype)
      }
      case ValidPrototypeTemplataCalleeCandidate(prototype) => {
        prototype
      }
    }
  }

  def getArrayGeneratorPrototype(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment,
    range: List[RangeS],
    callableTE: ReferenceExpressionTE,
    verifyConclusions: Boolean):
  PrototypeT = {
    val funcName = interner.intern(CodeNameS(keywords.underscoresCall))
    val paramFilters =
      Vector(
        callableTE.result.underlyingReference,
        CoordT(ShareT, IntT.i32))
      findFunction(
        callingEnv, coutputs, range, funcName, Vector.empty, Array.empty,
        paramFilters, Vector.empty, false, verifyConclusions) match {
        case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(range, e))
        case Ok(x) => x.prototype
      }
  }

  def getArrayConsumerPrototype(
    coutputs: CompilerOutputs,
    fate: FunctionEnvironmentBox,
    range: List[RangeS],
    callableTE: ReferenceExpressionTE,
    elementType: CoordT,
    verifyConclusions: Boolean):
  PrototypeT = {
    val funcName = interner.intern(CodeNameS(keywords.underscoresCall))
    val paramFilters =
      Vector(
        callableTE.result.underlyingReference,
        elementType)
    findFunction(
      fate.snapshot, coutputs, range, funcName, Vector.empty, Array.empty, paramFilters, Vector.empty, false, verifyConclusions) match {
      case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(range, e))
      case Ok(x) => x.prototype
    }
  }
}
