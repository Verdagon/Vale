package net.verdagon.vale.templar

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{GlobalFunctionFamilyNameS, IRuneS, RuneTypeSolveError, RuneTypeSolver}
import net.verdagon.vale.scout.rules.{EqualsSR, IRulexSR, RuneUsage}
import net.verdagon.vale.solver.{CompleteSolve, FailedSolve, IIncompleteOrFailedSolve}
import net.verdagon.vale.templar.OverloadTemplar.RuleTypeSolveFailure
import net.verdagon.vale.templar.ast.{AbstractT, ExternCalleeCandidate, FunctionBannerT, FunctionCalleeCandidate, ICalleeCandidate, IValidCalleeCandidate, OverrideT, ParameterT, PrototypeT, ReferenceExpressionTE, ValidCalleeCandidate, ValidExternCalleeCandidate}
import net.verdagon.vale.templar.infer.ITemplarSolverError
import net.verdagon.vale.templar.names.TemplataNamer
import net.verdagon.vale.{Err, Ok, RangeS, Result, vassertOne, vpass}
//import net.verdagon.vale.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator, RuleTyperSolveFailure, RuleTyperSolveSuccess}
//import net.verdagon.vale.scout.rules.{EqualsSR, TemplexSR, TypedSR}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.scout.{CodeRuneS, CodeTypeNameS, ExplicitTemplateArgRuneS, INameS}
import net.verdagon.vale.templar.OverloadTemplar.{IScoutExpectedFunctionFailureReason, InferFailure, ScoutExpectedFunctionFailure, SpecificParamDoesntMatch, SpecificParamVirtualityDoesntMatch, WrongNumberOfArguments, WrongNumberOfTemplateArguments}
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.FunctionTemplar
import net.verdagon.vale.templar.function.FunctionTemplar.{EvaluateFunctionFailure, EvaluateFunctionSuccess, IEvaluateFunctionResult}
//import net.verdagon.vale.templar.infer.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.{IProfiler, vassert, vcurious, vfail, vimpl}

import scala.collection.immutable.List

object OverloadTemplar {

  sealed trait IScoutExpectedFunctionFailureReason
  case class WrongNumberOfArguments(supplied: Int, expected: Int) extends IScoutExpectedFunctionFailureReason { override def hashCode(): Int = vcurious() }
  case class WrongNumberOfTemplateArguments(supplied: Int, expected: Int) extends IScoutExpectedFunctionFailureReason { override def hashCode(): Int = vcurious() }
  case class SpecificParamDoesntMatch(index: Int, reason: String) extends IScoutExpectedFunctionFailureReason { override def hashCode(): Int = vcurious() }
  case class SpecificParamVirtualityDoesntMatch(index: Int) extends IScoutExpectedFunctionFailureReason { override def hashCode(): Int = vcurious() }
//  case class Outscored() extends IScoutExpectedFunctionFailureReason { override def hashCode(): Int = vcurious() }
  case class RuleTypeSolveFailure(reason: RuneTypeSolveError) extends IScoutExpectedFunctionFailureReason { override def hashCode(): Int = vcurious() }
  case class InferFailure(reason: IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata, ITemplarSolverError]) extends IScoutExpectedFunctionFailureReason { override def hashCode(): Int = vcurious() }

  case class ScoutExpectedFunctionFailure(
    name: INameS,
    args: Vector[ParamFilter],
//    // All the ones that could have worked, but were outscored by the best match
//    outscoredCalleeToReason: Map[IPotentialCallee, IScoutExpectedFunctionFailureReason],
    // All the banners we rejected, and the reason why
    rejectedCalleeToReason: Map[ICalleeCandidate, IScoutExpectedFunctionFailureReason]
  ) {
    vpass()
    override def hashCode(): Int = vcurious()
    override def toString: String = vfail() // shouldnt use toString
  }
}

class OverloadTemplar(
    opts: TemplarOptions,
    profiler: IProfiler,
    templataTemplar: TemplataTemplar,
    inferTemplar: InferTemplar,
    functionTemplar: FunctionTemplar) {
  def scoutMaybeFunctionForPrototype(
    // The environment to look in.
    env: IEnvironment,
    temputs: Temputs,
    callRange: RangeS,
    functionName: INameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    args: Vector[ParamFilter],
    extraEnvsToLookIn: Vector[IEnvironment],
    exact: Boolean):
  Result[PrototypeT, ScoutExpectedFunctionFailure] = {
    profiler.newProfile("scoutMaybeFunctionForPrototype", "", () => {
      scoutPotentialFunction(
          env,
          temputs,
          callRange,
          functionName,
          explicitTemplateArgRulesS,
          explicitTemplateArgRunesS,
          args,
          extraEnvsToLookIn,
          exact) match {
        case Err(e) => Err(e)
        case Ok(potentialBanner) => {
          Ok(stampPotentialFunctionForPrototype(temputs, callRange, potentialBanner, args))
        }
      }
    })
  }

  def scoutExpectedFunctionForPrototype(
    env: IEnvironment,
    temputs: Temputs,
    callRange: RangeS,
    functionName: INameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    args: Vector[ParamFilter],
    extraEnvsToLookIn: Vector[IEnvironment],
    exact: Boolean):
  PrototypeT = {
    scoutMaybeFunctionForPrototype(
      env, temputs, callRange, functionName, explicitTemplateArgRulesS,
      explicitTemplateArgRunesS, args, extraEnvsToLookIn, exact) match {
      case Err(seff @ ScoutExpectedFunctionFailure(_, _, _)) => {
        throw CompileErrorExceptionT(CouldntFindFunctionToCallT(callRange, seff))
      }
      case Ok(p) => p
    }
  }

  private def paramMatches(
    temputs: Temputs,
    source: CoordT,
    destination: CoordT,
    exact: Boolean):
  (
    // Rejection reason, if any. None means it matches.
    Option[String]
  ) = {
    if (exact) {
      if (source == destination) {
        (None)
      } else {
        (Some(TemplataNamer.getReferenceIdentifierName(source) + " is not " + TemplataNamer.getReferenceIdentifierName(destination)))
      }
    } else {
      templataTemplar.isTypeConvertible(temputs, source, destination) match {
        case (true) => (None)
        case (false) => (Some(source + " cannot convert to " + destination))
      }
    }
  }

  private def paramsMatch(
    temputs: Temputs,
    desiredParams: Vector[ParamFilter],
    candidateParams: Vector[ParameterT],
    exact: Boolean):
  (
    // Rejection reason, if any. None means it matches.
    Option[IScoutExpectedFunctionFailureReason]
  ) = {
    if (desiredParams.size != candidateParams.size) {
      return (Some(WrongNumberOfArguments(desiredParams.size, candidateParams.size)))
    }
    desiredParams.zip(candidateParams).zipWithIndex.foreach({
      case (((desiredParam, candidateParam), paramIndex)) => {
        val ParamFilter(desiredTemplata, desiredMaybeVirtuality) = desiredParam
        val ParameterT(_, candidateMaybeVirtuality, candidateType) = candidateParam
        paramMatches(temputs, desiredTemplata, candidateType, exact) match {
          case (Some(rejectionReason)) => {
            return (Some(SpecificParamDoesntMatch(paramIndex, rejectionReason)))
          }
          case (None) => temputs
        }
        ((desiredMaybeVirtuality, candidateMaybeVirtuality) match {
          case (None, _) =>
          case (desiredVirtuality, candidateVirtuality) => {
            if (desiredVirtuality != candidateVirtuality) {
              return (Some(SpecificParamVirtualityDoesntMatch(paramIndex)))
            }
          }
        })
      }
    })
    // Would have bailed out early if there was a false
    (None)
  }

  private def getCandidateBanners(
    env: IEnvironment,
    temputs: Temputs,
    callRange: RangeS,
    functionName: INameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    paramFilters: Vector[ParamFilter],
    extraEnvsToLookIn: Vector[IEnvironment],
    exact: Boolean):
  Vector[ICalleeCandidate] = {
    val candidates =
      findHayTemplatas(env, temputs, functionName, paramFilters, extraEnvsToLookIn)
    candidates.flatMap({
      case KindTemplata(OverloadSet(overloadsEnv, nameInOverloadsEnv, _)) => {
        getCandidateBanners(
          overloadsEnv, temputs, callRange, nameInOverloadsEnv,
          explicitTemplateArgRulesS, explicitTemplateArgRunesS, paramFilters, Vector.empty, exact)
      }
      case KindTemplata(sr@StructTT(_)) => {
        val structEnv = temputs.getEnvForStructRef(sr)
        getCandidateBanners(
          structEnv, temputs, callRange, GlobalFunctionFamilyNameS(CallTemplar.CALL_FUNCTION_NAME), explicitTemplateArgRulesS, explicitTemplateArgRunesS, paramFilters, Vector.empty, exact)
      }
      case KindTemplata(sr@InterfaceTT(_)) => {
        val interfaceEnv = temputs.getEnvForInterfaceRef(sr)
        getCandidateBanners(
          interfaceEnv, temputs, callRange, GlobalFunctionFamilyNameS(CallTemplar.CALL_FUNCTION_NAME), explicitTemplateArgRulesS, explicitTemplateArgRunesS, paramFilters, Vector.empty, exact)
      }
      case ExternFunctionTemplata(header) => {
        Vector(ExternCalleeCandidate(header))
      }
      case ft@FunctionTemplata(_, function) => {
        Vector(FunctionCalleeCandidate(ft))
      }
    })
  }

  private def attemptCandidateBanner(
    env: IEnvironment,
    temputs: Temputs,
    callRange: RangeS,
    functionName: INameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    paramFilters: Vector[ParamFilter],
    candidate: ICalleeCandidate,
    exact: Boolean):
  Result[IValidCalleeCandidate, IScoutExpectedFunctionFailureReason] = {
    candidate match {
      case FunctionCalleeCandidate(ft@FunctionTemplata(_, function)) => {
        // See OFCBT.
        if (ft.function.isTemplate) {
          function.tyype match {
            case TemplateTemplataType(identifyingRuneTemplataTypes, FunctionTemplataType) => {
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

              //                      val connectingRules = Vector()
              //                        explicitTemplateArgRunesS.zip(function.identifyingRunes).map({
              //                          case (templateArgRuneS, identifyingRune) => {
              //                            EqualsSR(callRange, templateArgRuneS, identifyingRune)
              //                          }
              //                        })

              // And now that we know the types that are expected of these template arguments, we can
              // run these template argument templexes through the solver so it can evaluate them in
              // context of the current environment and spit out some templatas.
              RuneTypeSolver.solve(
                nameS => vassertOne(env.lookupWithImpreciseName(profiler, nameS, Set(TemplataLookupContext), true)).tyype,
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

                  // We only want to solve the template arg runes
                  profiler.childFrame("late astronoming", () => {
                    inferTemplar.solveComplete(
                      env,
                      temputs,
                      explicitTemplateArgRulesS,
                      explicitTemplateArgRuneToType ++ runeTypeConclusions,
                      callRange,
                      Map(),
                      Map()) match {
                      case (Err(e)) => {
                        Err(InferFailure(e))
                      }
                      case (Ok(explicitRuneSToTemplata)) => {
                        val explicitlySpecifiedTemplateArgTemplatas = explicitTemplateArgRunesS.map(explicitRuneSToTemplata)

                        functionTemplar.evaluateTemplatedFunctionFromCallForBanner(
                          temputs, callRange, ft, explicitlySpecifiedTemplateArgTemplatas.toVector, paramFilters) match {
                          case (EvaluateFunctionFailure(reason)) => {
                            Err(reason)
                          }
                          case (EvaluateFunctionSuccess(banner)) => {
                            paramsMatch(temputs, paramFilters, banner.params, exact) match {
                              case (Some(rejectionReason)) => {
                                Err(rejectionReason)
                              }
                              case (None) => {
                                Ok(ValidCalleeCandidate(banner, ft))
                              }
                            }
                          }
                        }
                      }
                    }
                  })
                }
              }
            }
            case FunctionTemplataType => {
              // So it's not a template, but it's a template in context. We'll still need to
              // feed it into the inferer.
              functionTemplar.evaluateTemplatedFunctionFromCallForBanner(
                temputs, callRange, ft, Vector.empty, paramFilters) match {
                case (EvaluateFunctionFailure(reason)) => {
                  Err(reason)
                }
                case (EvaluateFunctionSuccess(banner)) => {
                  paramsMatch(temputs, paramFilters, banner.params, exact) match {
                    case Some(reason) => Err(reason)
                    case None => Ok(ast.ValidCalleeCandidate(banner, ft))
                  }
                }
              }
            }
          }
        } else {
          val banner = functionTemplar.evaluateOrdinaryFunctionFromNonCallForBanner(temputs, callRange, ft)
          paramsMatch(temputs, paramFilters, banner.params, exact) match {
            case (None) => Ok(ast.ValidCalleeCandidate(banner, ft))
            case (Some(reason)) => Err(reason)
          }
        }
      }
      case ExternCalleeCandidate(header) => {
        paramsMatch(temputs, paramFilters, header.params, exact) match {
          case (None) => Ok(ValidExternCalleeCandidate(header))
          case Some(seff) => Err(seff)
        }
      }
    }
  }

//  private def reduceCandidateBanners(
//    env: IEnvironment,
//    temputs: Temputs,
//    callRange: RangeS,
//    functionName: INameS,
//    explicitTemplateArgRulesS: Vector[IRulexSR],
//    explicitTemplateArgRunesS: Array[IRuneS],
//    paramFilters: Vector[ParamFilter],
//    candidates: Vector[IPotentialCalleePotentialBanner],
//    exact: Boolean):
//  (
//    Vector[IPotentialCalleePotentialBanner],
//    // rejection reason by banner
//    Map[IPotentialCallee, IScoutExpectedFunctionFailureReason]
//  ) = {
////            case (Some(rejectionReason)) => {
//        case ft@FunctionTemplata(_, function) => {
//        }
//      })
//    val (successes, failures) =
//      results.map({
//        case Ok(p) => (List(p), List.empty)
//        case Err(e) => (List(e), List.empty)
//      }).unzip
//    (successes.flatten, failures.flatten) match {
//      case
//    }
//  }

  // Gets all the environments for all the arguments.
  private def getParamEnvironments(temputs: Temputs, paramFilters: Vector[ParamFilter]):
  Vector[IEnvironment] = {
    paramFilters.flatMap({ case ParamFilter(tyype, virtuality) =>
      (tyype.kind match {
        case sr @ StructTT(_) => Vector(temputs.getEnvForStructRef(sr))
        case ir @ InterfaceTT(_) => Vector(temputs.getEnvForInterfaceRef(ir))
        case _ => Vector.empty
      }) ++
        (virtuality match {
          case None => Vector.empty
          case Some(AbstractT) => Vector.empty
          case Some(OverrideT(ir)) => Vector(temputs.getEnvForInterfaceRef(ir))
        })
    })
  }

  // Looks in all the environments of the given arguments for something with the given name.
  private def findHayTemplatas(
      env: IEnvironment,
      temputs: Temputs,
      impreciseName: INameS,
      paramFilters: Vector[ParamFilter],
      extraEnvsToLookIn: Vector[IEnvironment]):
  Vector[ITemplata] = {
    val environments = Vector(env) ++ getParamEnvironments(temputs, paramFilters) ++ extraEnvsToLookIn
    environments
      .flatMap(_.lookupAllWithImpreciseName(profiler, impreciseName, Set(ExpressionLookupContext)))
      .distinct
  }

  // Checks to see if there's a function that *could*
  // exist that takes in these parameter types, and returns what the signature *would* look like.
  // Only considers when arguments match exactly.
  // If given something in maybeSuperInterfaceRef2, it will search for a function that
  // overrides that interfaceTT in that position. If we ever support multimethods we
  // might need to take a list of these, same length as the arg types... or combine
  // them somehow.
  def scoutPotentialFunction(
      env: IEnvironment,
      temputs: Temputs,
      callRange: RangeS,
      functionName: INameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
      args: Vector[ParamFilter],
    extraEnvsToLookIn: Vector[IEnvironment],
      exact: Boolean):
  Result[IValidCalleeCandidate, ScoutExpectedFunctionFailure] = {
    val candidates =
      getCandidateBanners(
        env, temputs, callRange, functionName, explicitTemplateArgRulesS,
        explicitTemplateArgRunesS, args, extraEnvsToLookIn, exact)
    val attempted =
      candidates.map(candidate => {
        attemptCandidateBanner(
          env, temputs, callRange, functionName, explicitTemplateArgRulesS,
          explicitTemplateArgRunesS, args, candidate, exact)
          .mapError(e => (candidate -> e))
      })
    val (successes, failedToReasonUnmerged) = Result.split(attempted)
    val failedToReason = failedToReasonUnmerged.toMap

    if (successes.isEmpty) {
      Err(ScoutExpectedFunctionFailure(functionName, args, failedToReason))
    } else if (successes.size == 1) {
      Ok(successes.head)
    } else {
      val (best, outscoreReasonByBanner) =
        narrowDownCallableOverloads(temputs, callRange, successes.toSet, args.map(_.tyype))
      Ok(best)
    }
  }

  private def getBannerParamScores(
    temputs: Temputs,
    banner: IValidCalleeCandidate,
    argTypes: Vector[CoordT]):
  (Vector[TypeDistance]) = {
    banner.banner.paramTypes.zip(argTypes)
      .foldLeft((Vector[TypeDistance]()))({
        case ((previousParamsScores), (paramType, argType)) => {
          templataTemplar.getTypeDistance(temputs, argType, paramType) match {
            case (None) => vfail("wat")
            case (Some(distance)) => (previousParamsScores :+ distance)
          }
        }
      })
  }

  private def narrowDownCallableOverloads(
      temputs: Temputs,
      callRange: RangeS,
      unfilteredBanners: Set[IValidCalleeCandidate],
      argTypes: Vector[CoordT]):
  (
    IValidCalleeCandidate,
    // Rejection reason by banner
    Map[IValidCalleeCandidate, IScoutExpectedFunctionFailureReason]) = {

    // Sometimes a banner might come from many different environments (remember,
    // when we do a call, we look in the environments of all the arguments' types).
    // Here we weed out these duplicates.
    val dedupedBanners =
      unfilteredBanners.foldLeft(Vector[IValidCalleeCandidate]())({
        case (potentialBannerByBannerSoFar, currentPotentialBanner) => {
          if (potentialBannerByBannerSoFar.exists(_.banner == currentPotentialBanner.banner)) {
            potentialBannerByBannerSoFar
          } else {
            potentialBannerByBannerSoFar :+ currentPotentialBanner
          }
        }
      })

    // If there are multiple overloads with the same exact parameter list,
    // then get rid of the templated ones; ordinary ones get priority.
    val banners =
      dedupedBanners.groupBy(_.banner.paramTypes).values.flatMap({ potentialBannersWithSameParamTypes =>
        val ordinaryBanners =
          potentialBannersWithSameParamTypes.filter({
            case ValidCalleeCandidate(_, function) => !function.function.isTemplate
            case ValidExternCalleeCandidate(_) => true
          })
        if (ordinaryBanners.isEmpty) {
          // No ordinary banners, so include all the templated ones
          potentialBannersWithSameParamTypes
        } else {
          // There are some ordinary banners, so only consider the ordinary banners
          ordinaryBanners
        }
      }).toVector

    val bannersAndScores =
      banners.foldLeft((Vector[(IValidCalleeCandidate, Vector[TypeDistance])]()))({
        case ((previousBannersAndScores), banner) => {
          val scores =
            getBannerParamScores(temputs, banner, argTypes)
          (previousBannersAndScores :+ (banner, scores))
        }
      })

    val bestScore =
      bannersAndScores.map(_._2).reduce((aScore, bScore) => {
        if (aScore == bScore) {
          // Doesn't matter, just return one
          aScore
        } else {
          val aIsBetter =
            aScore.zip(bScore).forall({
              case (aScorePart, bScorePart) => aScorePart.lessThanOrEqualTo(bScorePart)
            })
          if (aIsBetter) aScore else bScore
        }
      })

    val bannerByIsBestScore =
      bannersAndScores.groupBy[Boolean]({ case (_, score) => score == bestScore })


    val bannerWithBestScore =
      if (bannerByIsBestScore.getOrElse(true, Vector.empty).isEmpty) {
        vfail("wat")
      } else if (bannerByIsBestScore.getOrElse(true, Vector.empty).size > 1) {
        throw CompileErrorExceptionT(RangedInternalErrorT(callRange, "Can't resolve between:\n" + bannerByIsBestScore.mapValues(_.mkString("\n")).mkString("\n")))
      } else {
        bannerByIsBestScore(true).head._1
      };

    val rejectedBanners =
      bannerByIsBestScore.getOrElse(false, Vector.empty).map(_._1)
//    val rejectionReasonByBanner =
//      rejectedBanners.map((_, Outscored())).toMap
    vimpl()

//    (bannerWithBestScore, rejectionReasonByBanner)
  }

  def stampPotentialFunctionForBanner(
      env: IEnvironmentBox,
      temputs: Temputs,
      callRange: RangeS,
      potentialBanner: IValidCalleeCandidate):
  (FunctionBannerT) = {
    potentialBanner match {
      case ValidCalleeCandidate(signature, ft @ FunctionTemplata(_, _)) => {
        if (ft.function.isTemplate) {
          val (EvaluateFunctionSuccess(banner)) =
            functionTemplar.evaluateTemplatedLightFunctionFromCallForBanner(
              temputs, callRange, ft, Vector.empty, signature.paramTypes.map(p => ParamFilter(p, None)));
          (banner)
        } else {
          functionTemplar.evaluateOrdinaryFunctionFromNonCallForBanner(
            temputs, callRange, ft)
        }
      }
      case ValidExternCalleeCandidate(header) => {
        (header.toBanner)
      }
    }
  }

  // The "for temputs" thing is important, it means we don't care what the result is, we just
  // want to make sure it gets into the outputs.
  private def stampPotentialFunctionForPrototype(
      temputs: Temputs,
      range: RangeS,
      potentialBanner: IValidCalleeCandidate,
      args: Vector[ParamFilter]):
  (PrototypeT) = {
    potentialBanner match {
      case ValidCalleeCandidate(signature, ft @ FunctionTemplata(_, _)) => {
        if (ft.function.isTemplate) {
          functionTemplar.evaluateTemplatedFunctionFromCallForPrototype(
              temputs, range, ft, signature.fullName.last.templateArgs, args) match {
            case (EvaluateFunctionSuccess(prototype)) => (prototype)
            case (eff @ EvaluateFunctionFailure(_)) => vfail(eff.toString)
          }
        } else {
          // debt: look into making FunctionTemplar's methods accept function templatas
          // so we dont pass in the wrong environment again
          functionTemplar.evaluateOrdinaryFunctionFromNonCallForPrototype(
            temputs, range, ft)
        }
      }
      case ValidExternCalleeCandidate(header) => {
        (header.toPrototype)
      }
    }
  }

  def getArrayGeneratorPrototype(
    temputs: Temputs,
    fate: FunctionEnvironmentBox,
    range: RangeS,
    callableTE: ReferenceExpressionTE):
  PrototypeT = {
    val funcName = GlobalFunctionFamilyNameS(CallTemplar.CALL_FUNCTION_NAME)
    val paramFilters =
      Vector(
        ParamFilter(callableTE.resultRegister.underlyingReference, None),
        ParamFilter(CoordT(ShareT, ReadonlyT, IntT.i32), None))
      scoutExpectedFunctionForPrototype(
        fate.snapshot, temputs, range, funcName, Vector.empty, Array.empty,
        paramFilters, Vector.empty, false)
  }

  def getArrayConsumerPrototype(
    temputs: Temputs,
    fate: FunctionEnvironmentBox,
    range: RangeS,
    callableTE: ReferenceExpressionTE,
    elementType: CoordT):
  PrototypeT = {
    val funcName = GlobalFunctionFamilyNameS(CallTemplar.CALL_FUNCTION_NAME)
    val paramFilters =
      Vector(
        ParamFilter(callableTE.resultRegister.underlyingReference, None),
        ParamFilter(elementType, None))
    scoutExpectedFunctionForPrototype(
      fate.snapshot, temputs, range, funcName, Vector.empty, Array.empty, paramFilters, Vector.empty, false)
  }
}
