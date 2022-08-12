package dev.vale.typing.function

import dev.vale.highertyping.FunctionA
import dev.vale.{Err, Interner, Keywords, Ok, Profiler, RangeS, vassert, vassertOne, vassertSome, vcheck, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing._
import dev.vale.postparsing.patterns.AtomSP
import dev.vale.typing.{CompileErrorExceptionT, CompilerOutputs, ConvertHelper, DeferredEvaluatingFunction, RangedInternalErrorT, TemplataCompiler, TypingPassOptions, ast}
import dev.vale.typing.ast.{ArgLookupTE, ExternFunctionCallTE, ExternT, FunctionHeaderT, FunctionT, IFunctionAttributeT, LocationInFunctionEnvironment, ParameterT, PrototypeT, PureT, ReferenceExpressionTE, ReturnTE, SignatureT, UserFunctionT}
import dev.vale.typing.env._
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.names.{ExternFunctionNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, IFunctionNameT, NameTranslator}
import dev.vale.typing.templata.CoordTemplata
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing.{ast, _}
import dev.vale.typing.ast._
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.env._

import scala.collection.immutable.{List, Set}

case class ResultTypeMismatchError(expectedType: CoordT, actualType: CoordT) { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

class FunctionCompilerCore(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,

    templataCompiler: TemplataCompiler,
    convertHelper: ConvertHelper,
    delegate: IFunctionCompilerDelegate) {
  val bodyCompiler = new BodyCompiler(opts, nameTranslator, templataCompiler, convertHelper, new IBodyCompilerDelegate {
    override def evaluateBlockStatements(
      coutputs: CompilerOutputs,
      startingNenv: NodeEnvironment,
      nenv: NodeEnvironmentBox,
      life: LocationInFunctionEnvironment,
      parentRanges: List[RangeS],
      exprs: BlockSE
    ): (ReferenceExpressionTE, Set[CoordT]) = {
      delegate.evaluateBlockStatements(coutputs, startingNenv, nenv, life, parentRanges, exprs)
    }

    override def translatePatternList(
      coutputs: CompilerOutputs,
      nenv: NodeEnvironmentBox,
      life: LocationInFunctionEnvironment,
      parentRanges: List[RangeS],
      patterns1: Vector[AtomSP],
      patternInputExprs2: Vector[ReferenceExpressionTE]
    ): ReferenceExpressionTE = {
      delegate.translatePatternList(coutputs, nenv, life, parentRanges, patterns1, patternInputExprs2)
    }
  })

  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def evaluateFunctionForHeader(
    fullEnv: FunctionEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    params2: Vector[ParameterT]):
  (FunctionHeaderT) = {
    opts.debugOut("Evaluating function " + fullEnv.fullName)

//    val functionTemplateName = TemplataCompiler.getFunctionTemplate(fullEnv.fullName)
    val functionTemplateName = fullEnv.fullName

    val life = LocationInFunctionEnvironment(Vector())

    val isDestructor =
      params2.nonEmpty &&
        params2.head.tyype.ownership == OwnT &&
        (fullEnv.fullName.last match {
          case FunctionNameT(humanName, _, _) if humanName == keywords.drop => true
          case _ => false
        })

    val maybeExport =
      fullEnv.function.attributes.collectFirst { case e@ExportS(_) => e }


    val header =
      fullEnv.function.body match {
        case CodeBodyS(body) => {
          declareAndEvaluateFunctionBodyAndAdd(
            fullEnv, coutputs, life, callRange, params2, isDestructor)
        }
        case ExternBodyS => {
          val maybeRetCoord =
            fullEnv.lookupNearestWithImpreciseName(interner.intern(RuneNameS(fullEnv.function.maybeRetCoordRune.get.rune)), Set(TemplataLookupContext)).headOption
          val retCoord =
            maybeRetCoord match {
              case None => vfail("wat")
              case Some(CoordTemplata(r)) => r
            }
          val header =
            makeExternFunction(
              coutputs,
              fullEnv.fullName,
              fullEnv.function.range,
              translateFunctionAttributes(fullEnv.function.attributes),
              params2,
              retCoord,
              Some(FunctionTemplata(fullEnv.parentEnv, fullEnv.function)))
          (header)
        }
        case AbstractBodyS | GeneratedBodyS(_) => {
          val generatorId =
            fullEnv.function.body match {
              case AbstractBodyS => keywords.abstractBody
              case GeneratedBodyS(generatorId) => generatorId
            }
          val signature2 = SignatureT(fullEnv.fullName);
          val maybeRetTemplata =
            fullEnv.function.maybeRetCoordRune match {
              case None => (None)
              case Some(retCoordRune) => {
                fullEnv.lookupNearestWithImpreciseName(interner.intern(RuneNameS(retCoordRune.rune)), Set(TemplataLookupContext)).headOption
              }
            }
          val maybeRetCoord =
            maybeRetTemplata match {
              case None => (None)
              case Some(CoordTemplata(retCoord)) => {
                coutputs.declareFunctionReturnType(signature2, retCoord)
                (Some(retCoord))
              }
              case _ => throw CompileErrorExceptionT(RangedInternalErrorT(callRange, "Must be a coord!"))
            }

          // Funny story... let's say we're current instantiating a constructor,
          // for example MySome<T>().
          // The constructor returns a MySome<T>, which means when we do the above
          // evaluating of the function body, we stamp the MySome<T> struct.
          // That ends up stamping the entire struct, including the constructor.
          // That's what we were originally here for, and evaluating the body above
          // just did it for us O_o
          // So, here we check to see if we accidentally already did it.
          opts.debugOut("doesnt this mean we have to do this in every single generated function?")

          coutputs.lookupFunction(signature2) match {
            case Some(function2) => {
              (function2.header)
            }
            case None => {
              val generator = vassertSome(fullEnv.globalEnv.nameToFunctionBodyMacro.get(generatorId))
              val header =
                generator.generateFunctionBody(
                  fullEnv, coutputs, generatorId, life, callRange,
                  Some(fullEnv.function), params2, maybeRetCoord)
              if (header.toSignature != signature2) {
                throw CompileErrorExceptionT(RangedInternalErrorT(callRange, "Generator made a function whose signature doesn't match the expected one!\n" +
                  "Expected:  " + signature2 + "\n" +
                  "Generated: " + header.toSignature))
              }
              (header)
            }
          }
        }
      }

    maybeExport match {
      case None =>
      case Some(exportPackageCoord) => {
        val exportedName =
          fullEnv.fullName.last match {
            case FunctionNameT(FunctionTemplateNameT(humanName, _), _, _) => humanName
            case _ => vfail("Can't export something that doesn't have a human readable name!")
          }
        coutputs.addFunctionExport(
          fullEnv.function.range,
          header.toPrototype,
          exportPackageCoord.packageCoordinate,
          exportedName)
      }
    }

    if (header.attributes.contains(PureT)) {
      //      header.params.foreach(param => {
      //        if (param.tyype.permission != ReadonlyT) {
      //          throw CompileErrorExceptionT(NonReadonlyReferenceFoundInPureFunctionParameter(fullEnv.function.range, param.name))
      //        }
      //      })
    }

    header
  }

  // Preconditions:
  // - already spawned local env
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  def getFunctionPrototypeForCall(
    fullEnv: FunctionEnvironment,
      coutputs: CompilerOutputs,
    callRange: List[RangeS],
      params2: Vector[ParameterT]):
  (PrototypeT) = {
    getFunctionPrototypeInnerForCall(
      fullEnv, fullEnv.fullName)
  }

  def declareAndEvaluateFunctionBodyAndAdd(
    fullEnv: FunctionEnvironment,
    coutputs: CompilerOutputs,
    life: LocationInFunctionEnvironment,
    parentRanges: List[RangeS],
    paramsT: Vector[ParameterT],
    isDestructor: Boolean):
  FunctionHeaderT = {
    val attributesWithoutExport =
      fullEnv.function.attributes.filter({
        case ExportS(_) => false
        case _ => true
      })

    val attributesT = translateAttributes(attributesWithoutExport)

    val maybeExplicitReturnCoord =
      fullEnv.function.maybeRetCoordRune match {
        case Some(retCoordRune) => {
          fullEnv.lookupNearestWithImpreciseName(

              interner.intern(RuneNameS(retCoordRune.rune)),
              Set(TemplataLookupContext))  match {
            case Some(CoordTemplata(retCoord)) => Some(retCoord)
            case other => vwat(other)
          }
        }
        case None => None
      }

//    maybeExplicitReturnCoord match {
//      case None => {
//        opts.debugOut("Eagerly evaluating function: " + functionFullName)

    val (maybeInferredReturnCoord, body2) =
      bodyCompiler.declareAndEvaluateFunctionBody(
        FunctionEnvironmentBox(fullEnv), coutputs, life, parentRanges, fullEnv.function, maybeExplicitReturnCoord, paramsT, isDestructor)

    val returnCoord =
      maybeExplicitReturnCoord match {
        case Some(x) => x
        case None => {
          maybeInferredReturnCoord match {
            case None => vfail()
            case Some(explicitReturnCoord) => explicitReturnCoord
          }
        }
      }
    val header =
      finalizeHeader(fullEnv, coutputs, attributesT, paramsT, returnCoord)

    // Funny story... let's say we're current instantiating a constructor,
    // for example MySome<T>().
    // The constructor returns a MySome<T>, which means when we do the above
    // evaluating of the function body, we stamp the MySome<T> struct.
    // That ends up stamping the entire struct, including the constructor.
    // That's what we were originally here for, and evaluating the body above
    // just did it for us O_o
    // So, here we check to see if we accidentally already did it.

    coutputs.lookupFunction(header.toSignature) match {
      case None => {
        val function2 = FunctionT(header, body2);
        coutputs.addFunction(function2)
        (function2.header)
      }
      case Some(function2) => {
        (function2.header)
      }
    }
//      }
//      case Some(explicitReturnCoord) => {
//        fullEnv.setReturnType(Some(explicitReturnCoord))
//        val header = finalizeHeader(fullEnv, coutputs, attributesT, paramsT, explicitReturnCoord)
//        opts.debugOut("NOT DEFERRING FUNCTION: " + header.fullName)
//        coutputs.deferEvaluatingFunction(
//          DeferredEvaluatingFunction(
//            header.toPrototype,
//            (coutputs) => {
//              opts.debugOut("Finishing function: " + header.fullName)
//              finishFunctionMaybeDeferred(
//                coutputs,
//                fullEnv.snapshot,
//                life,
//                attributesT,
//                paramsT,
//                isDestructor,
//                maybeExplicitReturnCoord,
//                Some(header))
//            }))
//        header
//      }
//    }
  }

  def getFunctionPrototypeInnerForCall(
    fullEnv: FunctionEnvironment,
    fullName: FullNameT[IFunctionNameT]):
  PrototypeT = {
    val retCoordRune = vassertSome(fullEnv.function.maybeRetCoordRune)
    val returnCoord =
      fullEnv.lookupNearestWithImpreciseName(
        interner.intern(RuneNameS(retCoordRune.rune)),
        Set(TemplataLookupContext))  match {
        case Some(CoordTemplata(retCoord)) => retCoord
        case other => vwat(other)
      }
    PrototypeT(fullName, returnCoord)
  }

  def finalizeHeader(
      fullEnv: FunctionEnvironment,
      coutputs: CompilerOutputs,
      attributesT: Vector[IFunctionAttributeT],
      paramsT: Vector[ParameterT],
      returnCoord: CoordT) = {
    val header = FunctionHeaderT(fullEnv.fullName, attributesT, paramsT, returnCoord, Some(FunctionTemplata(fullEnv.parentEnv, fullEnv.function)));
    coutputs.declareFunctionReturnType(header.toSignature, returnCoord)
    header
  }

//  // By MaybeDeferred we mean that this function might be called later, to reduce reentrancy.
//  private def finishFunctionMaybeDeferred(
//      coutputs: CompilerOutputs,
//      fullEnvSnapshot: FunctionEnvironment,
//      life: LocationInFunctionEnvironment,
//      attributesT: Vector[IFunctionAttributeT],
//      paramsT: Vector[ParameterT],
//      isDestructor: Boolean,
//      maybeExplicitReturnCoord: Option[CoordT],
//      maybePreKnownHeader: Option[FunctionHeaderT]):
//  FunctionHeaderT = {
//  }

  def translateAttributes(attributesA: Vector[IFunctionAttributeS]) = {
    attributesA.map({
      //      case ExportA(packageCoord) => Export2(packageCoord)
      case UserFunctionS => UserFunctionT
      case PureS => PureT
    })
  }

  def makeExternFunction(
      coutputs: CompilerOutputs,
      fullName: FullNameT[IFunctionNameT],
      range: RangeS,
      attributes: Vector[IFunctionAttributeT],
      params2: Vector[ParameterT],
      returnType2: CoordT,
      maybeOrigin: Option[FunctionTemplata]):
  (FunctionHeaderT) = {
    fullName.last match {
      case FunctionNameT(FunctionTemplateNameT(humanName, _), Vector(), params) => {
        val header =
          ast.FunctionHeaderT(
            fullName,
            Vector(ExternT(range.file.packageCoordinate)) ++ attributes,
            params2,
            returnType2,
            maybeOrigin)

        val externFullName = FullNameT(fullName.packageCoord, Vector.empty, interner.intern(ExternFunctionNameT(humanName, params)))
        val externPrototype = PrototypeT(externFullName, header.returnType)
        coutputs.addFunctionExtern(range, externPrototype, fullName.packageCoord, humanName)

        val argLookups =
          header.params.zipWithIndex.map({ case (param2, index) => ArgLookupTE(index, param2.tyype) })
        val function2 =
          FunctionT(
            header,
            ReturnTE(ExternFunctionCallTE(externPrototype, argLookups)))

        coutputs.declareFunctionReturnType(header.toSignature, header.returnType)
        coutputs.addFunction(function2)
        (header)
      }
      case _ => throw CompileErrorExceptionT(RangedInternalErrorT(List(range), "Only human-named function can be extern!"))
    }
  }

  def translateFunctionAttributes(a: Vector[IFunctionAttributeS]): Vector[IFunctionAttributeT] = {
    a.map({
      case UserFunctionS => UserFunctionT
      case ExternS(packageCoord) => ExternT(packageCoord)
      case x => vimpl(x.toString)
    })
  }


//  def makeImplDestructor(
//    env: FunctionEnvironment,
//    coutputs: CompilerOutputs,
//    maybeOriginFunction1: Option[FunctionA],
//    structDefT: StructDefinitionT,
//    interfaceTT: InterfaceTT,
//    structDestructor: PrototypeT,
//  ):
//  (FunctionHeaderT) = {
//    val ownership = if (structDefT.mutability == MutableT) OwnT else ShareT
//    val permission = if (structDefT.mutability == MutableT) ReadwriteT else ReadonlyT
//    val structTT = structDefT.getRef
//    val structType2 = CoordT(ownership, permission, structTT)
//
//    val destructor2 =
//      ast.FunctionT(
//        ast.FunctionHeaderT(
//          env.fullName,
//          Vector.empty,
//          Vector(ast.ParameterT(interner.intern(CodeVarNameT("self")), None, structType2)),
//          CoordT(ShareT, VoidT()),
//          maybeOriginFunction1),
//        BlockTE(
//            ReturnTE(
//              FunctionCallTE(
//                structDestructor,
//                Vector(ArgLookupTE(0, structType2))))))
//
//    // If this fails, then the signature the FunctionCompilerMiddleLayer made for us doesn't
//    // match what we just made
//    vassert(
//      coutputs.getDeclaredSignatureOrigin(
//        destructor2.header.toSignature).nonEmpty)
//
//    // we cant make the destructor here because they might have a user defined one somewhere
//
//      coutputs
//        .declareFunctionReturnType(destructor2.header.toSignature, destructor2.header.returnType)
//      coutputs.addFunction(destructor2);
//
//    vassert(
//      coutputs.getDeclaredSignatureOrigin(
//        destructor2.header.toSignature).nonEmpty)
//
//    (destructor2.header)
//  }
}
