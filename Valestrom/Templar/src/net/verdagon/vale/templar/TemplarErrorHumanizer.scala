package net.verdagon.vale.templar

import net.verdagon.vale.SourceCodeUtils.{humanizePos, lineContaining, lineRangeContaining}
import net.verdagon.vale.astronomer.{AstronomerErrorHumanizer, ConstructorNameS, FunctionA, ImmConcreteDestructorNameS, ImmDropNameS, ImmInterfaceDestructorNameS}
import net.verdagon.vale.scout.ScoutErrorHumanizer.humanizeRune
import net.verdagon.vale.scout.rules.{IRulexSR, RuneUsage}
import net.verdagon.vale.scout.{CodeRuneS, CodeVarNameS, FunctionNameS, GlobalFunctionFamilyNameS, INameS, IRuneS, ImplicitRuneS, LambdaNameS, SenderRuneS, TopLevelCitizenDeclarationNameS}
import net.verdagon.vale.solver.{FailedSolve, IIncompleteOrFailedSolve, IncompleteSolve, RuleError, SolverConflict, SolverErrorHumanizer}
import net.verdagon.vale.templar.OverloadTemplar.{IScoutExpectedFunctionFailureReason, InferFailure, Outscored, ScoutExpectedFunctionFailure, SpecificParamDoesntMatch, SpecificParamVirtualityDoesntMatch, WrongNumberOfArguments, WrongNumberOfTemplateArguments}
import net.verdagon.vale.templar.infer.{CallResultWasntExpectedType, ITemplarSolverError, KindIsNotConcrete, KindIsNotInterface}
import net.verdagon.vale.templar.templata.{CoordTemplata, FunctionBannerT, IPotentialBanner, ITemplata, InterfaceTemplata, KindTemplata, MutabilityTemplata, OwnershipTemplata, PrototypeT, PrototypeTemplata, RuntimeSizedArrayTemplateTemplata, StaticSizedArrayTemplateTemplata, StructTemplata, VariabilityTemplata}
import net.verdagon.vale.templar.types.{BoolT, ConstraintT, CoordT, FinalT, FloatT, ImmutableT, IntT, InterfaceTT, KindT, MutableT, OwnT, ParamFilter, RawArrayTT, ReadonlyT, ReadwriteT, RuntimeSizedArrayTT, ShareT, StrT, StructTT, VaryingT, VoidT, WeakT}
import net.verdagon.vale.{FileCoordinate, FileCoordinateMap, RangeS, repeatStr, vimpl}

object TemplarErrorHumanizer {
  def humanize(
      verbose: Boolean,
      codeMap: FileCoordinateMap[String],
      err: ICompileErrorT):
  String = {
    val errorStrBody =
      err match {
        case RangedInternalErrorT(range, message) => { " " + message
        }
        case CantUseReadonlyReferenceAsReadwrite(range) => {
            ": Can't make readonly reference into a readwrite one!"
        }
        case CantMoveOutOfMemberT(range, name) => {
            ": Cannot move out of member (" + name + ")"
        }
        case CantMutateFinalMember(range, fullName, memberName) => {
            ": Cannot mutate final member '" + printableVarName(memberName.last) + "' of container " + printableFullName(fullName)
        }
        case CantMutateFinalElement(range, fullName) => {
            ": Cannot change a slot in array " + printableFullName(fullName) + " to point to a different element; it's an array of final references."
        }
        case LambdaReturnDoesntMatchInterfaceConstructor(range) => {
            ": Argument function return type doesn't match interface method param"
        }
        case CantUseUnstackifiedLocal(range, name) => {
            ": Can't use local that was already moved (" + name + ")"
        }
        case CantUnstackifyOutsideLocalFromInsideWhile(range, name) => {
            ": Can't move a local (" + name + ") from inside a while loop."
        }
        case CannotSubscriptT(range, tyype) => {
            ": Cannot subscript type: " + tyype + "!"
        }
        case CouldntConvertForReturnT(range, expectedType, actualType) => {
            ": Couldn't convert " + actualType + " to expected return type " + expectedType
        }
        case CouldntConvertForMutateT(range, expectedType, actualType) => {
            ": Mutate couldn't convert " + actualType + " to expected destination type " + expectedType
        }
        case CouldntFindMemberT(range, memberName) => {
            ": Couldn't find member " + memberName + "!"
        }
        case BodyResultDoesntMatch(range, functionName, expectedReturnType, resultType) => {
            ": Function " + printableName(codeMap, functionName) + " return type " + expectedReturnType + " doesn't match body's result: " + resultType
        }
        case CouldntFindIdentifierToLoadT(range, name) => {
            ": Couldn't find anything named `" + name + "`!"
        }
        case NonReadonlyReferenceFoundInPureFunctionParameter(range, name) => {
            ": Parameter `" + name + "` should be readonly, because it's in a pure function."
        }
        case CouldntFindTypeT(range, name) => {
            ": Couldn't find any type named `" + name + "`!"
        }
        case ImmStructCantHaveVaryingMember(range, structName, memberName) => {
            ": Immutable struct (\"" + printableName(codeMap, structName) + "\") cannot have varying member (\"" + memberName + "\")."
        }
        case CantDowncastUnrelatedTypes(range, sourceKind, targetKind) => {
            ": Can't downcast `" + sourceKind + "` to unrelated `" + targetKind + "`"
        }
        case CantDowncastToInterface(range, targetKind) => {
            ": Can't downcast to an interface (" + targetKind + ") yet."
        }
        case ArrayElementsHaveDifferentTypes(range, types) => {
            ": Array's elements have different types: " + types.mkString(", ")
        }
        case ExportedFunctionDependedOnNonExportedKind(range, paackage, signature, nonExportedKind) => {
          ": Exported function " + signature + " depends on kind " + nonExportedKind + " that wasn't exported from package " + paackage
        }
        case TypeExportedMultipleTimes(range, paackage, exports) => {
          ": Type exported multiple times:" + exports.map(export => {
            val posStr = humanizePos(codeMap, export.range.file, export.range.begin.offset)
            val line = lineContaining(codeMap, export.range.file, export.range.begin.offset)
            s"\n  ${posStr}: ${line}"
          })
        }
        case ExternFunctionDependedOnNonExportedKind(range, paackage, signature, nonExportedKind) => {
          ": Extern function " + signature + " depends on kind " + nonExportedKind + " that wasn't exported from package " + paackage
        }
        case ExportedImmutableKindDependedOnNonExportedKind(range, paackage, signature, nonExportedKind) => {
          ": Exported kind " + signature + " depends on kind " + nonExportedKind + " that wasn't exported from package " + paackage
        }
        case InitializedWrongNumberOfElements(range, expectedNumElements, numElementsInitialized) => {
            ": Supplied " + numElementsInitialized + " elements, but expected " + expectedNumElements + "."
        }
        case CouldntFindFunctionToCallT(range, ScoutExpectedFunctionFailure(name, args, outscoredReasonByPotentialBanner, rejectedReasonByBanner, rejectedReasonByFunction)) => {
            ": Couldn't find a suitable function named `" +
            (name match {
              case GlobalFunctionFamilyNameS(humanName) => humanName
              case other => other.toString
            }) +
            "` with args (" +
            (if (args.collectFirst({ case ParamFilter(tyype, Some(_)) => }).nonEmpty) {
              vimpl()
            } else {
              args.map({ case ParamFilter(tyype, None) => TemplataNamer.getReferenceIdentifierName(tyype) }).mkString(", ")
            }) +
            "):\n" + lineContaining(codeMap, range.file, range.end.offset) + "\n" +
            (if (outscoredReasonByPotentialBanner.size + rejectedReasonByBanner.size + rejectedReasonByFunction.size == 0) {
              "No function with that name exists.\nPerhaps you forget to include a file in the command line?\n"
            } else {
              (if (outscoredReasonByPotentialBanner.nonEmpty) {
                "Outscored candidates:\n" + outscoredReasonByPotentialBanner.map({
                  case (potentialBanner, outscoredReason) => {
                    "  " + TemplataNamer.getFullNameIdentifierName(potentialBanner.banner.fullName) + ":\n" +
                      humanizeRejectionReason(
                        verbose,
                        codeMap,
                        range,
                        outscoredReason)
                  }
                }).mkString("\n")
              } else {
                ""
              }) + "\n" +
                (if (rejectedReasonByBanner.size + rejectedReasonByFunction.size > 0) {
                  "Rejected candidates:\n" +
                    (rejectedReasonByBanner.map({
                      case (banner, rejectedReason) => {
                        "  " + humanizeBanner(codeMap, banner) + "\n" +
                          humanizeRejectionReason(verbose, codeMap, range, rejectedReason)
                      }
                    }) ++
                      rejectedReasonByFunction.map({
                        case (functionA, rejectedReason) => {
                          "  " + printableName(codeMap, functionA.name) + ":\n" +
                            humanizeRejectionReason(verbose, codeMap, range, rejectedReason)
                        }
                      })).mkString("\n") + "\n"
                } else {
                  ""
                })
            })
        }
        case FunctionAlreadyExists(oldFunctionRange, newFunctionRange, signature) => {
            ": Function " + signature.fullName.last + " already exists! Previous declaration at:\n" +
            humanizePos(codeMap, oldFunctionRange.file, oldFunctionRange.begin.offset)
        }
        case IfConditionIsntBoolean(range, actualType) => {
            ": If condition should be a bool, but was: " + actualType
        }
        case WhileConditionIsntBoolean(range, actualType) => {
            ": If condition should be a bool, but was: " + actualType
        }
        case CantImplStruct(range, struct) => {
            ": Can't extend a struct: (" + struct + ")"
        }
//        case NotEnoughToSolveError(range, conclusions, unknownRunes) => {
//          humanizeFailedSolve(codeMap, range, IncompleteSolve(conclusions, unknownRunes.toSet))
////          ": Couldn't solve unknowns: " + unknownRunes.toVector.sortBy({ case CodeRuneS(_) => 0 case _ => 1 }) + " but do know:\n" + conclusions.map({ case (k, v) => "  " + k + ": " + v + "\n" }).mkString("")
//        }
        case TemplarSolverError(range, failedSolve) => {
          humanizeFailedSolve(codeMap, range, failedSolve)
        }
        case InferAstronomerError(range, err) => {
          AstronomerErrorHumanizer.humanize(codeMap, range, err)
        }
      }

    val posStr = humanizePos(codeMap, err.range.file, err.range.begin.offset)
    val nextStuff = lineContaining(codeMap, err.range.file, err.range.begin.offset)
    val errorId = "T"
    f"${posStr} error ${errorId}: ${errorStrBody}\n${nextStuff}\n"
  }

  def humanizeBanner(
    codeMap: FileCoordinateMap[String],
    banner: FunctionBannerT):
  String = {
    banner.originFunction match {
      case None => "(internal)"
      case Some(x) => printableName(codeMap, x.name)
    }
  }

  private def printableName(
    codeMap: FileCoordinateMap[String],
    name: INameS):
  String = {
    name match {
      case CodeVarNameS(name) => name
      case TopLevelCitizenDeclarationNameS(name, codeLocation) => name
      case LambdaNameS(codeLocation) => humanizePos(codeMap, codeLocation.file, codeLocation.offset) + ": " + "(lambda)"
      case FunctionNameS(name, codeLocation) => humanizePos(codeMap, codeLocation.file, codeLocation.offset) + ": " + name
      case ConstructorNameS(TopLevelCitizenDeclarationNameS(name, range)) => humanizePos(codeMap, range.file, range.begin.offset) + ": " + name
      case ImmConcreteDestructorNameS(_) => vimpl()
      case ImmInterfaceDestructorNameS(_) => vimpl()
      case ImmDropNameS(_) => vimpl()
    }
  }

  private def printableCoordName(coord: CoordT): String = {
    val CoordT(ownership, permission, kind) = coord
    (ownership match {
      case ShareT => ""
      case OwnT => ""
      case ConstraintT => "&"
      case WeakT => "&&"
    }) +
    (permission match {
      case ReadonlyT => ""
      case ReadwriteT => "!"
    }) +
    printableKindName(kind)
  }

  private def printableKindName(kind: KindT): String = {
    kind match {
      case IntT(bits) => "i" + bits
      case BoolT() => "bool"
      case FloatT() => "float"
      case StrT() => "str"
      case StructTT(f) => printableFullName(f)
    }
  }
  private def printableFullName(fullName2: FullNameT[INameT]): String = {
    fullName2.last match {
      case CitizenNameT(humanName, templateArgs) => humanName + (if (templateArgs.isEmpty) "" else "<" + templateArgs.map(_.toString.mkString) + ">")
      case x => x.toString
    }
  }

  private def printableVarName(
    name: IVarNameT):
  String = {
    name match {
      case CodeVarNameT(n) => n
    }
  }

//  private def getFile(potentialBanner: IPotentialBanner): FileCoordinate = {
//    getFile(potentialBanner.banner)
//  }

//  private def getFile(banner: FunctionBanner2): FileCoordinate = {
//    banner.originFunction.map(getFile).getOrElse(FileCoordinate.internal(-76))
//  }

  private def getFile(functionA: FunctionA): FileCoordinate = {
    functionA.range.file
  }

  private def humanizeRejectionReason(
      verbose: Boolean,
      codeMap: FileCoordinateMap[String],
      invocationRange: RangeS,
      reason: IScoutExpectedFunctionFailureReason): String = {
    reason match {
      case WrongNumberOfArguments(supplied, expected) => {
        "Number of params doesn't match! Supplied " + supplied + " but function takes " + expected
      }
      case WrongNumberOfTemplateArguments(supplied, expected) => {
        "Number of template params doesn't match! Supplied " + supplied + " but function takes " + expected
      }
      case SpecificParamDoesntMatch(index, reason) => "Param at index " + index + " doesn't match: " + reason
      case SpecificParamVirtualityDoesntMatch(index) => "Virtualities don't match at index " + index
      case Outscored() => "Outscored!"
      case InferFailure(reason) => {
        if (verbose) {
            "Failed to infer:\n" +
            humanizeFailedSolve(codeMap, invocationRange, reason)
        } else {
          "(run with --verbose to see some incomprehensible details)"
        }
      }
    }
  }

  def humanizeRuleError(
    codeMap: FileCoordinateMap[String],
    error: ITemplarSolverError
  ): String = {
    error match {
      case KindIsNotConcrete(kind) => {
        "Expected kind to be concrete, but was not. Kind: " + kind
      }
      case KindIsNotInterface(kind) => {
        "Expected kind to be interface, but was not. Kind: " + kind
      }
      case CallResultWasntExpectedType(expected, actual) => {
        "Expected an instantiation of " + humanizeTemplata(codeMap, expected) + " but got " + humanizeTemplata(codeMap, actual)
      }
    }
  }

  def humanizeFailedSolve(
    codeMap: FileCoordinateMap[String],
    invocationRange: RangeS,
    result: IIncompleteOrFailedSolve[IRulexSR, IRuneS, ITemplata, ITemplarSolverError]):
  String = {
    SolverErrorHumanizer.humanizeFailedSolve(
      codeMap,
      humanizeRune,
      humanizeTemplata,
      humanizeRuleError,
      (rule: IRulexSR) => rule.range,
      (rule: IRulexSR) => rule.runeUsages.map(usage => (usage.rune, usage.range)),
      result)
  }

  def humanizeTemplata(
    codeMap: FileCoordinateMap[String],
    templata: ITemplata):
  String = {
    templata match {
      case RuntimeSizedArrayTemplateTemplata() => "[*]"
      case StaticSizedArrayTemplateTemplata() => "[]"
      case InterfaceTemplata(env, originInterface) => originInterface.name.name
      case StructTemplata(env, originStruct) => originStruct.name.name
      case VariabilityTemplata(variability) => {
        variability match {
          case FinalT => "final"
          case VaryingT => "vary"
        }
      }
      case MutabilityTemplata(mutability) => {
        mutability match {
          case MutableT => "mut"
          case ImmutableT => "imm"
        }
      }
      case OwnershipTemplata(ownership) => {
        ownership match {
          case OwnT => "own"
          case ConstraintT => "constraint"
          case WeakT => "weak"
          case ShareT => "share"
        }
      }
      case PrototypeTemplata(PrototypeT(name, returnType)) => {
        humanizeName(codeMap, name)
      }
      case CoordTemplata(CoordT(ownership, permission, kind)) => {
        (ownership match {
          case OwnT => ""
          case ShareT => ""
          case ConstraintT => {
            (permission match {
              case ReadonlyT => "&"
              case ReadwriteT => "&!"
            })
          }
          case WeakT => {
            (permission match {
              case ReadonlyT => "&&"
              case ReadwriteT => "&&!"
            })
          }
        }) +
          humanizeTemplata(codeMap, KindTemplata(kind))
      }
      case KindTemplata(kind) => {
        kind match {
          case IntT(bits) => "i" + bits
          case BoolT() => "bool"
          case StrT() => "str"
          case VoidT() => "void"
          case InterfaceTT(name) => humanizeName(codeMap, name)
          case StructTT(name) => humanizeName(codeMap, name)
          case RuntimeSizedArrayTT(RawArrayTT(elementType, mutability, variability)) => {
            "Array<" +
              humanizeTemplata(codeMap, MutabilityTemplata(mutability)) + ", " +
              humanizeTemplata(codeMap, VariabilityTemplata(variability)) + ", " +
              humanizeTemplata(codeMap, CoordTemplata(elementType)) + ">"
          }
        }
      }
      case other => vimpl(other)
    }
  }

  def humanizeName[T <: INameT](
    codeMap: FileCoordinateMap[String],
    name: FullNameT[T]):
  String = {
    name.last match {
      case LambdaCitizenNameT(codeLocation) => {
        "λ:" + humanizePos(codeMap, codeLocation.file, codeLocation.offset)
      }
      case FunctionNameT(humanName, templateArgs, parameters) => {
        humanName +
          (if (templateArgs.nonEmpty) {
            "<" + templateArgs.map(humanizeTemplata(codeMap, _)).mkString(", ") + ">"
          } else {
            ""
          }) +
          (if (parameters.nonEmpty) {
            "(" + parameters.map(CoordTemplata).map(humanizeTemplata(codeMap, _)).mkString(", ") + ")"
          } else {
            ""
          })
      }
      case CitizenNameT(humanName, templateArgs) => {
        humanName +
          (if (templateArgs.nonEmpty) {
            "<" + templateArgs.map(humanizeTemplata(codeMap, _)).mkString(", ") + ">"
          } else {
            ""
          })
      }
    }
  }
}
