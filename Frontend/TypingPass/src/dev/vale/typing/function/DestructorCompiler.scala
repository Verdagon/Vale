package dev.vale.typing.function


import dev.vale.postparsing._
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.expression.CallCompiler
import dev.vale.{Err, Interner, Keywords, Ok, PackageCoordinate, RangeS, vassert, vfail, vimpl}
import dev.vale.highertyping._
import dev.vale.postparsing.patterns._
import dev.vale.postparsing.rules.OwnershipLiteralSL
import dev.vale.postparsing.GlobalFunctionFamilyNameS
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.{CompileErrorExceptionT, CompilerOutputs, CouldntFindFunctionToCallT, OverloadResolver, RangedInternalErrorT, TypingPassOptions}
import dev.vale.typing.ast.{DiscardTE, FunctionCallTE, PrototypeT, ReferenceExpressionTE}
import dev.vale.typing.env.{GlobalEnvironment, IEnvironment, PackageEnvironment}
import dev.vale.typing.names.{FullNameT, PackageTopLevelNameT}
import dev.vale.typing.types._
import dev.vale.typing.{ast, _}
import dev.vale.typing.ast._
import dev.vale.typing.env._
import dev.vale.typing.function.FunctionCompiler.EvaluateFunctionSuccess
import dev.vale.typing.names.PackageTopLevelNameT

import scala.collection.immutable.List

class DestructorCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    structCompiler: StructCompiler,
    overloadCompiler: OverloadResolver) {
  def getDropFunction(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    type2: CoordT):
  EvaluateFunctionSuccess = {
    val name = interner.intern(CodeNameS(keywords.drop))
    val args = Vector(type2)
    overloadCompiler.findFunction(
      env, coutputs, callRange, name, Vector.empty, Array.empty, args, Vector(), true, true) match {
      case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(callRange, e))
      case Ok(x) => x
    }
  }

  def drop(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    undestructedExpr2: ReferenceExpressionTE):
  (ReferenceExpressionTE) = {
    val resultExpr2 =
      undestructedExpr2.result.reference match {
        case r@CoordT(OwnT, _) => {
          val destructorPrototype = getDropFunction(env, coutputs, callRange, r)
          vassert(coutputs.getInstantiationBounds(destructorPrototype.function.prototype.fullName).nonEmpty)
          FunctionCallTE(destructorPrototype.function.prototype, Vector(undestructedExpr2))
        }
        case CoordT(BorrowT, _) => (DiscardTE(undestructedExpr2))
        case CoordT(WeakT, _) => (DiscardTE(undestructedExpr2))
        case CoordT(ShareT, _) => {
          val destroySharedCitizenOrPlaceholder =
            (coutputs: CompilerOutputs, coord: CoordT) => {
              // DO NOT SUBMIT
//              val destructorHeader =
//                getDropFunction(env, coutputs, callRange, coord)
//              // We just needed to ensure it's in the coutputs, so that the backend can use it
//              // for when reference counts drop to zero.
//              // If/when we have a GC backend, we can skip generating share destructors.
//              val _ = destructorHeader
              DiscardTE(undestructedExpr2)
            };
          val destroySharedArray =
            (coutputs: CompilerOutputs, coord: CoordT) => {
              val destructorHeader = getDropFunction(env, coutputs, callRange, coord)
              // We just needed to ensure it's in the coutputs, so that the backend can use it
              // for when reference counts drop to zero.
              // If/when we have a GC backend, we can skip generating share destructors.
              val _ = destructorHeader
              DiscardTE(undestructedExpr2)
            };


          val unshareExpr2 =
            undestructedExpr2.result.reference.kind match {
              case NeverT(_) => undestructedExpr2
              case IntT(_) | StrT() | BoolT() | FloatT() | VoidT() => {
                DiscardTE(undestructedExpr2)
              }
              case OverloadSetT(overloadSetEnv, name) => {
                DiscardTE(undestructedExpr2)
              }
              case as@StaticSizedArrayTT(_) => {
                val underarrayReference2 =
                  CoordT(
                    undestructedExpr2.result.reference.ownership,
                    as)
                destroySharedArray(coutputs, underarrayReference2)
              }
              case as@RuntimeSizedArrayTT(_) => {
                val underarrayReference2 =
                  CoordT(
                    undestructedExpr2.result.reference.ownership,
                    as)
                destroySharedArray(coutputs, underarrayReference2)
              }
              case StructTT(_) | InterfaceTT(_) | PlaceholderT(_) => {
                destroySharedCitizenOrPlaceholder(coutputs, undestructedExpr2.result.reference)
              }
              case other => vfail("Unknown type to drop: " + other)
            }
          unshareExpr2
        }
      }
    resultExpr2.result.reference.kind match {
      case VoidT() | NeverT(_) =>
      case _ => {
        throw CompileErrorExceptionT(
          RangedInternalErrorT(
            callRange,
            "Unexpected return type for drop autocall.\nReturn: " + resultExpr2.result.reference.kind + "\nParam: " + undestructedExpr2.result.reference))
      }
    }
    resultExpr2
  }
}
