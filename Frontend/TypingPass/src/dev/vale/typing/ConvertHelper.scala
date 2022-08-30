package dev.vale.typing

import dev.vale.typing.ast.ReferenceExpressionTE
import dev.vale.typing.env.{GlobalEnvironment, IEnvironment}
import dev.vale.{RangeS, vcurious, vfail}
import dev.vale.typing.types._
import dev.vale._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.{IsParent, IsParentResult, IsntParent}
import dev.vale.typing.function.FunctionCompiler.EvaluateFunctionSuccess
//import dev.vale.astronomer.IRulexSR
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.env.IEnvironmentBox
import dev.vale.typing.templata._
import dev.vale.typing.types._

import scala.collection.immutable.List
//import dev.vale.carpenter.CovarianceCarpenter
import dev.vale.postparsing._

trait IConvertHelperDelegate {
  def isParent(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment,
    parentRanges: List[RangeS],
    descendantCitizenRef: ISubKindTT,
    ancestorInterfaceRef: ISuperKindTT):
  IsParentResult

  def getFreeFunction(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment,
    callRange: List[RangeS],
    type2: CoordT):
  EvaluateFunctionSuccess
}

class ConvertHelper(
    opts: TypingPassOptions,
    delegate: IConvertHelperDelegate) {
  def convertExprs(
      env: IEnvironment,
      coutputs: CompilerOutputs,
      range: List[RangeS],
      sourceExprs: Vector[ReferenceExpressionTE],
      targetPointerTypes: Vector[CoordT]):
  (Vector[ReferenceExpressionTE]) = {
    if (sourceExprs.size != targetPointerTypes.size) {
      throw CompileErrorExceptionT(RangedInternalErrorT(range, "num exprs mismatch, source:\n" + sourceExprs + "\ntarget:\n" + targetPointerTypes))
    }
    (sourceExprs zip targetPointerTypes).foldLeft((Vector[ReferenceExpressionTE]()))({
      case ((previousRefExprs), (sourceExpr, targetPointerType)) => {
        val refExpr =
          convert(env, coutputs, range, sourceExpr, targetPointerType)
        (previousRefExprs :+ refExpr)
      }
    })
  }

  def convert(
      env: IEnvironment,
      coutputs: CompilerOutputs,
      range: List[RangeS],
      sourceExpr: ReferenceExpressionTE,
      targetPointerType: CoordT):
  (ReferenceExpressionTE) = {
    val sourcePointerType = sourceExpr.result.reference

    if (sourceExpr.result.reference == targetPointerType) {
      return sourceExpr
    }

    sourceExpr.result.reference.kind match {
      case NeverT(_) => return sourceExpr
      case _ =>
    }

    val CoordT(targetOwnership, targetType) = targetPointerType;
    val CoordT(sourceOwnership, sourceType) = sourcePointerType;

    targetPointerType.kind match {
      case NeverT(_) => vcurious()
      case _ =>
    }

    // We make the hammer aware of nevers.
//    if (sourceType == Never2()) {
//      return (CompilerReinterpret2(sourceExpr, targetPointerType))
//    }

    (sourceOwnership, targetOwnership) match {
      case (OwnT, OwnT) =>
      case (BorrowT, OwnT) => {
        throw CompileErrorExceptionT(RangedInternalErrorT(range, "Supplied a borrow but target wants to own the argument"))
      }
      case (OwnT, BorrowT) => {
        throw CompileErrorExceptionT(RangedInternalErrorT(range, "Supplied an owning but target wants to only borrow"))
      }
      case (BorrowT, BorrowT) =>
      case (ShareT, ShareT) =>
      case (WeakT, WeakT) =>
      case _ => throw CompileErrorExceptionT(RangedInternalErrorT(range, "Supplied a " + sourceOwnership + " but target wants " + targetOwnership))
    }

    val sourceExprConverted =
      if (sourceType == targetType) {
        sourceExpr
      } else {
        (sourceType, targetType) match {
          case (s : ISubKindTT, i : ISuperKindTT) => {
            convert(env, coutputs, range, sourceExpr, s, i)
          }
          case _ => vfail()
        }
      };

    (sourceExprConverted)
  }

  def convert(
    callingEnv: IEnvironment,
    coutputs: CompilerOutputs,
    range: List[RangeS],
    sourceExpr: ReferenceExpressionTE,
    sourceSubKind: ISubKindTT,
    targetSuperKind: ISuperKindTT):
  (ReferenceExpressionTE) = {
    delegate.isParent(coutputs, callingEnv, range, sourceSubKind, targetSuperKind) match {
      case IsParent(_, _, implFullName) => {
        val ownership =
          sourceExpr.result.reference.ownership match {
            case BorrowT => OwnT
            case OwnT => OwnT
            case WeakT => OwnT
            case ShareT => ShareT
          }
        // Thisll still exist for mutable things, itll just contain a no-op.
        val freeInterfacePrototype =
          delegate.getFreeFunction(
            coutputs, callingEnv, range, CoordT(ownership, targetSuperKind))

        vassert(coutputs.getInstantiationBounds(implFullName).nonEmpty)
        UpcastTE(
          sourceExpr, targetSuperKind, implFullName, freeInterfacePrototype.function.prototype)
      }
      case IsntParent(candidates) => {
        throw CompileErrorExceptionT(RangedInternalErrorT(range, "Can't upcast a " + sourceSubKind + " to a " + targetSuperKind + ": " + candidates))
      }
    }
  }
}