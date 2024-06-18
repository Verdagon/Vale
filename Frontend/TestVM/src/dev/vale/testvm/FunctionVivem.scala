package dev.vale.testvm

import dev.vale.finalast.{FunctionH, ProgramH, PrototypeH}
import dev.vale.testvm.ExpressionVivem.NodeReturn
import dev.vale.finalast._
import ExpressionVivem.{NodeBreak, NodeContinue, NodeReturn}
import dev.vale.{vimpl, vwat}
import dev.vale.{finalast => m}

object FunctionVivem {
  def executeFunction(
      programH: ProgramH,
      stdin: (() => String),
      stdout: (String => Unit),
      heap: Heap,
      args: Vector[ReferenceV],
      functionH: FunctionH
  ): (CallId, NodeReturn) = {
    val callId = heap.pushNewStackFrame(functionH.prototype, args)

    heap.vivemDout.print("  " * callId.callDepth + "Entering function " + callId)

    // Increment all the args to show that they have arguments referring to them.
    // These will be decremented at some point in the callee function.
    args.indices.foreach(argIndex => {
      heap.incrementReferenceRefCount(
        ArgumentToObjectReferrer(ArgumentId(callId, argIndex), args(argIndex).ownership),
        args(argIndex))
    })

    heap.vivemDout.println()

    val rootExpressionId = ExpressionId(callId, Vector.empty)
    val returnRef =
      ExpressionVivem.executeNode(programH, stdin, stdout, heap, rootExpressionId, functionH.body) match {
        case NodeReturn(r) => NodeReturn(r)
        case NodeBreak() => vwat()
        case NodeContinue(r) => NodeReturn(r)
      }

    heap.vivemDout.println()
    heap.vivemDout.print("  " * callId.callDepth + "Returning")

    heap.popStackFrame(callId)

    heap.vivemDout.println()

    (callId, returnRef)
  }

  def getExternFunction(programH: ProgramH, ref: PrototypeH): (AdapterForExterns, Vector[ReferenceV]) => ReferenceV = {
    ref.id.fullyQualifiedName
      // The tests have a mode where they can interpret the builtins as separate packages, instead
      // of pulling it all in as one giant namespace. In that case, it prefixes things such as
      // v::builtins::arith. We can add other prefixes here too as needed.
      .replaceAllLiterally("v::builtins::arith", "") match {
      case """__vbi_addI32""" => VivemExterns.addI32(_, ref, _)
      case """__vbi_addFloatFloat""" => VivemExterns.addFloatFloat(_, ref, _)
      case """__vbi_panic""" => VivemExterns.panic(_, ref, _)
      case """__vbi_multiplyI32""" => VivemExterns.multiplyI32(_, ref, _)
      case """__vbi_subtractFloatFloat""" => VivemExterns.subtractFloatFloat(_, ref, _)
      case """__vbi_divideI32""" => VivemExterns.divideI32(_, ref, _)
      case """__vbi_multiplyFloatFloat""" => VivemExterns.multiplyFloatFloat(_, ref, _)
      case """__vbi_divideFloatFloat""" => VivemExterns.divideFloatFloat(_, ref, _)
      case """__vbi_subtractI32""" => VivemExterns.subtractI32(_, ref, _)
      case """addStr""" => VivemExterns.addStrStr(_, ref, _)
      case """__getch""" => VivemExterns.getch(_, ref, _)
      case """__vbi_eqFloatFloat""" => VivemExterns.eqFloatFloat(_, ref, _)
      case """sqrt""" => VivemExterns.sqrt(_, ref, _)
      case """__vbi_lessThanI32""" => VivemExterns.lessThanI32(_, ref, _)
      case """__vbi_lessThanFloat""" => VivemExterns.lessThanFloat(_, ref, _)
      case """__vbi_greaterThanOrEqI32""" => VivemExterns.greaterThanOrEqI32(_, ref, _)
      case """__vbi_greaterThanI32""" => VivemExterns.greaterThanI32(_, ref, _)
      case """__vbi_eqI32""" => VivemExterns.eqI32(_, ref, _)
      case """__vbi_eqBoolBool""" => VivemExterns.eqBoolBool(_, ref, _)
      case """printstr""" => VivemExterns.print(_, ref, _)
      case """__vbi_not""" => VivemExterns.not(_, ref, _)
      case """castI32Str""" => VivemExterns.castI32Str(_, ref, _)
      case """castI64Str""" => VivemExterns.castI64Str(_, ref, _)
      case """castI32Float""" => VivemExterns.castI32Float(_, ref, _)
      case """castFloatI32""" => VivemExterns.castFloatI32(_, ref, _)
      case """__vbi_lessThanOrEqI32""" => VivemExterns.lessThanOrEqI32(_, ref, _)
      case """__vbi_and""" => VivemExterns.and(_, ref, _)
      case """__vbi_or""" => VivemExterns.or(_, ref, _)
      case """__vbi_modI32""" => VivemExterns.modI32(_, ref, _)
      case """__vbi_strLength""" => VivemExterns.strLength(_, ref, _)
      case """castFloatStr""" => VivemExterns.castFloatStr(_, ref, _)
      case """streq""" => VivemExterns.eqStrStr(_, ref, _)
      case """__vbi_negateFloat""" => VivemExterns.negateFloat(_, ref, _)
      case """__vbi_addI64""" => VivemExterns.addI64(_, ref, _)
      case """__vbi_multiplyI64""" => VivemExterns.multiplyI64(_, ref, _)
      case """__vbi_divideI64""" => VivemExterns.divideI64(_, ref, _)
      case """__vbi_subtractI64""" => VivemExterns.subtractI64(_, ref, _)
      case """__vbi_lessThanI64""" => VivemExterns.lessThanI64(_, ref, _)
      case """__vbi_greaterThanOrEqI64""" => VivemExterns.greaterThanOrEqI64(_, ref, _)
      case """__vbi_eqI64""" => VivemExterns.eqI64(_, ref, _)
      case """__vbi_castI64Str""" => VivemExterns.castI64Str(_, ref, _)
      case """__vbi_castI64Float""" => VivemExterns.castI64Float(_, ref, _)
      case """__vbi_castFloatI64""" => VivemExterns.castFloatI64(_, ref, _)
      case """__vbi_lessThanOrEqI64""" => VivemExterns.lessThanOrEqI64(_, ref, _)
      case """__vbi_modI64""" => VivemExterns.modI64(_, ref, _)
      case """TruncateI64ToI32""" => VivemExterns.truncateI64ToI32(_, ref, _)
      case """VecOuterNew""" => VivemExterns.newVec(_, ref, _)
      case """Vec<i32>.new""" => VivemExterns.newVec(_, ref, _)
      case """Vec<i32>.with_capacity""" => VivemExterns.newVecWithCapacity(_, ref, _)
      case """Vec<i32>.capacity""" => VivemExterns.vecCapacity(_, ref, _)
      case _ => vimpl(ref.id.fullyQualifiedName)
    }
  }
}
