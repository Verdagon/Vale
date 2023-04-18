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

    ref.id.toFullString()
      // The tests have a mode where they can interpret the builtins as separate packages, instead
      // of pulling it all in as one giant namespace. In that case, it prefixes things such as
      // v::builtins::arith. We can add other prefixes here too as needed.
      .replaceAllLiterally("v::builtins::arith", "")
      .replaceAllLiterally("v::StrI(builtins)::StrI(arith)", "")
      .replaceAllLiterally("v::StrI(builtins)::StrI(str)", "")
      .replaceAllLiterally("v::StrI(builtins)::StrI(streq)", "")
      .replaceAllLiterally("v::StrI(builtins)::StrI(print)", "") match {
      case """::EF("__vbi_addI32")""" => VivemExterns.addI32
      case """::EF("__vbi_addFloatFloat")""" => VivemExterns.addFloatFloat
      case """::EF("__vbi_panic")""" => VivemExterns.panic
      case """::EF("__vbi_multiplyI32")""" => VivemExterns.multiplyI32
      case """::EF("__vbi_subtractFloatFloat")""" => VivemExterns.subtractFloatFloat
      case """::EF("__vbi_divideI32")""" => VivemExterns.divideI32
      case """::EF("__vbi_multiplyFloatFloat")""" => VivemExterns.multiplyFloatFloat
      case """::EF("__vbi_divideFloatFloat")""" => VivemExterns.divideFloatFloat
      case """::EF("__vbi_subtractI32")""" => VivemExterns.subtractI32
      case """::EF("addStr")""" => VivemExterns.addStrStr
      case """ioutils::F("__getch")""" => VivemExterns.getch
      case """::EF("__vbi_eqFloatFloat")""" => VivemExterns.eqFloatFloat
      case """math::F("sqrt",[®],[R(@,<,f)])""" => VivemExterns.sqrt
      case """::EF("__vbi_lessThanI32")""" => VivemExterns.lessThanI32
      case """::EF("__vbi_lessThanFloat")""" => VivemExterns.lessThanFloat
      case """::EF("__vbi_greaterThanOrEqI32")""" => VivemExterns.greaterThanOrEqI32
      case """::EF("__vbi_greaterThanI32")""" => VivemExterns.greaterThanI32
      case """::EF("__vbi_eqI32")""" => VivemExterns.eqI32
      case """::EF("__vbi_eqBoolBool")""" => VivemExterns.eqBoolBool
      case """::EF("printstr")""" => VivemExterns.print
      case """::EF("__vbi_not")""" => VivemExterns.not
      case """::EF("castI32Str")""" => VivemExterns.castI32Str
      case """::EF("castI64Str")""" => VivemExterns.castI64Str
      case """::EF("castI32Float")""" => VivemExterns.castI32Float
      case """::EF("castFloatI32")""" => VivemExterns.castFloatI32
      case """::EF("__vbi_lessThanOrEqI32")""" => VivemExterns.lessThanOrEqI32
      case """::EF("__vbi_and")""" => VivemExterns.and
      case """::EF("__vbi_or")""" => VivemExterns.or
      case """::EF("__vbi_modI32")""" => VivemExterns.modI32
      case """::EF("__vbi_strLength")""" => VivemExterns.strLength
      case """::EF("castFloatStr")""" => VivemExterns.castFloatStr
      case """::EF("streq")""" => VivemExterns.eqStrStr
      case """::EF("__vbi_negateFloat")""" => VivemExterns.negateFloat
      case """::EF("__vbi_addI64")""" => VivemExterns.addI64
      case """::EF("__vbi_multiplyI64")""" => VivemExterns.multiplyI64
      case """::EF("__vbi_divideI64")""" => VivemExterns.divideI64
      case """::EF("__vbi_subtractI64")""" => VivemExterns.subtractI64
      case """::EF("__vbi_lessThanI64")""" => VivemExterns.lessThanI64
      case """::EF("__vbi_greaterThanOrEqI64")""" => VivemExterns.greaterThanOrEqI64
      case """::EF("__vbi_eqI64")""" => VivemExterns.eqI64
      case """::EF("__vbi_castI64Str")""" => VivemExterns.castI64Str
      case """::EF("__vbi_castI64Float")""" => VivemExterns.castI64Float
      case """::EF("__vbi_castFloatI64")""" => VivemExterns.castFloatI64
      case """::EF("__vbi_lessThanOrEqI64")""" => VivemExterns.lessThanOrEqI64
      case """::EF("__vbi_modI64")""" => VivemExterns.modI64
      case """::EF("TruncateI64ToI32")""" => VivemExterns.truncateI64ToI32

      case _ => vimpl(ref.id.toFullString())
    }
  }
}
