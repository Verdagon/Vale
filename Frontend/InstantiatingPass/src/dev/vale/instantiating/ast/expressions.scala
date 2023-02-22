package dev.vale.instantiating.ast

import dev.vale._
import dev.vale.postparsing._

trait IExpressionResulIT  {
  def expectReference(): ReferenceResulIT = {
    this match {
      case r @ ReferenceResulIT(_) => r
      case AddressResultI(_) => vfail("Expected a reference as a result, but got an address!")
    }
  }
  def expectAddress(): AddressResultI = {
    this match {
      case a @ AddressResultI(_) => a
      case ReferenceResulIT(_) => vfail("Expected an address as a result, but got a reference!")
    }
  }
  def underlyingCoord: CoordI
  def kind: KindIT
}
case class AddressResultI(coord: CoordI) extends IExpressionResulIT {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  override def underlyingCoord: CoordI = coord
  override def kind = coord.kind
}
case class ReferenceResulIT(coord: CoordI) extends IExpressionResulIT {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  override def underlyingCoord: CoordI = coord
  override def kind = coord.kind
}
trait ExpressionI  {
  def result: IExpressionResulIT
  def kind: KindIT
}
trait ReferenceExpressionIE extends ExpressionI {
  override def result: ReferenceResulIT
  override def kind = result.coord.kind
}
// This is an Expression2 because we sometimes take an address and throw it
// directly into a struct (closures!), which can have addressible members.
trait AddressExpressionIE extends ExpressionI {
  override def result: AddressResultI
  override def kind = result.coord.kind

  def range: RangeS

  // Whether or not we can change where this address points to
  def variability: VariabilityI
}

case class LetAndLendIE(
  variable: ILocalVariableI,
  expr: ReferenceExpressionIE,
  targetOwnership: OwnershipI
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vassert(variable.coord == expr.result.coord)

  (expr.result.coord.ownership, targetOwnership) match {
    case (MutableShareI, MutableShareI) =>
    case (ImmutableShareI, ImmutableShareI) =>
    case (OwnI | MutableBorrowI | WeakI, MutableBorrowI) =>
    case (ImmutableBorrowI, ImmutableBorrowI) =>
  }

  expr match {
    case BreakIE() | ReturnIE(_) => vwat() // See BRCOBS
    case _ =>
  }

  override def result: ReferenceResulIT = {
    val CoordI(oldOwnership, kind) = expr.result.coord
    ReferenceResulIT(CoordI(targetOwnership, kind))
  }
}

case class LockWeakIE(
  innerExpr: ReferenceExpressionIE,
  // We could just calculaIE this, but it feels better to let the StructCompiler
  // make it, so we're sure it's created.
  resultOptBorrowType: CoordI,

  // Function to give a borrow ref to to make a Some(borrow ref)
  someConstructor: PrototypeI,
  // Function to make a None of the right type
  noneConstructor: PrototypeI,

  // This is the impl we use to allow/permit the upcast from the some to the none.
  // It'll be useful for monomorphization and later on for locating the itable ptr to put in fat pointers.
  someImplName: IdI[IImplNameI],
  // This is the impl we use to allow/permit the upcast from the some to the none.
  // It'll be useful for monomorphization and later on for locating the itable ptr to put in fat pointers.
  noneImplName: IdI[IImplNameI],
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    ReferenceResulIT(resultOptBorrowType)
  }
}

// Turns a borrow ref into a weak ref
// NoIE that we can also get a weak ref from LocalLoad2'ing a
// borrow ref local into a weak ref.
case class BorrowToWeakIE(
  innerExpr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  vassert(
    innerExpr.result.coord.ownership == ImmutableBorrowI ||
      innerExpr.result.coord.ownership == MutableBorrowI)

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  innerExpr.result.coord.ownership match {
    case MutableBorrowI | ImmutableBorrowI =>
  }

  override def result: ReferenceResulIT = {
    ReferenceResulIT(CoordI(WeakI, innerExpr.kind))
  }
}

case class LetNormalIE(
  variable: ILocalVariableI,
  expr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
  }

  expr.kind match {
    case NeverIT(_) => // then we can put it into whatever type we want
    case _ => {
      variable.coord.kind match {
        case NeverIT(_) => vfail() // can't receive into a never
        case _ => vassert(variable.coord == expr.result.coord)
      }
    }
  }

  expr match {
    case BreakIE() | ReturnIE(_) => vwat() // See BRCOBS
    case _ =>
  }
}

// Only ExpressionCompiler.unletLocal should make these
case class UnletIE(variable: ILocalVariableI) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(variable.coord)

  vpass()
}

// Throws away a reference.
// Unless given to an instruction which consumes it, all borrow and share
// references must eventually hit a Discard2, just like all owning
// references must eventually hit a Destructure2.
// Depending on the backend, it will either be a no-op (like for GC'd backends)
// or a decrement+maybedestruct (like for RC'd backends)
// See DINSIE for why this isnt three instructions, and why we dont have the
// destructor in here for shareds.
case class DiscardIE(
  expr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
  }

  expr.result.coord.ownership match {
    case MutableBorrowI =>
    case ImmutableBorrowI =>
    case MutableShareI | ImmutableShareI =>
    case WeakI =>
  }

  expr match {
    case ConsecutorIE(exprs) => {
      exprs.last match {
        case DiscardIE(_) => vwat()
        case _ =>
      }
    }
    case _ =>
  }
}

case class DeferIE(
  innerExpr: ReferenceExpressionIE,
  // Every deferred expression should discard its result, IOW, return Void.
  deferredExpr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  override def result = ReferenceResulIT(innerExpr.result.coord)

  vassert(deferredExpr.result.coord == CoordI(MutableShareI, VoidIT()))
}


// Eventually, when we want to do if-let, we'll have a different construct
// entirely. See comment below If2.
// These are blocks because we don't want inner locals to escape.
case class IfIE(
  condition: ReferenceExpressionIE,
  thenCall: ReferenceExpressionIE,
  elseCall: ReferenceExpressionIE) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  private val conditionResultCoord = condition.result.coord
  private val thenResultCoord = thenCall.result.coord
  private val elseResultCoord = elseCall.result.coord

  conditionResultCoord match {
    case CoordI(MutableShareI | ImmutableShareI, BoolIT()) =>
    case other => vfail(other)
  }

  (thenResultCoord.kind, thenResultCoord.kind) match {
    case (NeverIT(_), _) =>
    case (_, NeverIT(_)) =>
    case (a, b) if a == b =>
    case _ => vwat()
  }

  private val commonSupertype =
    thenResultCoord.kind match {
      case NeverIT(_) => elseResultCoord
      case _ => thenResultCoord
    }

  override def result = ReferenceResulIT(commonSupertype)
}

// The block is expected to return a boolean (false = stop, true = keep going).
// The block will probably contain an If2(the condition, the body, false)
case class WhileIE(block: BlockIE) extends ReferenceExpressionIE {
  // While loops must always produce void.
  // If we want a foreach/map/whatever construct, the loop should instead
  // add things to a list inside; WhileIE shouldnt do it for it.
  val resultCoord =
  block.result.coord match {
    case CoordI(_, VoidIT()) => block.result.coord
    case CoordI(_, NeverIT(true)) => CoordI(MutableShareI, VoidIT())
    case CoordI(_, NeverIT(false)) => block.result.coord
    case _ => vwat()
  }

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(resultCoord)
  vpass()
}

case class MutateIE(
  destinationExpr: AddressExpressionIE,
  sourceExpr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(destinationExpr.result.coord)
}


case class ReturnIE(
  sourceExpr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    ReferenceResulIT(CoordI(MutableShareI, NeverIT(false)))
  }
}

case class BreakIE() extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    ReferenceResulIT(CoordI(MutableShareI, NeverIT(true)))
  }
}

// when we make a closure, we make a struct full of pointers to all our variables
// and the first element is our parent closure
// this can live on the stack, since blocks are limited to this expression
// later we can optimize it to only have the things we use

// Block2 is required to unlet all the variables it introduces.
case class BlockIE(
  inner: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  vpass()

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = inner.result
}

// A pure block will:
// 1. CreaIE a new region (someday possibly with an allocator)
// 2. Freeze the existing region
// 3. Run the inner code
// 4. Un-freeze the existing region
// 5. Merge (transmigraIE) any results from the new region into the existing region
// 6. Destroy the new region
case class MutabilifyIE(
  inner: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  vpass()

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    val innerCoord = inner.result.coord
    val newOwnership =
      innerCoord.ownership match {
        case ImmutableShareI => MutableShareI
        case ImmutableBorrowI => MutableBorrowI
        case MutableShareI => vwat()
        case MutableBorrowI => vwat()
        case OwnI => vwat()
        case WeakI => vimpl()
      }
    ReferenceResulIT(innerCoord.copy(ownership = newOwnership))
  }
}

case class ConsecutorIE(exprs: Vector[ReferenceExpressionIE]) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  // There shouldn't be a 0-element consecutor.
  // If we want a consecutor that returns nothing, put a VoidLiteralIE in it.
  vassert(exprs.nonEmpty)

  if (exprs.size > 1) {
    vassert(exprs.init.collect({ case VoidLiteralIE() => }).isEmpty)
  }

  // There shouldn't be a 1-element consecutor.
  // This isn't a hard technical requirement, but it does simplify the resulting ASI a bit.
  // Call Compiler.consecutive to conform to this.
  vassert(exprs.size >= 2)

  // A consecutor should never contain another consecutor.
  // This isn't a hard technical requirement, but it does simplify the resulting ASI a bit.
  // Call Compiler.consecutive to make new consecutors in a way that conforms to this.
  exprs.collect({ case ConsecutorIE(_) => vfail() })

  // Everything but the last should result in a Void or a Never.
  // The last can be anything, even a Void or a Never.
  exprs.init.foreach(expr => {
    expr.kind match {
      case VoidIT() | NeverIT(_) =>
      case _ => vwat()
    }
  })

  //  // If there's a Never2() anywhere, then the entire block should end in an unreachable
  //  // or panic or something.
  //  if (exprs.exists(_.kind == NeverI())) {
  //    vassert(exprs.last.kind == NeverI())
  //  }
  // Nevermind, we made it so the consecutor's result is Never if there's
  // a Never *anywhere* inside it.

  vassert(exprs.collect({
    case ReturnIE(_) =>
  }).size <= 1)

  override val result: ReferenceResulIT =
    exprs.map(_.result.coord)
      .collectFirst({ case n @ CoordI(MutableShareI, NeverIT(_)) => n }) match {
      case Some(n) => ReferenceResulIT(n)
      case None => exprs.last.result
    }

  def lastReferenceExpr = exprs.last
}

case class TupleIE(
  elements: Vector[ReferenceExpressionIE],
  resultReference: CoordI) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(resultReference)
}

//// Discards a reference, whether it be owned or borrow or whatever.
//// This is used after panics or other never-returning things, to signal that a certain
//// variable should be considered gone. See AUMAP.
//// This can also be used if theres anything after a panic in a block, like
////   exported func main() int {
////     __panic();
////     println("hi");
////   }
//case class UnreachableMootIE(innerExpr: ReferenceExpressionIE) extends ReferenceExpressionIE {
//  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
//  override def result = ReferenceResulIT(CoordI(MutableShareI, NeverI()))
//}

case class StaticArrayFromValuesIE(
  elements: Vector[ReferenceExpressionIE],
  resultReference: CoordI,
  arrayType: StaticSizedArrayIT,
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(resultReference)
}

case class ArraySizeIE(array: ReferenceExpressionIE) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(CoordI(MutableShareI, IntIT.i32))
}

// Can we do an === of objects in two regions? It could be pretty useful.
case class IsSameInstanceIE(left: ReferenceExpressionIE, right: ReferenceExpressionIE) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vassert(left.result.coord == right.result.coord)

  override def result = ReferenceResulIT(CoordI(MutableShareI, BoolIT()))
}

case class AsSubtypeIE(
  sourceExpr: ReferenceExpressionIE,
  targetType: CoordI,

  // We could just calculaIE this, but it feels better to let the StructCompiler
  // make it, so we're sure it's created.
  resultResultType: CoordI,
  // Function to give a borrow ref to to make a Some(borrow ref)
  okConstructor: PrototypeI,
  // Function to make a None of the right type
  errConstructor: PrototypeI,

  // This is the impl we use to allow/permit the downcast. It'll be useful for monomorphization.
  implName: IdI[IImplNameI],

  // These are the impls that we conceptually use to upcast the created Ok/Err to Result.
  // Really they're here so the instantiator can know what impls it needs to instantiaIE.
  okImplName: IdI[IImplNameI],
  errImplName: IdI[IImplNameI],
) extends ReferenceExpressionIE {
  vpass()

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(resultResultType)
}

case class VoidLiteralIE() extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
  }
}

case class ConstantIntIE(value: Long, bits: Int) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    ReferenceResulIT(CoordI(MutableShareI, IntIT(bits)))
  }
}

case class ConstantBoolIE(value: Boolean) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    ReferenceResulIT(CoordI(MutableShareI, BoolIT()))
  }
}

case class ConstantStrIE(value: String) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    ReferenceResulIT(CoordI(MutableShareI, StrIT()))
  }
}

case class ConstantFloatIE(value: Double) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(CoordI(MutableShareI, FloatIT()))
}

case class LocalLookupIE(
  range: RangeS,
  // This is the local variable at the time it was created
  localVariable: ILocalVariableI,
  // The instantiator might want to load this as a different region mutability than the mutability
  // when originally created, so tihs field will be able to hold that.
  // Conceptually, it's the current mutability of the source region at the time of the local lookup.
  pureHeight: Int,
  //  reference: CoordI,
  //  variability: VariabilityI
) extends AddressExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: AddressResultI = {
//    val CoordI(localVarOwnership, localVarRegion, kind) = localVariable.coord
//    AddressResultI(
//      CoordI((localVarOwnership, localVarPureHeight == pureHeight) match {
//        case (OwnI, _) => OwnI
//        case other => vimpl(other)
//      }, localVarRegion, kind)
    vimpl()
  }
  override def variability: VariabilityI = localVariable.variability
}

case class ArgLookupIE(
  paramIndex: Int,
  coord: CoordI
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = ReferenceResulIT(coord)
}

case class StaticSizedArrayLookupIE(
  range: RangeS,
  arrayExpr: ReferenceExpressionIE,
  indexExpr: ReferenceExpressionIE,
  // See RMLRMO for why this is the same ownership as the original field.
  elementType: CoordI,
  // See RMLRMO for why we dont have a targetOwnership field here.
  variability: VariabilityI
) extends AddressExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  override def result = {
    // See RMLRMO why we just return the element type.
    AddressResultI(elementType)
  }
}

case class RuntimeSizedArrayLookupIE(
  range: RangeS,
  arrayExpr: ReferenceExpressionIE,
  arrayType: RuntimeSizedArrayIT,
  indexExpr: ReferenceExpressionIE,
  // See RMLRMO for why we dont have a targetOwnership field here.
  variability: VariabilityI
) extends AddressExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vassert(arrayExpr.result.coord.kind == arrayType)

  override def result = {
    AddressResultI(arrayType.elementType)
  }
}

case class ArrayLengthIE(arrayExpr: ReferenceExpressionIE) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    ReferenceResulIT(CoordI(MutableShareI, IntIT.i32))
  }
}

case class ReferenceMemberLookupIE(
  range: RangeS,
  structExpr: ReferenceExpressionIE,
  memberName: IVarNameI,
  // See RMLRMO for why this is the same ownership as the original field.
  memberReference: CoordI,
  // See RMLRMO for why we dont have a targetOwnership field here.
  variability: VariabilityI) extends AddressExpressionIE {
  vpass()

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = {
    // See RMLRMO why we just return the member type.
    AddressResultI(memberReference)
  }
}
case class AddressMemberLookupIE(
  range: RangeS,
  structExpr: ReferenceExpressionIE,
  memberName: IVarNameI,
  resultType2: CoordI,
  variability: VariabilityI) extends AddressExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result = AddressResultI(resultType2)
}

case class InterfaceFunctionCallIE(
  superFunctionPrototype: PrototypeI,
  virtualParamIndex: Int,
  resultReference: CoordI,
  args: Vector[ReferenceExpressionIE]) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = ReferenceResulIT(resultReference)
}

case class ExternFunctionCallIE(
  prototype2: PrototypeI,
  args: Vector[ReferenceExpressionIE]) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  // We dont:
  //   vassert(prototype2.fullName.last.templateArgs.isEmpty)
  // because we totally can have extern templates.
  // Will one day be useful for plugins, and we already use it for
  // lock<T>, which is generated by the backend.

  prototype2.id.localName match {
    case ExternFunctionNameI(_, _) =>
    case _ => vwat()
  }



  override def result = ReferenceResulIT(prototype2.returnType)
}

case class FunctionCallIE(
  callable: PrototypeI,
  args: Vector[ReferenceExpressionIE]
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  vassert(callable.paramTypes.size == args.size)
  args.map(_.result.coord).zip(callable.paramTypes).foreach({
    case (CoordI(_, NeverIT(_)), _) =>
    case (a, b) => vassert(a == b)
  })

  override def result: ReferenceResulIT = {
    ReferenceResulIT(callable.returnType)
  }
}

// A typingpass reinterpret is interpreting a type as a different one which is hammer-equivalent.
// For example, a pack and a struct are the same thing to hammer.
// Also, a closure and a struct are the same thing to hammer.
// But, Compiler attaches different meanings to these things. The typingpass is free to reinterpret
// between hammer-equivalent things as it wants.
case class ReinterpretIE(
  expr: ReferenceExpressionIE,
  resultReference: CoordI) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vassert(expr.result.coord != resultReference)

  override def result = ReferenceResulIT(resultReference)

  expr.result.coord.kind match {
    // Unless it's a Never...
    case NeverIT(_) =>
    case _ => {
      if (resultReference.ownership != expr.result.coord.ownership) {
        // Cant reinterpret to a different ownership!
        vfail("wat");
      }
    }
  }
}

case class ConstructIE(
  structTT: StructIT,
  resultReference: CoordI,
  args: Vector[ExpressionI],
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vpass()

  override def result = ReferenceResulIT(resultReference)
}

// NoIE: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
case class NewMutRuntimeSizedArrayIE(
  arrayType: RuntimeSizedArrayIT,
  capacityExpr: ReferenceExpressionIE,
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    ReferenceResulIT(
      CoordI(
        arrayType.mutability match {
          case MutableI => OwnI
          case ImmutableI => MutableShareI
        },
        arrayType))
  }
}

case class StaticArrayFromCallableIE(
  arrayType: StaticSizedArrayIT,
  generator: ReferenceExpressionIE,
  generatorMethod: PrototypeI,
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    ReferenceResulIT(
      CoordI(
        arrayType.mutability match {
          case MutableI => OwnI
          case ImmutableI => MutableShareI
        },
        arrayType))
  }
}

// NoIE: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
// This returns nothing, as opposed to DrainStaticSizedArray2 which returns a
// sequence of results from the call.
case class DestroyStaticSizedArrayIntoFunctionIE(
  arrayExpr: ReferenceExpressionIE,
  arrayType: StaticSizedArrayIT,
  consumer: ReferenceExpressionIE,
  consumerMethod: PrototypeI) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vassert(consumerMethod.paramTypes.size == 2)
  vassert(consumerMethod.paramTypes(0) == consumer.result.coord)
  vassert(consumerMethod.paramTypes(1) == arrayType.elementType)

  // See https://github.com/ValeLang/Vale/issues/375
  consumerMethod.returnType.kind match {
    case StructIT(IdI(_, _, StructNameI(StructTemplateNameI(name), _))) => {
      vassert(name.str == "Tup")
    }
    case VoidIT() =>
    case _ => vwat()
  }

  override def result: ReferenceResulIT = ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
}

// We destroy both Share and Own things
// If the struct contains any addressibles, those die immediately and aren't stored
// in the destination variables, which is why it's a list of ReferenceLocalVariable2.
case class DestroyStaticSizedArrayIntoLocalsIE(
  expr: ReferenceExpressionIE,
  staticSizedArray: StaticSizedArrayIT,
  destinationReferenceVariables: Vector[ReferenceLocalVariableI]
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = ReferenceResulIT(CoordI(MutableShareI, VoidIT()))

  vassert(expr.kind == staticSizedArray)
}

case class DestroyMutRuntimeSizedArrayIE(
  arrayExpr: ReferenceExpressionIE,
) extends ReferenceExpressionIE {
  override def result: ReferenceResulIT = {
    ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
  }
}

case class RuntimeSizedArrayCapacityIE(
  arrayExpr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  override def result: ReferenceResulIT = ReferenceResulIT(CoordI(MutableShareI, IntIT(32)))
}

case class PushRuntimeSizedArrayIE(
  arrayExpr: ReferenceExpressionIE,
  //  arrayType: RuntimeSizedArrayIT,
  newElementExpr: ReferenceExpressionIE,
  //  newElementType: CoordI,
) extends ReferenceExpressionIE {
  override def result: ReferenceResulIT = ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
}

case class PopRuntimeSizedArrayIE(
  arrayExpr: ReferenceExpressionIE
) extends ReferenceExpressionIE {
  private val elementType =
    arrayExpr.result.coord.kind match {
      case contentsRuntimeSizedArrayIT(_, e, _) => e
      case other => vwat(other)
    }
  override def result: ReferenceResulIT = ReferenceResulIT(elementType)
}

case class InterfaceToInterfaceUpcastIE(
  innerExpr: ReferenceExpressionIE,
  targetInterface: InterfaceIT) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  def result: ReferenceResulIT = {
    ReferenceResulIT(
      CoordI(
        innerExpr.result.coord.ownership,
        targetInterface))
  }
}

// This used to be StructToInterfaceUpcastIE, and then we added generics.
// Now, it could be that we're upcasting a placeholder to an interface, or a
// placeholder to another placeholder. For all we know, this'll eventually be
// upcasting an int to an int.
// So, the target kind can be anything, not just an interface.
case class UpcastIE(
  innerExpr: ReferenceExpressionIE,
  targetSuperKind: ISuperKindIT,
  // This is the impl we use to allow/permit the upcast. It'll be useful for monomorphization
  // and later on for locating the itable ptr to put in fat pointers.
  implName: IdI[IImplNameI],
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  def result: ReferenceResulIT = {
    ReferenceResulIT(
      CoordI(
        innerExpr.result.coord.ownership,
        targetSuperKind))
  }
}

// A soft load is one that turns an int&& into an int*. a hard load turns an int* into an int.
// Turns an Addressible(Pointer) into an OwningPointer. Makes the source owning pointer into null

// If the source was an own and target is borrow, that's a point

case class SoftLoadIE(
  expr: AddressExpressionIE,
  targetOwnership: OwnershipI
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  vassert((targetOwnership == MutableShareI) == (expr.result.coord.ownership == MutableShareI))
  vassert((targetOwnership == ImmutableShareI) == (expr.result.coord.ownership == ImmutableShareI))
  vassert(targetOwnership != OwnI) // need to unstackify or destroy to get an owning reference
  // This is just here to try the asserts inside Coord's constructor
  CoordI(targetOwnership, expr.result.coord.kind)

  override def result: ReferenceResulIT = {
    ReferenceResulIT(CoordI(targetOwnership, expr.result.coord.kind))
  }
}

// Destroy an object.
// If the struct contains any addressibles, those die immediately and aren't stored
// in the destination variables, which is why it's a list of ReferenceLocalVariable2.
//
// We also destroy shared things with this, see DDSOT.
case class DestroyIE(
  expr: ReferenceExpressionIE,
  structTT: StructIT,
  destinationReferenceVariables: Vector[ReferenceLocalVariableI]
) extends ReferenceExpressionIE {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
  }
}

case class DestroyImmRuntimeSizedArrayIE(
  arrayExpr: ReferenceExpressionIE,
  arrayType: RuntimeSizedArrayIT,
  consumer: ReferenceExpressionIE,
  consumerMethod: PrototypeI,
) extends ReferenceExpressionIE {
  arrayType.mutability match {
    case ImmutableI =>
    case _ => vwat()
  }

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  vassert(consumerMethod.paramTypes.size == 2)
  vassert(consumerMethod.paramTypes(0) == consumer.result.coord)
  //  vassert(consumerMethod.paramTypes(1) == Program2.intType)
  vassert(consumerMethod.paramTypes(1) == arrayType.elementType)

  // See https://github.com/ValeLang/Vale/issues/375
  consumerMethod.returnType.kind match {
    case VoidIT() =>
  }

  override def result: ReferenceResulIT = ReferenceResulIT(CoordI(MutableShareI, VoidIT()))
}

// NoIE: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
case class NewImmRuntimeSizedArrayIE(
  arrayType: RuntimeSizedArrayIT,
  sizeExpr: ReferenceExpressionIE,
  generator: ReferenceExpressionIE,
  generatorMethod: PrototypeI,
) extends ReferenceExpressionIE {
  arrayType.mutability match {
    case ImmutableI =>
    case _ => vwat()
  }
  // We dont want to own the generator
  generator.result.coord.ownership match {
    case MutableBorrowI | ImmutableBorrowI | ImmutableShareI | MutableShareI =>
    case other => vwat(other)
  }
  generatorMethod.returnType.ownership match {
    case ImmutableShareI | MutableShareI =>
    case other => vwat(other)
  }

  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()
  override def result: ReferenceResulIT = {
    ReferenceResulIT(
      CoordI(
        arrayType.mutability match {
          case MutableI => OwnI
          case ImmutableI => MutableShareI
        },
        arrayType))
  }
}

object referenceExprResultStructName {
  def unapply(expr: ReferenceExpressionIE): Option[StrI] = {
    expr.result.coord.kind match {
      case StructIT(IdI(_, _, StructNameI(StructTemplateNameI(name), _))) => Some(name)
      case _ => None
    }
  }
}

object referenceExprResultKind {
  def unapply(expr: ReferenceExpressionIE): Option[KindIT] = {
    Some(expr.result.coord.kind)
  }
}
