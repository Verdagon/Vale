package dev.vale.simplifying

import dev.vale.instantiating.ast._

// Projects a UFCS-flat function id into Rust's "args-on-the-type" shape.
//
// The typing pass produces ids where the citizen step in initSteps is the instantiated
// form (StructNameI(template, [args])) — its templateArgs come from the struct's
// instantiation context. The function's leaf step also carries those parent args,
// since FunctionScout prepends extraGenericParamsFromParentS to the function's own
// genericParameters.
//
// For Rust output, we want each generic arg to appear at exactly one structural
// level. The convention is: citizen-introduced args live on the citizen step; the
// function's own args live on the leaf. So we strip the leading "parent-inherited"
// args from the leaf — the count is the sum of templateArgs across all citizen
// steps in initSteps.
//
//   In:  rust.vec.Vec<i32>::with_capacity<i32>(i64) → SimpleId after this drops
//        the leaf's i32 → renders as Vec<i32>::with_capacity(i64).
//
//   Free function (no citizen step in initSteps) → no drop, renders unchanged.
object RustShapeProjector {
  def projectFunctionId(id: IdI[cI, IFunctionNameI[cI]]): IdI[cI, IFunctionNameI[cI]] = {
    val IdI(packageCoord, initSteps, leaf) = id
    val parentArgCount =
      initSteps.collect({ case c: ICitizenNameI[cI] => c.templateArgs.size }).sum
    if (parentArgCount == 0) {
      id
    } else {
      val newLeaf = stripLeafTemplateArgs(leaf, parentArgCount)
      IdI(packageCoord, initSteps, newLeaf)
    }
  }

  private def stripLeafTemplateArgs(leaf: IFunctionNameI[cI], n: Int): IFunctionNameI[cI] = {
    leaf match {
      case FunctionNameIX(template, templateArgs, parameters) =>
        FunctionNameIX(template, templateArgs.drop(n), parameters)
      case ExternFunctionNameI(humanName, templateArgs, parameters) =>
        ExternFunctionNameI(humanName, templateArgs.drop(n), parameters)
      case other => other
    }
  }
}
