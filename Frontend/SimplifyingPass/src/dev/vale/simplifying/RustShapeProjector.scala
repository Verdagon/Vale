package dev.vale.simplifying

import dev.vale.instantiating.ast._

// Projects a UFCS-flat function id into Rust's "args-on-the-type" shape.
//
// The post-rollback typing pass produces ids where:
//   - Each citizen step in initSteps is in *template* form (no per-step args), e.g.
//     `StructTemplateNameI(Vec)`.
//   - The function's leaf step carries the full chain of template args concatenated,
//     in order: parent-citizen-1 args, ..., parent-citizen-n args, function's own args.
//     This is because FunctionScout prepends `extraGenericParamsFromParentS` to each
//     function's `genericParameters`, and the callsite's explicit-container-template-args
//     channel feeds them positionally.
//
// For Rust output, we want each generic arg to appear at exactly one structural level:
// each citizen template's args should ride on its own step (e.g. `Vec<i32>`, not
// `Vec::with_capacity::<i32>`). So we walk initSteps left-to-right, look up each citizen
// template's arity, peel that many args off the front of the leaf's templateArgs, and
// rewrite the citizen step from `StructTemplateNameI(template)` to
// `StructNameI(template, [peeled args])`. Whatever remains on the leaf is the function's
// own template args.
//
//   In:  [StructTemplateNameI(Vec), ExternFunctionNameI(with_capacity, [i32], [i64])]
//        → After projection:
//        [StructNameI(Vec, [i32]), ExternFunctionNameI(with_capacity, [], [i64])]
//        Renders as `Vec<i32>::with_capacity(i64)` for Rust output.
//
//   In:  [ExternFunctionNameI(free_fn, [i32], [])]  (no parent citizen step)
//        → No change. Renders as `free_fn<i32>()`.
object RustShapeProjector {
  def projectFunctionId(hinputs: HinputsI, id: IdI[cI, IFunctionNameI[cI]]): IdI[cI, IFunctionNameI[cI]] = {
    val IdI(packageCoord, initSteps, leaf) = id

    // Build a lookup from citizen template id → arity. Every instantiation in hinputs
    // contributes the same arity for its template (one entry per template wins).
    val templateArity: Map[ICitizenTemplateNameI[cI], Int] =
      (hinputs.structs.map(s => {
        val name = s.instantiatedCitizen.id.localName
        (name.template: ICitizenTemplateNameI[cI]) -> name.templateArgs.size
      }) ++ hinputs.interfaces.map(i => {
        val name = i.instantiatedCitizen.id.localName
        (name.template: ICitizenTemplateNameI[cI]) -> name.templateArgs.size
      })).toMap

    val leafTemplateArgs = templateArgsOf(leaf)

    // Walk initSteps; for each bare-template citizen step, peel that many args off
    // the front of the remaining leaf args and rebuild the step in instantiated form.
    var remaining = leafTemplateArgs
    val newInitSteps = initSteps.map(step => projectStep(step, templateArity, remaining) match {
      case (newStep, leftover) =>
        remaining = leftover
        newStep
    })

    val newLeaf = setTemplateArgs(leaf, remaining)
    IdI(packageCoord, newInitSteps, newLeaf)
  }

  private def projectStep(
    step: INameI[cI],
    templateArity: Map[ICitizenTemplateNameI[cI], Int],
    remainingLeafArgs: Vector[ITemplataI[cI]]
  ): (INameI[cI], Vector[ITemplataI[cI]]) = {
    step match {
      case t: IStructTemplateNameI[cI] =>
        val arity = templateArity.getOrElse(t, 0)
        val (consumed, leftover) = remainingLeafArgs.splitAt(arity)
        (StructNameI(t, consumed), leftover)
      case t: IInterfaceTemplateNameI[cI] =>
        val arity = templateArity.getOrElse(t, 0)
        val (consumed, leftover) = remainingLeafArgs.splitAt(arity)
        (InterfaceNameI(t, consumed), leftover)
      case other => (other, remainingLeafArgs)
    }
  }

  private def templateArgsOf(leaf: IFunctionNameI[cI]): Vector[ITemplataI[cI]] = {
    leaf match {
      case FunctionNameIX(_, templateArgs, _) => templateArgs
      case ExternFunctionNameI(_, templateArgs, _) => templateArgs
      case _ => Vector.empty
    }
  }

  private def setTemplateArgs(leaf: IFunctionNameI[cI], templateArgs: Vector[ITemplataI[cI]]): IFunctionNameI[cI] = {
    leaf match {
      case FunctionNameIX(template, _, parameters) => FunctionNameIX(template, templateArgs, parameters)
      case ExternFunctionNameI(humanName, _, parameters) => ExternFunctionNameI(humanName, templateArgs, parameters)
      case other => other
    }
  }
}
