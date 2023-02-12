package dev.vale.simplifying

import dev.vale.finalast.{BlockH, MutabilifyH, NeverHT}
import dev.vale.typing.Hinputs
import dev.vale.typing.ast.{BlockTE, FunctionHeaderT}
import dev.vale.{vcurious, vfail, vimpl, finalast => m}
import dev.vale.typing.ast._
import dev.vale.typing.templata.RegionTemplata
import dev.vale.typing.types.IntT

class BlockHammer(expressionHammer: ExpressionHammer) {
  def translateBlock(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    currentFunctionHeader: FunctionHeaderT,
    parentLocals: LocalsBox,
    block2: BlockTE):
  (BlockH) = {
    val blockLocals = LocalsBox(parentLocals.snapshot)

    val exprH =
      expressionHammer.translateExpressionsAndDeferreds(
        hinputs, hamuts, currentFunctionHeader, blockLocals, Vector(block2.inner));

    // We dont vassert(deferreds.isEmpty) here, see BMHD for why.

    val localIdsInThisBlock = blockLocals.locals.keys.toSet.diff(parentLocals.locals.keys.toSet)
//    val localsInThisBlock = localIdsInThisBlock.map(blockLocals.locals)
    val unstackifiedLocalIdsInThisBlock = blockLocals.unstackifiedVars.intersect(localIdsInThisBlock)

    if (localIdsInThisBlock != unstackifiedLocalIdsInThisBlock) {
      exprH.resultType.kind match {
        case NeverHT(_) => {
          // Then it's forgivable, because the block never reaches its end.
        }
        case _ => {
          // This probably means that there was no UnletH or DestructureH for that variable.
          vfail("Ununstackified local: " + (localIdsInThisBlock -- unstackifiedLocalIdsInThisBlock))
        }
      }
    }

    // All the parent locals...
    val unstackifiedLocalsFromParent =
      parentLocals.locals.keySet
        // ...minus the ones that were unstackified before...
        .diff(parentLocals.unstackifiedVars)
        // ...which were unstackified by the block.
        .intersect(blockLocals.unstackifiedVars)
    unstackifiedLocalsFromParent.foreach(parentLocals.markUnstackified)
    parentLocals.setNextLocalIdNumber(blockLocals.nextLocalIdNumber)

//    start here, we're returning locals and thats not optimal
    BlockH(exprH)
  }

  def translatePure(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    currentFunctionHeader: FunctionHeaderT,
    locals: LocalsBox,
    node: PureTE):
  MutabilifyH = {
    val PureTE(_, transmigrateResultToRegion, innerTE) = node
    vcurious(transmigrateResultToRegion == RegionTemplata(true))

    val innerHE =
      expressionHammer.translateExpressionsAndDeferreds(
        hinputs, hamuts, currentFunctionHeader, locals, Vector(innerTE));

    innerTE.result.coord.kind match {
      case IntT(bits) => {
        MutabilifyH(innerHE)
      }
      case other => vimpl(other)
    }
  }
}
