package dev.vale.typing

import dev.vale.postparsing.RegionTemplataType
import dev.vale.typing.ast.{ReferenceExpressionTE, TupleTE}
import dev.vale.{Interner, Keywords, Profiler, RangeS, vassert, vassertSome, vimpl}
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.env.{IInDenizenEnvironment, TemplataLookupContext}
import dev.vale.typing.names.{CitizenTemplateNameT, IRegionNameT, IdT, StructTemplateNameT}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.typing.ast._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing.citizen.StructCompilerCore
import dev.vale.typing.env.PackageEnvironment
import dev.vale.typing.function.FunctionCompiler

class SequenceCompiler(
  opts: TypingPassOptions,
  interner: Interner,
  keywords: Keywords,
    structCompiler: StructCompiler,
    templataCompiler: TemplataCompiler) {
  def resolveTuple(
    env: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    exprs2: Vector[ReferenceExpressionTE]):
  (ReferenceExpressionTE) = {
    val types2 = exprs2.map(_.result.expectReference().coord)
    val region = vimpl()
    val finalExpr = TupleTE(exprs2, makeTupleCoord(env, coutputs, parentRanges, region, types2))
    (finalExpr)
  }

  def makeTupleKind(
    env: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    types2: Vector[CoordT]):
  StructTT = {
    val tupleTemplate @ StructDefinitionTemplata(_, _) =
      vassertSome(
        env.lookupNearestWithName(
          interner.intern(StructTemplateNameT(keywords.tupleHumanName)), Set(TemplataLookupContext)))
    structCompiler.resolveStruct(
      coutputs,
      env,
      RangeS.internal(interner, -17653) :: parentRanges,
      tupleTemplate,
//      Vector(CoordListTemplata(types2))).kind
      types2.map(CoordTemplata),
      vimpl()).expect().kind
  }

  def makeTupleCoord(
    env: IInDenizenEnvironment,
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    region: ITemplata[RegionTemplataType],
    types2: Vector[CoordT]):
  CoordT = {
    templataCompiler.coerceKindToCoord(
      coutputs, makeTupleKind(env, coutputs, parentRanges, types2), region)
  }
}
