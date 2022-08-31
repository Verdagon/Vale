package dev.vale.typing.macros

import dev.vale.{Keywords, RangeS, StrI, vassert, vassertSome, vimpl}
import dev.vale.highertyping.FunctionA
import dev.vale.typing.{CompilerOutputs, TemplataCompiler, ast}
import dev.vale.typing.ast.{AbstractT, ArgLookupTE, BlockTE, FunctionHeaderT, FunctionT, InterfaceFunctionCallTE, LocationInFunctionEnvironment, ParameterT, ReturnTE}
import dev.vale.typing.env.FunctionEnvironment
import dev.vale.typing.types.CoordT
import dev.vale.typing.ast._
import dev.vale.typing.templata.FunctionTemplata

class AbstractBodyMacro(keywords: Keywords) extends IFunctionBodyMacro {
  val generatorId: StrI = keywords.abstractBody

  override def generateFunctionBody(
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    generatorId: StrI,
    life: LocationInFunctionEnvironment,
    callRange: List[RangeS],
    originFunction: Option[FunctionA],
    params2: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  (FunctionHeaderT, ReferenceExpressionTE) = {
    val returnReferenceType2 = vassertSome(maybeRetCoord)
    vassert(params2.exists(_.virtuality == Some(AbstractT())))
    val header =
      FunctionHeaderT(
        env.fullName,
        Vector.empty,
        params2,
        returnReferenceType2,
        originFunction.map(FunctionTemplata(env.parentEnv, _)))


//    val thisFuncBounds = vassertSome(coutputs.getInstantiationBounds(env.fullName))
//    coutputs.addInstantiationBounds(header.toPrototype.fullName, thisFuncBounds)
//
//    header.toPrototype.fullName

//    val runeToFunctionBound = TemplataCompiler.assembleFunctionBoundToRune(env.templatas)
//    coutputs.addInstantiationBounds(header.toPrototype.fullName, runeToFunctionBound)

    val body =
      BlockTE(
        ReturnTE(
          InterfaceFunctionCallTE(
            header,
            header.returnType,
            header.params.zipWithIndex.map({ case (param2, index) => ArgLookupTE(index, param2.tyype) }))))

    (header, body)
  }
}
