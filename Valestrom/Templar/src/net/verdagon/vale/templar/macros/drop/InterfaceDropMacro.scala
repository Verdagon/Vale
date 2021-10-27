package net.verdagon.vale.templar.macros.drop

import net.verdagon.vale.astronomer.{FunctionA, ImmInterfaceDestructorImpreciseNameS, InterfaceA}
import net.verdagon.vale.parser.{LendConstraintP, MoveP, ReadonlyP}
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{LookupSR, RuneUsage}
import net.verdagon.vale.templar.ast.PrototypeT
import net.verdagon.vale.templar.env.{FunctionEnvEntry, IEnvironment}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.macros.IOnInterfaceDefinedMacro
import net.verdagon.vale.templar.names.{FullNameT, FunctionTemplateNameT, INameT}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.{OverloadTemplar, Templar, Temputs}
import net.verdagon.vale.{CodeLocationS, RangeS, vassert}

class InterfaceDropMacro(overloadTemplar: OverloadTemplar) extends IOnInterfaceDefinedMacro {

  override def getInterfaceSiblingEntries(structName: FullNameT[INameT], interfaceA: InterfaceA): Vector[(INameT, FunctionEnvEntry)] = {
    Vector()
  }

  override def getInterfaceChildEntries(interfaceName: FullNameT[INameT], interfaceA: InterfaceA): Vector[(INameT, FunctionEnvEntry)] = {
    val dropFunctionA =
      FunctionA(
        RangeS.internal(-64),
        FunctionNameS(CallTemplar.DROP_FUNCTION_NAME, CodeLocationS.internal(-17)),
        Vector(UserFunctionS),
        TemplateTemplataType(Vector(CoordTemplataType), FunctionTemplataType),
        Vector(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))),
        Map(CodeRuneS("T") -> CoordTemplataType, CodeRuneS("V") -> CoordTemplataType),
        Vector(
          ParameterS(
            AtomSP(
              RangeS.internal(-1340),
              Some(CaptureS(CodeVarNameS("this"))),
              None,
              Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))), None))),
        Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("V"))),
        Vector(
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("T")),SelfNameS()),
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("V")),CodeNameS("void"))),
        CodeBodyS(
          BodySE(RangeS.internal(-167213),
            Vector(),
            BlockSE(RangeS.internal(-167213),
              Vector(LocalS(CodeVarNameS("this"), NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
              Vector(
                FunctionCallSE(RangeS.internal(-167213),
                  OutsideLoadSE(RangeS.internal(-167213),
                    Array(),
                    CallTemplar.VIRTUAL_DROP_FUNCTION_NAME,
                    None,
                    LendConstraintP(None)),
                  Vector(LocalLoadSE(RangeS.internal(-167213), CodeVarNameS("this"), MoveP))))))))

    val virtualDropFunctionA =
      FunctionA(
        RangeS.internal(-64),
        FunctionNameS(CallTemplar.VIRTUAL_DROP_FUNCTION_NAME, CodeLocationS.internal(-17)),
        Vector(UserFunctionS),
        TemplateTemplataType(Vector(CoordTemplataType), FunctionTemplataType),
        Vector(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))),
        Map(CodeRuneS("T") -> CoordTemplataType, CodeRuneS("V") -> CoordTemplataType),
        Vector(
          ParameterS(
            AtomSP(
              RangeS.internal(-1340),
              Some(CaptureS(CodeVarNameS("this"))),
              Some(AbstractSP),
              Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("T"))), None))),
        Some(RuneUsage(RangeS.internal(-64002), CodeRuneS("V"))),
        Vector(
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("T")),SelfNameS()),
          LookupSR(RangeS.internal(-167213),RuneUsage(RangeS.internal(-64002), CodeRuneS("V")),CodeNameS("void"))),
        GeneratedBodyS("abstractBody"))

    Vector(
      FunctionTemplateNameT(CallTemplar.DROP_FUNCTION_NAME, CodeLocationS.internal(-76)) ->
        FunctionEnvEntry(dropFunctionA),
      FunctionTemplateNameT(CallTemplar.VIRTUAL_DROP_FUNCTION_NAME, CodeLocationS.internal(-77)) ->
        FunctionEnvEntry(virtualDropFunctionA))
  }

  def getImmInterfaceDestructor(
    temputs: Temputs,
    env: IEnvironment,
    interfaceTT: InterfaceTT):
  PrototypeT = {
    vassert(Templar.getMutability(temputs, interfaceTT) == ImmutableT)

    val prototype =
      overloadTemplar.findFunction(
        env,
        temputs,
        RangeS.internal(-1677),
        ImmInterfaceDestructorImpreciseNameS(),
        Vector.empty,
        Array.empty,
        Vector(ParamFilter(CoordT(ShareT, ReadonlyT, interfaceTT), None)),
        Vector.empty,
        true)
    prototype
  }

}
