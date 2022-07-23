package dev.vale.typing.macros.citizen

import dev.vale.highertyping.{FunctionA, InterfaceA}
import dev.vale.postparsing.patterns.{AbstractSP, AtomSP, CaptureS}
import dev.vale.postparsing.rules.{CallSR, IRulexSR, LookupSR, RuneUsage}
import dev.vale.{Accumulator, Interner, Keywords, RangeS, StrI}
import dev.vale.postparsing._
import dev.vale.typing.ast.PrototypeT
import dev.vale.typing.env.{FunctionEnvEntry, IEnvEntry}
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.macros.IOnInterfaceDefinedMacro
import dev.vale.typing.names.{FullNameT, INameT, NameTranslator}
import dev.vale.typing.types.MutabilityT
import dev.vale.highertyping.FunctionA
import dev.vale.parsing.ast.MoveP
import dev.vale.postparsing._
import dev.vale.postparsing.patterns.AbstractSP
import dev.vale.typing.env.IEnvironment
import dev.vale.typing.names.FunctionTemplateNameT
import dev.vale.typing.types._
import dev.vale.typing.OverloadResolver

import scala.collection.mutable

class InterfaceDropMacro(
  interner: Interner,
  keywords: Keywords,
  nameTranslator: NameTranslator
) extends IOnInterfaceDefinedMacro {

  val macroName: StrI = keywords.DeriveInterfaceDrop

  override def getInterfaceSiblingEntries(interfaceName: FullNameT[INameT], interfaceA: InterfaceA): Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
    def range(n: Int) = RangeS.internal(interner, n)
    def use(n: Int, rune: IRuneS) = RuneUsage(range(n), rune)

    val v = CodeRuneS(keywords.V)
    val t = CodeRuneS(keywords.T)
    val self = CodeRuneS(keywords.self)

    val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
    runeToType.put(t, CoordTemplataType())
    runeToType.put(v, CoordTemplataType())

    val rules = new Accumulator[IRulexSR]()
    rules.add(LookupSR(range(-1672147),use(-64002, v),interner.intern(CodeNameS(keywords.void))))

    val interfaceNameRune = StructNameRuneS(interfaceA.name)
    runeToType += (interfaceNameRune -> interfaceA.tyype)
    runeToType += (self -> CoordTemplataType())
    rules.add(LookupSR(interfaceA.name.range, RuneUsage(interfaceA.name.range, interfaceNameRune), interfaceA.name.getImpreciseName(interner)))
    rules.add(CallSR(interfaceA.name.range, use(-64002, self), RuneUsage(interfaceA.name.range, interfaceNameRune), interfaceA.genericParameters.map(_.rune).toArray))

    val dropFunctionA =
      FunctionA(
        interfaceA.name.range,
        interner.intern(FunctionNameS(keywords.drop, interfaceA.name.range.begin)),
        Vector(),
        TemplateTemplataType(Vector(CoordTemplataType()), FunctionTemplataType()),
        Vector(GenericParameterS(use(-64002, t), None)),
        runeToType.toMap,
        Vector(
          ParameterS(
            AtomSP(
              range(-1340),
              Some(CaptureS(interner.intern(CodeVarNameS(keywords.thiss)))),
              Some(AbstractSP(range(-64002), true)),
              Some(use(-64002, self)), None))),
        Some(use(-64002, v)),
        rules.buildArray().toVector,
        AbstractBodyS)

    Vector(
      interfaceName.copy(last = nameTranslator.translateFunctionNameToTemplateName(dropFunctionA.name)) ->
        FunctionEnvEntry(dropFunctionA))
  }

  override def getInterfaceChildEntries(interfaceName: FullNameT[INameT], interfaceA: InterfaceA, mutability: MutabilityT): Vector[(FullNameT[INameT], IEnvEntry)] = {
    Vector()
  }
}
