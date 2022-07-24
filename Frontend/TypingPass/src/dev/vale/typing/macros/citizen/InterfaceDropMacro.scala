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
    strt here
    // "Tests a templated linked list" test fails
    // I think it's because interface Opt<T> is trying to define a drop, even though
    // it has #!DeriveInterfaceDrop. Thats cuz this method is called directly, regardless
    // of whether the #! thing is there or not.
    // We need to make it conditionally run.
    // We cant have this as a child entry because then it will be under the defining environment
    // of the interface, with all sorts of placeholders and nonsense in it.
    // Perhaps we can:
    //  1. just run this from Compiler.scala, if the #! thing isnt there.
    //  2. make an umbrella environment that has siblings and interface in it, so the interface's
    //    placeholder runes dont get into it.
    // Why do we even put that declaring environment anywhere?
    // Ah, its for overload resolution, so we can see its siblings and parents.
    // Well then, we dont have to have the runes in there. Why do we put the runes into an environment?
    // Maybe we can just not do that.
    // Ok, thats option 3.
    //  3. Dont put runes in the environment! The solving is only useful for the definition anyway.

    def range(n: Int) = RangeS.internal(interner, n)
    def use(n: Int, rune: IRuneS) = RuneUsage(range(n), rune)

    val v = CodeRuneS(keywords.V)
    val self = CodeRuneS(keywords.self)

    val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
    runeToType.put(v, CoordTemplataType())

    val interfaceNameRune = StructNameRuneS(interfaceA.name)
    runeToType += (interfaceNameRune -> interfaceA.tyype)
    runeToType += (self -> CoordTemplataType())

    // Use the same generic parameters as the interface
    val functionGenericParameters = interfaceA.genericParameters
    functionGenericParameters.foreach(genericParam => {
      // Bring in their types too
      runeToType += (genericParam.rune.rune -> interfaceA.runeToType(genericParam.rune.rune))
    })

    val rules = new Accumulator[IRulexSR]()
    rules.add(LookupSR(range(-1672147),use(-64002, v),interner.intern(CodeNameS(keywords.void))))
    rules.add(
      LookupSR(
        interfaceA.name.range,
        RuneUsage(interfaceA.name.range, interfaceNameRune),
        interfaceA.name.getImpreciseName(interner)))
    rules.add(
      CallSR(
        interfaceA.name.range,
        use(-64002, self),
        RuneUsage(interfaceA.name.range, interfaceNameRune),
        interfaceA.genericParameters.map(_.rune).toArray))

    val dropFunctionA =
      FunctionA(
        interfaceA.name.range,
        interner.intern(FunctionNameS(keywords.drop, interfaceA.name.range.begin)),
        Vector(),
        TemplateTemplataType(Vector(CoordTemplataType()), FunctionTemplataType()),
        functionGenericParameters,
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
