package net.verdagon.vale.scout

import net.verdagon.vale.{CodeLocationS, PackageCoordinate, RangeS, vassert, vcheck, vcurious, vimpl, vpass, vwat}

trait INameS
sealed trait IVarNameS extends INameS
trait IFunctionDeclarationNameS extends INameS {
  def packageCoordinate: PackageCoordinate
}
case class LambdaNameS(
//  parentName: INameS,
  codeLocation: CodeLocationS
) extends IFunctionDeclarationNameS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  override def packageCoordinate: PackageCoordinate = codeLocation.file.packageCoordinate
}
case class FunctionNameS(name: String, codeLocation: CodeLocationS) extends IFunctionDeclarationNameS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  override def packageCoordinate: PackageCoordinate = codeLocation.file.packageCoordinate
}
case class TopLevelCitizenDeclarationNameS(name: String, range: RangeS) extends INameS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  vpass()
}
case class LambdaStructNameS(lambdaName: LambdaNameS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class ImplNameS(subCitizenHumanName: String, codeLocation: CodeLocationS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class ExportAsNameS(codeLocation: CodeLocationS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class LetNameS(codeLocation: CodeLocationS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
//case class UnnamedLocalNameS(codeLocation: CodeLocationS) extends IVarNameS {  override def hashCode(): Int = vcurious() }
case class ClosureParamNameS() extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class MagicParamNameS(codeLocation: CodeLocationS) extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class AnonymousSubstructMemberNameS(index: Int) extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class CodeVarNameS(name: String) extends IVarNameS {
  vcheck(name != "set", "Can't name a variable 'set'")
  vcheck(name != "mut", "Can't name a variable 'mut'")
}
case class ConstructingMemberNameS(name: String) extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class RuneNameS(rune: IRuneS) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
//case class DropNameS(packageCoordinate: PackageCoordinate) extends IFunctionDeclarationNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }

// We differentiate rune names from regular names, we scout out what's actually
// a rune so we can inform the templar. The templar wants to know so it can know
// how to handle this thing; if it's a name, we expect it to exist in the
// environment already, but if it's a rune we can assign something into it.
// Also, we might refer to a rune that was defined in our container's container's
// container, so asking "is this thing here a rune" involves looking at all our
// containers. That's much easier for the scout, so thats a nice bonus.
// We have all these subclasses instead of a string so we don't have to have
// prefixes and names like __implicit_0, __paramRune_0, etc.
// This extends INameS so we can use it as a lookup key in Templar's environments.
trait IRuneS
case class CodeRuneS(name: String) extends IRuneS {
  vpass()
}
case class ImplDropCoordRuneS() extends IRuneS
case class ImplDropVoidRuneS() extends IRuneS
case class ImplicitRuneS(lid: LocationInDenizen) extends IRuneS {
  vpass()
}
case class LetImplicitRuneS(lid: LocationInDenizen) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class MagicParamRuneS(lid: LocationInDenizen) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class MemberRuneS(memberIndex: Int) extends IRuneS {
   val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
// Used to type the templex handed to the size part of the static sized array expressions
case class ArraySizeImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// Used to type the templex handed to the mutability part of the static sized array expressions
case class ArrayMutabilityImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// Used to type the templex handed to the variability part of the static sized array expressions
case class ArrayVariabilityImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class ReturnRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class StructNameRuneS(structName: TopLevelCitizenDeclarationNameS) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class CodeNameS(name: String) extends INameS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  vpass()
  vassert(name != "_")
}
// When we're calling a function, we're addressing an overload set, not a specific function.
// If we want a specific function, we use TopLevelDeclarationNameS.
case class GlobalFunctionFamilyNameS(name: String) extends INameS {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
}
case class ImpreciseCodeVarNameS(name: String) extends INameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// These are only made by the templar
case class ArgumentRuneS(argIndex: Int) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class PatternInputRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class ExplicitTemplateArgRuneS(index: Int) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
case class AnonymousSubstructParentInterfaceRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// Vale has no notion of Self, it's just a convenient name for a first parameter.
case class SelfNameS() extends IVarNameS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
// A miscellaneous name, for when a name doesn't really make sense, like it's the only entry in the environment or something.
case class ArbitraryNameS() extends INameS