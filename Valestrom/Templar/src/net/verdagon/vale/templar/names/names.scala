package net.verdagon.vale.templar.names

import net.verdagon.vale._
import net.verdagon.vale.scout.IRuneS
import net.verdagon.vale.templar.ast.LocationInFunctionEnvironment
import net.verdagon.vale.templar.templata.{CoordTemplata, ITemplata}
import net.verdagon.vale.templar.types._

// Scout's/Astronomer's name parts correspond to where they are in the source code,
// but Templar's correspond more to what packages and stamped functions / structs
// they're in. See TNAD.

case class FullNameT[+T <: INameT](
  packageCoord: PackageCoordinate,
  initSteps: Vector[INameT],
  last: T
)  {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  // PackageTopLevelName2 is just here because names have to have a last step.
  vassert(!initSteps.contains(PackageTopLevelNameT()))

  this match {
    case FullNameT(PackageCoordinate.TEST_TLD, Vector(), FunctionNameT("main", Vector(), Vector())) =>
    case _ =>
  }

  def steps: Vector[INameT] = {
    last match {
      case PackageTopLevelNameT() => initSteps
      case _ => initSteps :+ last
    }
  }
  def addStep[Y <: INameT](newLast: Y): FullNameT[Y] = {
    FullNameT[Y](packageCoord, steps, newLast)
  }
  def init: FullNameT[INameT] = {
    if (initSteps.isEmpty) {
      if (last == PackageTopLevelNameT()) {
        vimpl()
      } else {
        FullNameT(packageCoord, Vector(), PackageTopLevelNameT())
      }
    } else {
      FullNameT(packageCoord, initSteps.init, initSteps.last)
    }
  }

  def parent: Option[FullNameT[INameT]] = {
    if (initSteps.isEmpty) {
      packageCoord.parent match {
        case None => None
        case Some(parentPackage) => Some(FullNameT(parentPackage, Vector(), PackageTopLevelNameT()))
      }
    } else {
      Some(FullNameT(packageCoord, initSteps.init, initSteps.last))
    }
  }

  def selfAndParents: List[FullNameT[INameT]] = {
    parent match {
      case None => List(this)
      case Some(parent) => this :: parent.selfAndParents
    }
  }

  def parents: List[FullNameT[INameT]] = {
    parent match {
      case None => List()
      case Some(parent) => parent.selfAndParents
    }
  }
}
// not sure if we need imprecise names in templar
//// An imprecise name is one where we don't know exactly where the thing is defined.
//// For example, in
////   fn main() int export {
////     doStuff("hello");
////   }
//// we don't know exactly where doStuff was defined, that depends on what overload the
//// typing stage decides.
//case class ImpreciseName2[+T <: IImpreciseNameStep2](init: Vector[IImpreciseNameStep2], last: T) {//extends IImpreciseNameS[T] {
//  def addStep[Y <: IImpreciseNameStep2](newLast: Y): ImpreciseName2[Y] = ImpreciseName2[Y](init :+ last, newLast)
//}

sealed trait INameT  {
  def order: Int
}
sealed trait IFunctionNameT extends INameT {
  def templateArgs: Vector[ITemplata]
  def parameters: Vector[CoordT]
}
sealed trait ICitizenNameT extends INameT {
  def templateArgs: Vector[ITemplata]
}
case class ImplDeclareNameT(subCitizenHumanName: String, codeLocation: CodeLocationS) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 1;  }
case class LetNameT(codeLocation: CodeLocationS) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 2;  }
case class ExportAsNameT(codeLocation: CodeLocationS) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 2;  }

case class RawArrayNameT(mutability: MutabilityT, elementType: CoordT) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 40;  }
case class StaticSizedArrayNameT(size: Int, arr: RawArrayNameT) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 42;  }
case class RuntimeSizedArrayNameT(arr: RawArrayNameT) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 47;  }
sealed trait IVarNameT extends INameT
case class TemplarBlockResultVarNameT(life: LocationInFunctionEnvironment) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 18;  }
case class TemplarFunctionResultVarNameT() extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 19;  }
case class TemplarTemporaryVarNameT(life: LocationInFunctionEnvironment) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 20;  }
case class TemplarPatternMemberNameT(life: LocationInFunctionEnvironment) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 23;  }
case class TemplarIgnoredParamNameT(num: Int) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 53;  }
case class TemplarPatternDestructureeNameT(life: LocationInFunctionEnvironment) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 23;  }
case class UnnamedLocalNameT(codeLocation: CodeLocationS) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 3;  }
case class ClosureParamNameT() extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 41;  }
case class ConstructingMemberNameT(name: String) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 4;  }
case class MagicParamNameT(codeLocation2: CodeLocationS) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 5;  }
case class CodeVarNameT(name: String) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 6;  }
// We dont use CodeVarName2(0), CodeVarName2(1) etc because we dont want the user to address these members directly.
case class AnonymousSubstructMemberNameT(index: Int) extends IVarNameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 24;  }
case class PrimitiveNameT(humanName: String) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 26;  }
// Only made in templar
case class PackageTopLevelNameT() extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 25;  }
case class ProjectNameT(name: String) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 51;  }
case class PackageNameT(name: String) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 52;  }
case class RuneNameT(rune: IRuneS) extends INameT { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 52;  }

// We use this one to look for impls, which are disambiguated by the above ImplDeclareName2
//case class ImplImpreciseName2() extends IName2 { def order = 22; def all[T](func: PartialFunction[Queriable2, T]): Vector[T] = { Vector(this).collect(func) } }

// This is the name of a function that we're still figuring out in the function templar.
// We have its closured variables, but are still figuring out its template args and params.
case class BuildingFunctionNameWithClosuredsT(
  templateName: IFunctionTemplateNameT,
) extends INameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 33;

}
// This is the name of a function that we're still figuring out in the function templar.
// We have its closured variables and template args, but are still figuring out its params.
case class BuildingFunctionNameWithClosuredsAndTemplateArgsT(
  templateName: IFunctionTemplateNameT,
  templateArgs: Vector[ITemplata]
) extends INameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 37;

}

//// We dont just use "destructor" as the name because we don't want the user to override it.
//case class ImmConcreteDestructorNameT(kind: KindT) extends IFunctionNameT {
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//  override def templateArgs: Vector[ITemplata] = Vector(CoordTemplata(CoordT(ShareT, ReadonlyT, kind)))
//  override def parameters: Vector[CoordT] = Vector(CoordT(ShareT, ReadonlyT, kind))
//
//  kind match {
//    case InterfaceTT(_) => vwat()
//    case _ =>
//  }
//
//  def order = 38;
//
//}
// We dont just use "idestructor" as the name because we don't want the user to override it.
//case class ImmInterfaceDestructorNameT(
//    templateArgs: Vector[ITemplata],
//    parameters: Vector[CoordT]
//) extends IFunctionNameT {
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//  def order = 38;
//
//}
// We dont just use "drop" as the name because we don't want the user to override it.
case class DropNameT(templateArgs: Vector[ITemplata], coord: CoordT) extends IFunctionNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//  override def templateArgs: Vector[ITemplata] = Vector()
  override def parameters: Vector[CoordT] = Vector(coord)
  def order = 39;
}


case class ExternFunctionNameT(
  humanName: String,
  parameters: Vector[CoordT]
) extends IFunctionNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  override def templateArgs: Vector[ITemplata] = Vector.empty

  def order = 46;

}

case class FunctionNameT(
  humanName: String,
  templateArgs: Vector[ITemplata],
  parameters: Vector[CoordT]
) extends IFunctionNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

  def order = 13;

}
sealed trait IFunctionTemplateNameT extends INameT

case class FunctionTemplateNameT(
    humanName: String,
    codeLocation: CodeLocationS
) extends INameT with IFunctionTemplateNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 31;

}
case class LambdaTemplateNameT(
  codeLocation: CodeLocationS
) extends INameT with IFunctionTemplateNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 36;

}
case class ConstructorTemplateNameT(
  codeLocation: CodeLocationS
) extends INameT with IFunctionTemplateNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 35;

}
//case class ImmConcreteDestructorTemplateNameT() extends INameT with IFunctionTemplateNameT {
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//  def order = 43;
//
//}
//case class ImmInterfaceDestructorTemplateNameT() extends INameT with IFunctionTemplateNameT {
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//  def order = 44;
//
//}
//case class DropTemplateNameT() extends INameT with IFunctionTemplateNameT {
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//  def order = 45;
//}
// Vale has no Self, its just a convenient first name parameter.
// See also SelfNameS.
case class SelfNameT() extends INameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 55;
}
case class ConstructorNameT(
  parameters: Vector[CoordT]
) extends IFunctionNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 21;
  def templateArgs: Vector[ITemplata] = Vector.empty

}
//// We have this and LambdaCitizenName2 both because sometimes lambdas dont come with
//// a struct, like if they capture nothing. When they do come with structs, theyll both
//// be in the name, this one after the LambdaCitizenName2 name.
//case class LambdaName2(
//  codeLocation: CodeLocation2,
//  templateArgs: Vector[ITemplata],
//  parameters: Vector[Coord]
//) extends IFunctionName2 {
//  def order = 14;
//  def all[T](func: PartialFunction[Queriable2, T]): Vector[T] = {
//    Vector(this).collect(func) ++ templateArgs.flatMap(_.all(func)) ++ parameters.flatMap(_.all(func))
//  }
//}
//case class CitizenName2(
//  humanName: String,
//  templateArgs: Vector[ITemplata]
//) extends ICitizenName2 {
//  def order = 15;
//  def all[T](func: PartialFunction[Queriable2, T]): Vector[T] = {
//    Vector(this).collect(func) ++ templateArgs.flatMap(_.all(func))
//  }
//}
case class CitizenNameT(
  humanName: String,
  templateArgs: Vector[ITemplata]
) extends ICitizenNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 15;

}
case class TupleNameT(
  members: Vector[CoordT]
) extends ICitizenNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  vpass()
  override def templateArgs: Vector[ITemplata] = members.map(CoordTemplata)
  def order = 16;

}
case class LambdaCitizenNameT(
  codeLocation: CodeLocationS
) extends ICitizenNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  vpass()

  def templateArgs: Vector[ITemplata] = Vector.empty
  def order = 17;

}
case class AnonymousSubstructLambdaNameT(
  codeLocation: CodeLocationS
) extends ICitizenNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  vpass()

  def templateArgs: Vector[ITemplata] = Vector.empty
  def order = 54;

}
case class CitizenTemplateNameT(
  humanName: String,
  codeLocation: CodeLocationS
) extends INameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 30;


  def makeCitizenName(templateArgs: Vector[ITemplata]): CitizenNameT = {
    CitizenNameT(humanName, templateArgs)
  }
}
case class AnonymousSubstructNameT(callables: Vector[CoordT]) extends ICitizenNameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 27;
  def templateArgs: Vector[ITemplata] = callables.map(CoordTemplata)

}
case class AnonymousSubstructImplNameT() extends INameT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
  def order = 29;

}
//// This one is probably only used by the templar, so we can have a way to
//// figure out the closure struct for a certain environment.
//case class EnvClosureName2() extends IName2 {
//  def order = 32;
//  def all[T](func: PartialFunction[Queriable2, T]): Vector[T] = {
//    Vector(this).collect(func)
//  }
//}

// This is an IName2 because we put these into the environment.
// We don't just reuse INameA because there are some templar-specific ones.
//case class CodeRuneS(name: String) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 7;  }
//case class ImplicitRuneS(parentName: INameT, name: Int) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 8;  }
//case class LetImplicitRuneS(codeLocation: CodeLocationT, name: Int) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 34;  }
//case class ArraySizeImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 48;  }
//case class ArrayVariabilityImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 49;  }
//case class ArrayMutabilityImplicitRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 50;  }
//case class MemberRuneS(memberIndex: Int) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 9;  }
//case class MagicImplicitRuneS(scoutPath: Array[Int]) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 10;  }
//case class ReturnRuneS() extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 11;  }
//case class SolverKindRuneS(paramRune: IRuneS) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 12;  }
//case class ExplicitTemplateArgRuneS(index: Int) extends IRuneS { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; def order = 34;  }
//
//case class AnonymousSubstructParentInterfaceRuneS() extends IRuneS {
//  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;
//  def order = 28;
//
//}

//
//sealed trait IImpreciseNameStep2
//case class CodeTypeName2(name: String) extends IImpreciseNameStep2 { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
//// When we're calling a function, we're addressing an overload set, not a specific function.
//// If we want a specific function, we use TopLevelDeclarationNameS.
//case class GlobalFunctionFamilyName2(name: String) extends IImpreciseNameStep2 { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
//case class ImpreciseCodeVarName2(name: String) extends IImpreciseNameStep2 { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; }
