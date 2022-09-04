package dev.vale.simplifying

import dev.vale.{CodeLocationS, FileCoordinate, PackageCoordinate, finalast, vwat}
import dev.vale.finalast.FullNameH
import dev.vale.typing.Hinputs
import dev.vale.typing.names._
import dev.vale.finalast._
import dev.vale.postparsing.AnonymousSubstructParentInterfaceTemplateRuneS
import dev.vale.typing._
import dev.vale.typing.env.PackageEnvironment
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.CodeLocationS
import dev.vale.von.{IVonData, VonArray, VonInt, VonMember, VonObject, VonStr}

import scala.collection.immutable.List

class NameHammer(translateName: (Hinputs, HamutsBox, INameT) => IVonData) {
  def getReadableName(namePart: INameT): String = {
    namePart match {
      case SelfNameT() => "self"
      case AnonymousSubstructImplNameT(template, templateArgs) => "AnonSubstructImpl"
//      case AbstractVirtualFreeNameT(_, _) => "(abstract vfree)"
//      case OverrideVirtualFreeNameT(_, _) => "(override vfree)"
//      case AbstractVirtualDropFunctionNameT(_, _, _) => "vdrop"
//      case OverrideVirtualDropFunctionNameT(_, _, _) => "vdrop"
      case FreeNameT(FreeTemplateNameT(codeLoc), templateArgs, kind) => "Free"
      case AnonymousSubstructMemberNameT(index) => "anonSubstructMember" + index
      case AnonymousSubstructConstructorNameT(AnonymousSubstructConstructorTemplateNameT(substruct), templateArgs, params) => "anonSubstructConstructor"
      case AnonymousSubstructNameT(_, _) => "AnonSubstruct"
      case BuildingFunctionNameWithClosuredsT(_) => vwat() // Shouldnt see this in hammer
      case CitizenNameT(templateName, templateArgs) => getReadableName(templateName)
      case StructTemplateNameT(humanName) => humanName.str
      case InterfaceTemplateNameT(humanName) => humanName.str
      case ClosureParamNameT() => "closure"
      case CodeVarNameT(name) => name.str
      case ConstructingMemberNameT(name) => name.str
//      case ConstructorNameT(params) => "constructor"
      case ConstructorTemplateNameT(codeLoc) => "constructorTemplate"
      case ExternFunctionNameT(humanName, params) => humanName.str
      case FunctionNameT(FunctionTemplateNameT(humanName, codeLocation), templateArgs, params) => humanName.str
      case FunctionTemplateNameT(humanName, codeLoc) => humanName.str
      case PackageTopLevelNameT() => vwat() // Does this ever make it to hammer?
//      case ImplDeclareNameT(codeLoc) => "impl" + codeLoc
      case StaticSizedArrayNameT(_, size, variability, arr) => "ssa" + size + "," + variability
      case LambdaCitizenNameT(codeLoc) => "lam"
      case LambdaCallFunctionNameT(_, _, _) => "lamCall"
      case LambdaCallFunctionTemplateNameT(_, _) => "lamTemplate"
      case LetNameT(codeLoc) => "let"
      case IterableNameT(range) => "iterable"
      case IteratorNameT(range) => "iterator"
      case IterationOptionNameT(range) => "iterationOption"
      case MagicParamNameT(codeLoc) => "magicParam"
      case PrimitiveNameT(humanName) => humanName.str
      case RawArrayNameT(mutability, elementType) => "rawArr"
      case TypingPassBlockResultVarNameT(num) => "blockResult" + num
      case TypingPassFunctionResultVarNameT() => "funcResult"
      case TypingPassPatternDestructureeNameT(num) => "patDestr" + num
      case TypingPassPatternMemberNameT(life) => "patMem" + life
      case TypingPassTemporaryVarNameT(num) => "tempVar" + num
      case RuntimeSizedArrayNameT(_, _) => "rsa"
      case UnnamedLocalNameT(codeLoc) => "unnamedLocal"
      case ForwarderFunctionTemplateNameT(inner, index) => "fwdt_" + index + "_" + getReadableName(inner)
      case ForwarderFunctionNameT(ForwarderFunctionTemplateNameT(innerFuncName, index), inner) => "fwd_" + index + "_" + getReadableName(inner)
    }
  }

  def translateFullName(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    fullName2: FullNameT[INameT]
  ): FullNameH = {
    val FullNameT(packageCoord@PackageCoordinate(project, packageSteps), _, _) = fullName2
    val newNameParts = fullName2.steps.map(step => translateName(hinputs, hamuts, step))
    val readableName = getReadableName(fullName2.last)

    val id =
      if (fullName2.last.isInstanceOf[ExternFunctionNameT]) {
        -1
      } else {
        hamuts.getNameId(readableName, packageCoord, newNameParts)
      }
    finalast.FullNameH(readableName, id, packageCoord, newNameParts)
  }

  // Adds a step to the name.
  def addStep(
    hamuts: HamutsBox,
    fullName: FullNameH,
    s: String):
  FullNameH = {
    val newNameParts = fullName.parts :+ VonStr(s)
    val id = hamuts.getNameId(s, fullName.packageCoordinate, newNameParts)
    FullNameH(s, id, fullName.packageCoordinate, newNameParts)
  }
}

object NameHammer {
  def translateCodeLocation(location: CodeLocationS): VonObject = {
    val CodeLocationS(fileCoord, offset) = location
    VonObject(
      "CodeLocation",
      None,
      Vector(
        VonMember("file", translateFileCoordinate(fileCoord)),
        VonMember("offset", VonInt(offset))))
  }

  def translateFileCoordinate(coord: FileCoordinate): VonObject = {
    val FileCoordinate(PackageCoordinate(module, paackage), filename) = coord
    VonObject(
      "FileCoordinate",
      None,
      Vector(
        VonMember("module", VonStr(module.str)),
        VonMember("paackage", VonArray(None, paackage.map(_.str).map(VonStr).toVector)),
        VonMember("filename", VonStr(filename))))
  }

  def translatePackageCoordinate(coord: PackageCoordinate): VonObject = {
    val PackageCoordinate(module, paackage) = coord
    val nonEmptyModuleName = if (module.str == "") "__vale" else module.str;
    VonObject(
      "PackageCoordinate",
      None,
      Vector(
        VonMember("project", VonStr(nonEmptyModuleName)),
        VonMember("packageSteps", VonArray(None, paackage.map(_.str).map(VonStr).toVector))))
  }
}
