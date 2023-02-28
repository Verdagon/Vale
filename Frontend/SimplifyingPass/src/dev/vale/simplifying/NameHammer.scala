package dev.vale.simplifying

import dev.vale.{CodeLocationS, Collector, FileCoordinate, PackageCoordinate, finalast, vfail, vwat}
import dev.vale.finalast.IdH
import dev.vale.finalast._
import dev.vale.postparsing.AnonymousSubstructParentInterfaceTemplateRuneS
import dev.vale.instantiating._
import dev.vale.instantiating.ast._
import dev.vale.von.{IVonData, VonArray, VonInt, VonMember, VonObject, VonStr}

import scala.collection.immutable.List

class NameHammer(translateName: (HinputsI, HamutsBox, INameI[cI]) => IVonData) {
  def getReadableName(namePart: INameI[cI]): String = {
    namePart match {
      case SelfNameI() => "self"
      case AnonymousSubstructImplNameI(_, _, _) => "AnonSubstructImpl"
//      case AbstractVirtualFreeNameI(_, _) => "(abstract vfree)"
//      case OverrideVirtualFreeNameI(_, _) => "(override vfree)"
//      case AbstractVirtualDropFunctionNameI(_, _, _) => "vdrop"
//      case OverrideVirtualDropFunctionNameI(_, _, _) => "vdrop"
//      case FreeNameI(FreeTemplateNameI(codeLoc), templateArgs, kind) => "Free"
      case AnonymousSubstructMemberNameI(index) => "anonSubstructMember" + index
      case AnonymousSubstructConstructorNameI(AnonymousSubstructConstructorTemplateNameI(substruct), templateArgs, params) => "anonSubstructConstructor"
      case AnonymousSubstructNameI(_, _) => "AnonSubstruct"
      case BuildingFunctionNameWithClosuredsI(_) => vwat() // Shouldnt see this in hammer
      case CitizenNameI(templateName, templateArgs) => getReadableName(templateName)
      case StructTemplateNameI(humanName) => humanName.str
      case InterfaceTemplateNameI(humanName) => humanName.str
      case ClosureParamNameI(_) => "closure"
      case CodeVarNameI(name) => name.str
      case ConstructingMemberNameI(name) => name.str
//      case ConstructorNameI(params) => "constructor"
      case ConstructorTemplateNameI(codeLoc) => "constructorTemplate"
      case ExternFunctionNameI(humanName, params) => humanName.str
      case FunctionNameIX(FunctionTemplateNameI(humanName, codeLocation), templateArgs, params) => humanName.str
      case FunctionTemplateNameI(humanName, codeLoc) => humanName.str
      case PackageTopLevelNameI() => vwat() // Does this ever make it to hammer?
//      case ImplDeclareNameI(codeLoc) => "impl" + codeLoc
      case StaticSizedArrayNameI(_, size, variability, arr) => "ssa" + size + "," + variability
      case LambdaCitizenNameI(codeLoc) => "lam"
      case LambdaCallFunctionNameI(_, _, _) => "lamCall"
      case LambdaCallFunctionTemplateNameI(_, _) => "lamTemplate"
      case LetNameI(codeLoc) => "let"
      case IterableNameI(range) => "iterable"
      case IteratorNameI(range) => "iterator"
      case IterationOptionNameI(range) => "iterationOption"
      case MagicParamNameI(codeLoc) => "magicParam"
      case PrimitiveNameI(humanName) => humanName.str
      case RawArrayNameI(mutability, elementType, region) => "rawArr"
      case TypingPassBlockResultVarNameI(num) => "blockResult" + num
      case TypingPassFunctionResultVarNameI() => "funcResult"
      case TypingPassPatternDestructureeNameI(num) => "patDestr" + num
      case TypingPassPatternMemberNameI(life) => "patMem" + life
      case TypingPassTemporaryVarNameI(num) => "tempVar" + num
      case RuntimeSizedArrayNameI(_, _) => "rsa"
      case UnnamedLocalNameI(codeLoc) => "unnamedLocal"
      case ForwarderFunctionTemplateNameI(inner, index) => "fwdt_" + index + "_" + getReadableName(inner)
      case ForwarderFunctionNameI(ForwarderFunctionTemplateNameI(innerFuncName, index), inner) => "fwd_" + index + "_" + getReadableName(inner)
    }
  }

  def translateFullName(
    hinputs: HinputsI,
    hamuts: HamutsBox,
    fullName2: IdI[cI, INameI[cI]]
  ): IdH = {
    val IdI(packageCoord@PackageCoordinate(project, packageSteps), _, _) = fullName2
    val newNameParts = fullName2.steps.map(step => translateName(hinputs, hamuts, step))
    val readableName = getReadableName(fullName2.localName)

    val id =
      if (fullName2.localName.isInstanceOf[ExternFunctionNameI[cI]]) {
        -1
      } else {
        hamuts.getNameId(readableName, packageCoord, newNameParts)
      }
    finalast.IdH(readableName, id, packageCoord, newNameParts)
  }

  // Adds a step to the name.
  def addStep(
    hamuts: HamutsBox,
    fullName: IdH,
    s: String):
  IdH = {
    val newNameParts = fullName.parts :+ VonStr(s)
    val id = hamuts.getNameId(s, fullName.packageCoordinate, newNameParts)
    IdH(s, id, fullName.packageCoordinate, newNameParts)
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
