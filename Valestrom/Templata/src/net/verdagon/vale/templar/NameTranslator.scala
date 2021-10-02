package net.verdagon.vale.templar

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout._
import net.verdagon.vale.templar.types.CitizenRefT
import net.verdagon.vale.{vimpl, vwat}

import scala.collection.immutable.List

object NameTranslator {
  def translateFunctionNameToTemplateName(functionName: IFunctionDeclarationNameS): IFunctionTemplateNameT = {
      functionName match {
        case ImmConcreteDestructorNameS(_) => ImmConcreteDestructorTemplateNameT()
        case ImmInterfaceDestructorNameS(_) => ImmInterfaceDestructorTemplateNameT()
        case ImmDropNameS(_) => ImmDropTemplateNameT()
        case LambdaNameS(/*parent, */codeLocation) => {
          LambdaTemplateNameT(NameTranslator.translateCodeLocation(codeLocation))
        }
        case FunctionNameS(name, codeLocation) => {
          FunctionTemplateNameT(name, NameTranslator.translateCodeLocation(codeLocation))
        }
        case ConstructorNameS(TopLevelCitizenDeclarationNameS(name, codeLocation)) => {
          FunctionTemplateNameT(name, NameTranslator.translateCodeLocation(codeLocation.begin))
        }
      }
  }

//  def translateImpreciseTypeName(fullNameS: ImpreciseNameS[CodeTypeNameS]): ImpreciseName2[CodeTypeName2] = {
//    val ImpreciseNameS(initS, lastS) = fullNameS
//    ImpreciseName2(initS.map(translateImpreciseNameStep), translateCodeTypeName(lastS))
//  }
//
//  def translateImpreciseName(fullNameS: ImpreciseNameS[INameS]): ImpreciseName2[IImpreciseNameStep2] = {
//    val ImpreciseNameS(initS, lastS) = fullNameS
//    ImpreciseName2(initS.map(translateImpreciseNameStep), translateImpreciseNameStep(lastS))
//  }
//
//  def translateCodeTypeName(codeTypeNameS: CodeTypeNameS): CodeTypeName2 = {
//    val CodeTypeNameS(name) = codeTypeNameS
//    CodeTypeName2(name)
//  }
//
//  def translateImpreciseNameStep(impreciseNameStepA: INameS): IImpreciseNameStep2 = {
//    impreciseNameStepA match {
//      case ctn @ CodeTypeNameS(_) => translateCodeTypeName(ctn)
//      case GlobalFunctionFamilyNameS(name) => GlobalFunctionFamilyName2(name)
//      case icvn @ ImpreciseCodeVarNameS(_) => translateImpreciseCodeVarNameStep(icvn)
//    }
//  }
//
//  def translateImpreciseCodeVarNameStep(impreciseNameStepA: ImpreciseCodeVarNameS): ImpreciseCodeVarName2 = {
//    var ImpreciseCodeVarNameS(name) = impreciseNameStepA
//    ImpreciseCodeVarName2(name)
//  }
//
//  def translateRune(absoluteNameS: AbsoluteNameS[IRuneS]): FullName2[IRuneT] = {
//    val AbsoluteNameS(file, initS, lastS) = absoluteNameS
//    FullName2(file, initS.map(translateNameStep), translateRune(lastS))
//  }
//
//  def translateVarAbsoluteName(absoluteNameS: AbsoluteNameS[IVarNameS]): FullName2[IVarName2] = {
//    val AbsoluteNameS(file, initS, lastS) = absoluteNameS
//    FullName2(file, initS.map(translateNameStep), translateVarNameStep(lastS))
//  }
//
//  def translateVarImpreciseName(absoluteNameS: ImpreciseNameS[ImpreciseCodeVarNameS]):
//  ImpreciseName2[ImpreciseCodeVarName2] = {
//    val ImpreciseNameS(initS, lastS) = absoluteNameS
//    ImpreciseName2(initS.map(translateImpreciseNameStep), translateImpreciseCodeVarNameStep(lastS))
//  }
//
//  def translateFunctionFamilyName(name: ImpreciseNameS[GlobalFunctionFamilyNameS]):
//  ImpreciseName2[GlobalFunctionFamilyName2] = {
//    val ImpreciseNameS(init, last) = name
//    ImpreciseName2(init.map(translateImpreciseNameStep), translateGlobalFunctionFamilyName(last))
//  }
//
//  def translateGlobalFunctionFamilyName(s: GlobalFunctionFamilyNameS): GlobalFunctionFamilyName2 = {
//    val GlobalFunctionFamilyNameS(name) = s
//    GlobalFunctionFamilyName2(name)
//  }
//
//  def translateName(absoluteNameS: AbsoluteNameS[INameS]): FullName2[IName2] = {
//    val AbsoluteNameS(file, initS, lastS) = absoluteNameS
//    FullName2(file, initS.map(translateNameStep), translateNameStep(lastS))
//  }

  def translateCitizenName(name: TopLevelCitizenDeclarationNameS): CitizenTemplateNameT = {
    val TopLevelCitizenDeclarationNameS(humanName, codeLocation) = name
    CitizenTemplateNameT(humanName, NameTranslator.translateCodeLocation(codeLocation.begin))
  }

  def translateNameStep(name: INameS): INameT = {
    name match {
//      case LambdaNameS(codeLocation) => LambdaName2(codeLocation)
//      case FunctionNameS(name, codeLocation) => FunctionName2(name, codeLocation)
//      case TopLevelCitizenDeclarationNameS(name, codeLocation) => TopLevelCitizenDeclarationName2(name, codeLocation)
      case CodeTypeNameS(n @ ("int" | "str")) => PrimitiveNameT(n)
      case LambdaStructNameS(LambdaNameS(codeLocation)) => LambdaCitizenNameT(NameTranslator.translateCodeLocation(codeLocation))
      case ImplNameS(subCitizenHumanName, codeLocation) => ImplDeclareNameT(subCitizenHumanName, translateCodeLocation(codeLocation))
      case LetNameS(codeLocation) => LetNameT(translateCodeLocation(codeLocation))
      case ExportAsNameS(codeLocation) => ExportAsNameT(translateCodeLocation(codeLocation))
//      case UnnamedLocalNameS(codeLocation) => UnnamedLocalNameT(translateCodeLocation(codeLocation))
      case ClosureParamNameS() => ClosureParamNameT()
      case MagicParamNameS(codeLocation) => MagicParamNameT(translateCodeLocation(codeLocation))
      case CodeVarNameS(name) => CodeVarNameT(name)
//      case ImplicitRuneS(parentName, name) => ImplicitRuneT(translateNameStep(parentName), name)
      case t @ TopLevelCitizenDeclarationNameS(_, _) => translateCitizenName(t)
//      case CodeRuneS(name) => CodeRuneT(name)
//      case MagicImplicitRuneS(codeLocationS) => MagicImplicitRuneT(codeLocationS)
//      case AnonymousSubstructParentInterfaceRuneS() => AnonymousSubstructParentInterfaceRuneT()
//      case LetImplicitRuneS(codeLocation, name) => LetImplicitRuneT(translateCodeLocation(codeLocation), name)
//      case ImplicitRuneS(name) => ImplicitRune2(name)
//      case MagicImplicitRuneS(magicParamIndex) => MagicImplicitRune2(magicParamIndex)
//      case MemberRuneS(memberIndex) => MemberRuneT(memberIndex)
//      case ReturnRuneS() => ReturnRuneT()

      case LambdaNameS(codeLocation) => {
        LambdaTemplateNameT(NameTranslator.translateCodeLocation(codeLocation))
      }
      case FunctionNameS(name, codeLocation) => {
        FunctionTemplateNameT(name, NameTranslator.translateCodeLocation(codeLocation))
      }
//      case ConstructorNameS(TopLevelCitizenDeclarationNameS(name, codeLocation)) => {
//        FunctionTemplateNameT(name, NameTranslator.translateCodeLocation(codeLocation))
//      }
      case _ => vimpl(name.toString)
    }
  }

  def translateCodeLocation(s: CodeLocationS): CodeLocationS = {
    val CodeLocationS(line, col) = s
    CodeLocationS(line, col)
  }

  def translateVarNameStep(name: IVarNameS): IVarNameT = {
    name match {
//      case UnnamedLocalNameS(codeLocation) => UnnamedLocalNameT(translateCodeLocation(codeLocation))
      case ClosureParamNameS() => ClosureParamNameT()
      case MagicParamNameS(codeLocation) => MagicParamNameT(translateCodeLocation(codeLocation))
      case ConstructingMemberNameS(n) => ConstructingMemberNameT(n)
      case CodeVarNameS(name) => CodeVarNameT(name)
      case AnonymousSubstructMemberNameS(index) => AnonymousSubstructMemberNameT(index)
    }
  }

//  def translateRune(rune: IRuneS): IRuneS = {
//    rune match {
//      case CodeRuneS(name) => CodeRuneS(name)
////      case ImplicitRuneS(containerName, name) => ImplicitRuneT(translateNameStep(containerName), name)
////      case LetImplicitRuneS(codeLocation, name) => LetImplicitRuneT(translateCodeLocation(codeLocation), name)
////      case MagicImplicitRuneS(codeLocation) => MagicImplicitRuneT(codeLocation)
//      case MemberRuneS(memberIndex) => MemberRuneS(memberIndex)
//      case ReturnRuneS() => ReturnRuneS()
//      case ArraySizeImplicitRuneS() => ArraySizeImplicitRuneS()
//      case ArrayVariabilityImplicitRuneS() => ArrayVariabilityImplicitRuneS()
//      case ArrayMutabilityImplicitRuneS() => ArrayMutabilityImplicitRuneS()
////      case AnonymousSubstructParentInterfaceRuneS() => AnonymousSubstructParentInterfaceRuneT()
//      case ExplicitTemplateArgRuneS(index) => ExplicitTemplateArgRuneS(index)
//      case x => vimpl(x.toString)
//    }
//  }

  def translateImplName(n: ImplNameS): ImplDeclareNameT = {
    val ImplNameS(subCitizenHumanName, l) = n
    ImplDeclareNameT(subCitizenHumanName, translateCodeLocation(l))
  }

  def getImplNameForNameInner(useOptimization: Boolean, nameSteps: Vector[INameT]): Option[ImplImpreciseNameS] = {
    nameSteps.last match {
      case CitizenNameT(humanName, templateArgs) => Some(ImplImpreciseNameS(humanName))
      case TupleNameT(_) => None
      case LambdaCitizenNameT(_) => None
      case AnonymousSubstructNameT(_) => {
        // Use the paren'ts name, see INSHN.
        getImplNameForNameInner(useOptimization, nameSteps.init)
      }
      case _ => vwat()
    }
  }

  // Gets the name of an impl that would be for this citizen.
  // Returns None if it can't be in an impl.
  def getImplNameForName(useOptimization: Boolean, ref: CitizenRefT): Option[ImplImpreciseNameS] = {
    getImplNameForNameInner(useOptimization, ref.fullName.steps)
  }
}
