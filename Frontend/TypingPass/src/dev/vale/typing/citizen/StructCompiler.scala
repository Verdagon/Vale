package dev.vale.typing.citizen

import dev.vale.highertyping.FunctionA
import dev.vale.{Interner, Keywords, Profiler, RangeS, vcurious, _}
import dev.vale.postparsing._
import dev.vale.postparsing.rules.IRulexSR
import dev.vale.typing.ast.{FunctionHeaderT, PrototypeT}
import dev.vale.typing.env.IEnvironment
import dev.vale.typing.{CompilerOutputs, InferCompiler, TypingPassOptions}
import dev.vale.typing.names.{FullNameT, ICitizenNameT, ICitizenTemplateNameT, IInterfaceTemplateNameT, IStructTemplateNameT, ITemplateNameT, NameTranslator}
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.parsing._
import dev.vale.postparsing.patterns.AtomSP
import dev.vale.postparsing.rules._
import dev.vale.typing._
import dev.vale.typing.env._
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.ast._
import dev.vale.typing.function.FunctionCompiler.{EvaluateFunctionSuccess, IEvaluateFunctionResult}
import dev.vale.typing.templata.ITemplata.expectMutability

import scala.collection.immutable.List
import scala.collection.mutable

case class WeakableImplingMismatch(structWeakable: Boolean, interfaceWeakable: Boolean) extends Throwable { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

trait IStructCompilerDelegate {
  def evaluateGenericFunctionFromNonCallForHeader(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    functionTemplata: FunctionTemplata,
    verifyConclusions: Boolean):
  FunctionHeaderT
//
//  def evaluateGenericLightFunctionFromCallForPrototype(
//    coutputs: CompilerOutputs,
//    callRange: List[RangeS],
//    callingEnv: IEnvironment, // See CSSNCE
//    functionTemplata: FunctionTemplata,
//    explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
//    args: Vector[Option[CoordT]]):
//  IEvaluateFunctionResult

  def scoutExpectedFunctionForPrototype(
    env: IEnvironment,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    functionName: IImpreciseNameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Array[IRuneS],
    args: Vector[CoordT],
    extraEnvsToLookIn: Vector[IEnvironment],
    exact: Boolean,
    verifyConclusions: Boolean):
  EvaluateFunctionSuccess
}

case class ResolveSuccess[+T <: KindT](
  kind: T,
  //runeToSuppliedFunction: Map[IRuneS, PrototypeTemplata]
)

class StructCompiler(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    templataCompiler: TemplataCompiler,
    inferCompiler: InferCompiler,
    delegate: IStructCompilerDelegate) {
  val templateArgsLayer =
    new StructCompilerGenericArgsLayer(
      opts, interner, keywords, nameTranslator, templataCompiler, inferCompiler, delegate)

  def resolveStruct(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: List[RangeS],
    structTemplata: StructDefinitionTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  ResolveSuccess[StructTT] = {
    Profiler.frame(() => {
      templateArgsLayer.resolveStruct(
        coutputs, callingEnv, callRange, structTemplata, uncoercedTemplateArgs)
    })
  }

  def precompileStruct(
    coutputs: CompilerOutputs,
    structTemplata: StructDefinitionTemplata):
  Unit = {
    val StructDefinitionTemplata(declaringEnv, structA) = structTemplata

    val structTemplateFullName = templataCompiler.resolveStructTemplate(structTemplata)

    coutputs.declareType(structTemplateFullName)

    structA.maybePredictedMutability match {
      case None =>
      case Some(predictedMutability) => {
        coutputs.declareTypeMutability(
          structTemplateFullName,
          MutabilityTemplata(Conversions.evaluateMutability(predictedMutability)))
      }
    }
  }

  def precompileInterface(
    coutputs: CompilerOutputs,
    interfaceTemplata: InterfaceDefinitionTemplata):
  Unit = {
    val InterfaceDefinitionTemplata(declaringEnv, interfaceA) = interfaceTemplata

    val interfaceTemplateFullName = templataCompiler.resolveInterfaceTemplate(interfaceTemplata)

    coutputs.declareType(interfaceTemplateFullName)

    interfaceA.maybePredictedMutability match {
      case None =>
      case Some(predictedMutability) => {
        coutputs.declareTypeMutability(
          interfaceTemplateFullName,
          MutabilityTemplata(Conversions.evaluateMutability(predictedMutability)))
      }
    }
  }

  def compileStruct(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    structTemplata: StructDefinitionTemplata):
  Unit = {
    Profiler.frame(() => {
      templateArgsLayer.compileStruct(coutputs, parentRanges, structTemplata)
    })
  }

  // See SFWPRL for how this is different from resolveInterface.
  def predictInterface(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: List[RangeS],
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceDefinitionTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  (InterfaceTT) = {
    templateArgsLayer.predictInterface(
      coutputs, callingEnv, callRange, interfaceTemplata, uncoercedTemplateArgs)
  }

  // See SFWPRL for how this is different from resolveStruct.
  def predictStruct(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: List[RangeS],
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    structTemplata: StructDefinitionTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  (StructTT) = {
    templateArgsLayer.predictStruct(
      coutputs, callingEnv, callRange, structTemplata, uncoercedTemplateArgs)
  }

  def resolveInterface(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: List[RangeS],
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceDefinitionTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  ResolveSuccess[InterfaceTT] = {
    val success =
      templateArgsLayer.resolveInterface(
        coutputs, callingEnv, callRange, interfaceTemplata, uncoercedTemplateArgs)

    success
  }

  def resolveCitizen(
    coutputs: CompilerOutputs,
    callingEnv: IEnvironment, // See CSSNCE
    callRange: List[RangeS],
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    citizenTemplata: CitizenDefinitionTemplata,
    uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
  ResolveSuccess[ICitizenTT] = {
    citizenTemplata match {
      case st @ StructDefinitionTemplata(_, _) => resolveStruct(coutputs, callingEnv, callRange, st, uncoercedTemplateArgs)
      case it @ InterfaceDefinitionTemplata(_, _) => resolveInterface(coutputs, callingEnv, callRange, it, uncoercedTemplateArgs)
    }
  }

  def compileInterface(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceDefinitionTemplata):
  Unit = {
    templateArgsLayer.compileInterface(
      coutputs, parentRanges, interfaceTemplata)
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    name: IFunctionDeclarationNameS,
    functionS: FunctionA,
    members: Vector[StructMemberT]):
  (StructTT, MutabilityT, FunctionTemplata) = {
//    Profiler.reentrant("StructCompiler-makeClosureUnderstruct", name.codeLocation.toString, () => {
      templateArgsLayer.makeClosureUnderstruct(containingFunctionEnv, coutputs, parentRanges, name, functionS, members)
//    })
  }

//  def getMemberCoords(coutputs: CompilerOutputs, structTT: StructTT): Vector[CoordT] = {
//    coutputs.lookupStruct(structTT).members.map(_.tyype).map({
//      case ReferenceMemberTypeT(coord) => coord
//      case AddressMemberTypeT(_) => {
//        // At time of writing, the only one who calls this is the inferer, who wants to know so it
//        // can match incoming arguments into a destructure. Can we even destructure things with
//        // addressible members?
//        vcurious()
//      }
//    })
//  }

}

object StructCompiler {
  def getCompoundTypeMutability(memberTypes2: Vector[CoordT])
  : MutabilityT = {
    val membersOwnerships = memberTypes2.map(_.ownership)
    val allMembersImmutable = membersOwnerships.isEmpty || membersOwnerships.toSet == Set(ShareT)
    if (allMembersImmutable) ImmutableT else MutableT
  }

  def getMutability(interner: Interner, keywords: Keywords, coutputs: CompilerOutputs, structTT: StructTT): ITemplata[MutabilityTemplataType] = {
    val definition = coutputs.lookupStruct(structTT)
    val transformer =
      TemplataCompiler.getPlaceholderSubstituter(interner, keywords, structTT.fullName)
    val result = transformer.substituteForTemplata(definition.mutability)
    ITemplata.expectMutability(result)
  }
}