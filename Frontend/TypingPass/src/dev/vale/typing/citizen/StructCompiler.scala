package dev.vale.typing.citizen

import dev.vale.highertyping.FunctionA
import dev.vale._
import dev.vale.postparsing._
import dev.vale.postparsing.rules.IRulexSR
import dev.vale.typing.ast.{FunctionHeaderT, PrototypeT}
import dev.vale.typing.env.IInDenizenEnvironmentT
import dev.vale.typing._
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.parsing._
import dev.vale.postparsing.patterns.AtomSP
import dev.vale.postparsing.rules._
import dev.vale.typing.env._
import dev.vale.typing.function._
import dev.vale.typing.ast._
import dev.vale.typing.templata.ITemplataT.expectMutability

import scala.collection.immutable.List
import scala.collection.mutable

case class WeakableImplingMismatch(structWeakable: Boolean, interfaceWeakable: Boolean) extends Throwable { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

trait IStructCompilerDelegate {
  def precompileGenericFunction(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    functionTemplata: FunctionTemplataT):
  Unit

  def compileGenericFunction(
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    functionTemplata: FunctionTemplataT):
  FunctionHeaderT

  def scoutExpectedFunctionForPrototype(
    env: IInDenizenEnvironmentT,
    coutputs: CompilerOutputs,
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    functionName: IImpreciseNameS,
    explicitTemplateArgRulesS: Vector[IRulexSR],
    explicitTemplateArgRunesS: Vector[IRuneS],
    contextRegion: RegionT,
    args: Vector[CoordT],
    extraEnvsToLookIn: Vector[IInDenizenEnvironmentT],
    exact: Boolean):
  StampFunctionSuccess
}

sealed trait IResolveOutcome[+T <: KindT] {
  def expect(): ResolveSuccess[T]
}
case class ResolveSuccess[+T <: KindT](kind: T) extends IResolveOutcome[T] {
  override def expect(): ResolveSuccess[T] = this
}
case class ResolveFailure[+T <: KindT](range: List[RangeS], x: IResolvingError) extends IResolveOutcome[T] {
  override def expect(): ResolveSuccess[T] = {
    throw CompileErrorExceptionT(TypingPassResolvingError(range, x))
  }
}

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
    callingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    structTemplata: StructDefinitionTemplataT,
    uncoercedTemplateArgs: Vector[ITemplataT[ITemplataType]]):
  IResolveOutcome[StructTT] = {
    Profiler.frame(() => {
      templateArgsLayer.resolveStruct(
        coutputs, callingEnv, callRange, callLocation, structTemplata, uncoercedTemplateArgs)
    })
  }

  def precompileStruct(
    coutputs: CompilerOutputs,
    packageEnv: PackageEnvironmentT[INameT],
    structTemplata: StructDefinitionTemplataT):
  (IdT[IStructTemplateNameT], () => Unit) = {
    val StructDefinitionTemplataT(declaringEnv, structA) = structTemplata

    val structTemplateId = templataCompiler.resolveStructTemplate(structTemplata)

    // We declare the type and its predicted mutability before this, see SDOSWB.

    // We declare the struct's outer environment this early because of MDATOEF.
    val outerEnv =
      CitizenEnvironmentT(
        declaringEnv.globalEnv,
        declaringEnv,
        structTemplateId,
        structTemplateId,
        TemplatasStore(structTemplateId, Map(), Map())
          .addEntries(
            interner,
            // Merge in any things from the global environment that say they're part of this
            // structs's namespace (see IMRFDI and CODME).
            // StructFreeMacro will put a free function here.
            // DO NOT SUBMIT is this really necessary?
            declaringEnv.globalEnv.nameToTopLevelEnvironment
              .get(structTemplateId.addStep(interner.intern(PackageTopLevelNameT())))
              .toVector
              .flatMap(_.entriesByNameT)))
    coutputs.declareTypeOuterEnv(structTemplateId, outerEnv)

    val innerCompiler = templateArgsLayer.precompileStruct(coutputs, structTemplata)

    val compiler = () => {
      innerCompiler()
      val packageId = packageEnv.id
      val globalEnv = packageEnv.globalEnv

      val maybeExport =
        structA.attributes.collectFirst { case e@ExportS(_) => e }
      maybeExport match {
        case None =>
        case Some(ExportS(packageCoordinate)) => {
          val templateName = interner.intern(ExportTemplateNameT(structA.range.begin))
          val templateId = IdT(packageId.packageCoord, Vector(), templateName)
          val exportOuterEnv =
            ExportEnvironmentT(
              globalEnv, packageEnv, templateId, templateId, TemplatasStore(templateId, Map(), Map()))

          val regionPlaceholder = RegionT()

          val placeholderedExportName = interner.intern(ExportNameT(templateName, RegionT()))
          val placeholderedExportId = templateId.copy(localName = placeholderedExportName)
          val exportEnv =
            ExportEnvironmentT(
              globalEnv, packageEnv, templateId, placeholderedExportId, TemplatasStore(placeholderedExportId, Map(), Map()))

          val exportPlaceholderedStruct =
            resolveStruct(
              coutputs, exportEnv, List(structA.range), LocationInDenizen(Vector()), structTemplata, Vector()) match {
              case ResolveSuccess(kind) => kind
              case ResolveFailure(range, reason) => {
                throw CompileErrorExceptionT(TypingPassResolvingError(range, reason))
              }
            }

          val exportName =
            structA.name match {
              case TopLevelCitizenDeclarationNameS(name, range) => name
              case other => vwat(other)
            }

          coutputs.addKindExport(
            structA.range, exportPlaceholderedStruct, placeholderedExportId, exportName)
        }
      }
    }

    (structTemplateId, compiler)
  }

  def precompileInterface(
    coutputs: CompilerOutputs,
      packageEnv: PackageEnvironmentT[INameT],
    interfaceTemplata: InterfaceDefinitionTemplataT):
  (IdT[IInterfaceTemplateNameT], () => Unit) = {
    val InterfaceDefinitionTemplataT(declaringEnv, interfaceA) = interfaceTemplata

    val interfaceTemplateId = templataCompiler.resolveInterfaceTemplate(interfaceTemplata)

    // We declare the type and its predicted mutability before this, see SDOSWB.

    // We do this here because we might compile a virtual function somewhere before we compile the interface.
    // The virtual function will need to know if the type is sealed to know whether it's allowed to be
    // virtual on this interface.
    coutputs.declareTypeSealed(interfaceTemplateId, interfaceA.attributes.contains(SealedS))


    // We declare the interface's outer environment this early because of MDATOEF.
    val outerEnv =
      CitizenEnvironmentT(
        declaringEnv.globalEnv,
        declaringEnv,
        interfaceTemplateId,
        interfaceTemplateId,
        TemplatasStore(interfaceTemplateId, Map(), Map())
          .addEntries(
            interner,
            // TODO: Take those internal methods that were defined inside the interface, and move them to
            // just be name-prefixed like Free is, see IMRFDI.
            interfaceA.internalMethods
              .map(internalMethod => {
                val functionName = nameTranslator.translateGenericFunctionName(internalMethod.name)
                (functionName -> FunctionEnvEntry(internalMethod))
              }) ++
              // Merge in any things from the global environment that say they're part of this
              // interface's namespace (see IMRFDI and CODME).
              declaringEnv.globalEnv.nameToTopLevelEnvironment
                .get(interfaceTemplateId.addStep(interner.intern(PackageTopLevelNameT())))
                .toVector
                .flatMap(_.entriesByNameT)))
    coutputs.declareTypeOuterEnv(interfaceTemplateId, outerEnv)

    val innerCompiler = templateArgsLayer.precompileInterface(coutputs, interfaceTemplata)

    val compiler = () => {
      innerCompiler()
      val packageId = packageEnv.id
      val globalEnv = packageEnv.globalEnv

      val maybeExport =
        interfaceA.attributes.collectFirst { case e@ExportS(_) => e }
      maybeExport match {
        case None =>
        case Some(ExportS(packageCoordinate)) => {
          val templateName = interner.intern(ExportTemplateNameT(interfaceA.range.begin))
          val templateId = IdT(packageId.packageCoord, Vector(), templateName)
          val exportOuterEnv =
            ExportEnvironmentT(
              globalEnv, packageEnv, templateId, templateId, TemplatasStore(templateId, Map(), Map()))

          val placeholderedExportName = interner.intern(ExportNameT(templateName, RegionT()))
          val placeholderedExportId = templateId.copy(localName = placeholderedExportName)
          val exportEnv =
            ExportEnvironmentT(
              globalEnv, packageEnv, templateId, placeholderedExportId, TemplatasStore(placeholderedExportId, Map(), Map()))

          val exportPlaceholderedKind =
            resolveInterface(
              coutputs, exportEnv, List(interfaceA.range), LocationInDenizen(Vector()), interfaceTemplata, Vector()) match {
              case ResolveSuccess(kind) => kind
              case ResolveFailure(range, reason) => {
                throw CompileErrorExceptionT(TypingPassResolvingError(range, reason))
              }
            }

          val exportName =
            interfaceA.name match {
              case TopLevelCitizenDeclarationNameS(name, range) => name
              case other => vwat(other)
            }

          coutputs.addKindExport(
            interfaceA.range, exportPlaceholderedKind, placeholderedExportId, exportName)
        }
      }
    }

    (interfaceTemplateId, compiler)
  }

  // See SFWPRL for how this is different from resolveInterface.
  def predictInterface(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceDefinitionTemplataT,
    uncoercedTemplateArgs: Vector[ITemplataT[ITemplataType]]):
  (InterfaceTT) = {
    templateArgsLayer.predictInterface(
      coutputs, callingEnv, callRange, callLocation, interfaceTemplata, uncoercedTemplateArgs)
  }

  // See SFWPRL for how this is different from resolveStruct.
  def predictStruct(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    structTemplata: StructDefinitionTemplataT,
    uncoercedTemplateArgs: Vector[ITemplataT[ITemplataType]]):
  (StructTT) = {
    templateArgsLayer.predictStruct(
      coutputs, callingEnv, callRange, callLocation, structTemplata, uncoercedTemplateArgs)
  }

  def resolveInterface(
    coutputs: CompilerOutputs,
    callingEnv: IInDenizenEnvironmentT, // See CSSNCE
    callRange: List[RangeS],
    callLocation: LocationInDenizen,
    // We take the entire templata (which includes environment and parents) so we can incorporate
    // their rules as needed
    interfaceTemplata: InterfaceDefinitionTemplataT,
    uncoercedTemplateArgs: Vector[ITemplataT[ITemplataType]]):
  IResolveOutcome[InterfaceTT] = {
    val success =
      templateArgsLayer.resolveInterface(
        coutputs, callingEnv, callRange, callLocation, interfaceTemplata, uncoercedTemplateArgs)

    success
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: NodeEnvironmentT,
    coutputs: CompilerOutputs,
    parentRanges: List[RangeS],
    callLocation: LocationInDenizen,
    name: IFunctionDeclarationNameS,
    functionS: FunctionA,
    members: Vector[NormalStructMemberT]):
  (StructTT, MutabilityT, FunctionTemplataT) = {
//    Profiler.reentrant("StructCompiler-makeClosureUnderstruct", name.codeLocation.toString, () => {
      templateArgsLayer.makeClosureUnderstruct(containingFunctionEnv, coutputs, parentRanges, callLocation, name, functionS, members)
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

  def getMutability(
    sanityCheck: Boolean,
    interner: Interner,
    keywords: Keywords,
    coutputs: CompilerOutputs,
    originalCallingDenizenId: IdT[ITemplateNameT],
    region: RegionT,
    structTT: StructTT,
    boundArgumentsSource: IBoundArgumentsSource):
  ITemplataT[MutabilityTemplataType] = {
    val definition = coutputs.lookupStruct(structTT.id)
    val transformer =
      TemplataCompiler.getPlaceholderSubstituter(
        sanityCheck,
        interner, keywords,
        originalCallingDenizenId,
        structTT.id, boundArgumentsSource)
    val result = transformer.substituteForTemplata(coutputs, definition.mutability)
    ITemplataT.expectMutability(result)
  }
}