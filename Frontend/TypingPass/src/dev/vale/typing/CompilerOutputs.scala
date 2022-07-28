package dev.vale.typing

import dev.vale.postparsing.{IntegerTemplataType, MutabilityTemplataType, VariabilityTemplataType}
import dev.vale.typing.ast.{FunctionExportT, FunctionExternT, FunctionT, ImplT, KindExportT, KindExternT, PrototypeT, SignatureT, getFunctionLastName}
import dev.vale.typing.env.{CitizenEnvironment, FunctionEnvironment, IEnvironment}
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.names.{AnonymousSubstructNameT, AnonymousSubstructTemplateNameT, CitizenTemplateNameT, FreeNameT, FullNameT, ICitizenTemplateNameT, IFunctionNameT, IInterfaceTemplateNameT, INameT, IStructTemplateNameT, ITemplateNameT, InterfaceTemplateNameT, StructTemplateNameT}
import dev.vale.typing.types._
import dev.vale.{Collector, PackageCoordinate, RangeS, StrI, vassert, vassertOne, vassertSome, vfail, vpass}
import dev.vale.typing.ast._
import dev.vale.typing.templata.{ITemplata, MutabilityTemplata}
import dev.vale.typing.types.InterfaceTT

import scala.collection.immutable.{List, Map}
import scala.collection.mutable


case class DeferredEvaluatingFunction(
  prototypeT: PrototypeT,
  call: (CompilerOutputs) => Unit)


case class CompilerOutputs() {
  // Removed this because generics and removing infer-ret solved it
  //  // Signatures that have already started to be compiled.
  //  // The value is a location for checking where a given function came from, which is useful
  //  // for detecting when the user makes two functions with identical signatures.
  //  private val declaredSignatures: mutable.HashMap[SignatureT, RangeS] = mutable.HashMap()

  // Not all signatures/banners will have a return type here, it might not have been processed yet.
  private val returnTypesBySignature: mutable.HashMap[SignatureT, CoordT] = mutable.HashMap()

  // Not all signatures/banners or even return types will have a function here, it might not have
  // been processed yet.
  private val functionsBySignature: mutable.HashMap[SignatureT, FunctionT] = mutable.HashMap()
  private val functionsByPrototype: mutable.HashMap[PrototypeT, FunctionT] = mutable.HashMap()
  private val envByFunctionSignature: mutable.HashMap[SignatureT, FunctionEnvironment] = mutable.HashMap()

  // One must fill this in when putting things into declaredTemplateNames.
  private val templateNameToMutability: mutable.HashMap[FullNameT[ITemplateNameT], ITemplata[MutabilityTemplataType]] = mutable.HashMap()

  // declaredTemplateNames is the structs that we're currently in the process of defining
  // Things will appear here before they appear in structTemplateNameToDefinition/interfaceTemplateNameToDefinition
  // This is to prevent infinite recursion / stack overflow when typingpassing recursive types
  private val declaredTemplateNames: mutable.HashSet[FullNameT[ITemplateNameT]] = mutable.HashSet()
  // Outer env is the env that contains the template.
  private val templateNameToOuterEnv: mutable.HashMap[FullNameT[ITemplateNameT], IEnvironment] = mutable.HashMap()
  // Inner env is the env that contains the solved rules for the declaration, given placeholders.
  private val templateNameToInnerEnv: mutable.HashMap[FullNameT[ITemplateNameT], IEnvironment] = mutable.HashMap()
  private val structTemplateNameToDefinition: mutable.HashMap[FullNameT[IStructTemplateNameT], StructDefinitionT] = mutable.HashMap()
  private val interfaceTemplateNameToDefinition: mutable.HashMap[FullNameT[IInterfaceTemplateNameT], InterfaceDefinitionT] = mutable.HashMap()

  // This is a HashSet to help deduplicate, see CIFBD.
  private val allImpls: mutable.HashSet[ImplT] = mutable.HashSet()
  private val subCitizenTemplateToImpls: mutable.HashMap[FullNameT[ICitizenTemplateNameT], Vector[ImplT]] = mutable.HashMap()
  private val superInterfaceTemplateToImpls: mutable.HashMap[FullNameT[IInterfaceTemplateNameT], Vector[ImplT]] = mutable.HashMap()

  private val kindExports: mutable.ArrayBuffer[KindExportT] = mutable.ArrayBuffer()
  private val functionExports: mutable.ArrayBuffer[FunctionExportT] = mutable.ArrayBuffer()
  private val kindExterns: mutable.ArrayBuffer[KindExternT] = mutable.ArrayBuffer()
  private val functionExterns: mutable.ArrayBuffer[FunctionExternT] = mutable.ArrayBuffer()

//  // Only ArrayCompiler can make an RawArrayT2.
//  private val staticSizedArrayTypes:
//    mutable.HashMap[(ITemplata[IntegerTemplataType], ITemplata[MutabilityTemplataType], ITemplata[VariabilityTemplataType], CoordT), StaticSizedArrayTT] =
//    mutable.HashMap()
//  // Only ArrayCompiler can make an RawArrayT2.
//  private val runtimeSizedArrayTypes: mutable.HashMap[(ITemplata[MutabilityTemplataType], CoordT), RuntimeSizedArrayTT] = mutable.HashMap()

//  // A queue of functions that our code uses, but we don't need to compile them right away.
//  // We can compile them later. Perhaps in parallel, someday!
//  private val deferredEvaluatingFunctions: mutable.LinkedHashMap[PrototypeT, DeferredEvaluatingFunction] = mutable.LinkedHashMap()
//  private var evaluatedDeferredFunctions: mutable.LinkedHashSet[PrototypeT] = mutable.LinkedHashSet()

  def countDenizens(): Int = {
//    staticSizedArrayTypes.size +
//      runtimeSizedArrayTypes.size +
      functionsBySignature.size +
      structTemplateNameToDefinition.size +
      interfaceTemplateNameToDefinition.size
  }

//  def peekNextDeferredEvaluatingFunction(): Option[DeferredEvaluatingFunction] = {
//    deferredEvaluatingFunctions.headOption.map(_._2)
//  }
//  def markDeferredFunctionEvaluated(prototypeT: PrototypeT): Unit = {
//    vassert(prototypeT == vassertSome(deferredEvaluatingFunctions.headOption)._1)
//    evaluatedDeferredFunctions += prototypeT
//    deferredEvaluatingFunctions -= prototypeT
//  }

  def lookupFunction(signature2: SignatureT): Option[FunctionT] = {
    functionsBySignature.get(signature2)
  }

  def findImmDestructor(kind: KindT): PrototypeT = {
    vassertOne(
      functionsBySignature.values
        .filter({
//          case getFunctionLastName(DropNameT(_, CoordT(_, _, k))) if k == kind => true
          case getFunctionLastName(FreeNameT(_, _, k)) if k == kind => true
          case _ => false
        }))
      .header.toPrototype
  }

//  // This means we've at least started to evaluate this function's body.
//  // We use this to cut short any infinite looping that might happen when,
//  // for example, there's a recursive function call.
//  def declareFunctionSignature(range: RangeS, signature: SignatureT, maybeEnv: Option[FunctionEnvironment]): Unit = {
//    // The only difference between this and declareNonGlobalFunctionSignature is
//    // that we put an environment in here.
//
//    // This should have been checked outside
//    vassert(!declaredSignatures.contains(signature))
//
//    declaredSignatures += signature -> range
//    envByFunctionSignature ++= maybeEnv.map(env => Map(signature -> env)).getOrElse(Map())
//    this
//  }

  def declareFunctionReturnType(signature: SignatureT, returnType2: CoordT): Unit = {
    returnTypesBySignature.get(signature) match {
      case None =>
      case Some(existingReturnType2) => vassert(existingReturnType2 == returnType2)
    }
//    if (!declaredSignatures.contains(signature)) {
//      vfail("wot")
//    }
    returnTypesBySignature += (signature -> returnType2)
  }

  def addFunction(function: FunctionT): Unit = {
//    vassert(declaredSignatures.contains(function.header.toSignature))
    vassert(
      function.body.result.reference.kind == NeverT(false) ||
      function.body.result.reference == function.header.returnType)
//    if (!useOptimization) {
//      Collector.all(function, {
//        case ReturnTE(innerExpr) => {
//          vassert(
//            innerExpr.result.reference.kind == NeverT(false) ||
//              innerExpr.result.reference == function.header.returnType)
//        }
//      })
//    }

    if (functionsByPrototype.contains(function.header.toPrototype)) {
      vfail("wot")
    }
    if (functionsBySignature.contains(function.header.toSignature)) {
      vfail("wot")
    }

    functionsBySignature.put(function.header.toSignature, function)
    functionsByPrototype.put(function.header.toPrototype, function)
  }

  // We can't declare the struct at the same time as we declare its mutability or environment,
  // see MFDBRE.
  def declareTemplate(templateName: FullNameT[ITemplateNameT]): Unit = {
    vassert(!declaredTemplateNames.contains(templateName))
    declaredTemplateNames += templateName
  }

  def declareTemplateMutability(
    templateName: FullNameT[ITemplateNameT],
    mutability: ITemplata[MutabilityTemplataType]
  ): Unit = {
    vassert(declaredTemplateNames.contains(templateName))
    vassert(!templateNameToMutability.contains(templateName))
    templateNameToMutability += (templateName -> mutability)
  }

  def declareOuterEnvForTemplate(
    templateNameT: FullNameT[ITemplateNameT],
    env: IEnvironment,
  ): Unit = {
    vassert(declaredTemplateNames.contains(templateNameT))
    vassert(!templateNameToOuterEnv.contains(templateNameT))
    vassert(templateNameT == env.fullName)
    templateNameToOuterEnv += (templateNameT -> env)
  }

  def declareInnerEnvForTemplate(
    templateNameT: FullNameT[ITemplateNameT],
    env: IEnvironment,
  ): Unit = {
    vassert(declaredTemplateNames.contains(templateNameT))
    // One should declare the outer env first
    vassert(templateNameToOuterEnv.contains(templateNameT))
    vassert(!templateNameToInnerEnv.contains(templateNameT))
    vassert(templateNameT == env.fullName)
    templateNameToInnerEnv += (templateNameT -> env)
  }

  def add(structDef: StructDefinitionT): Unit = {
    if (structDef.mutability == MutabilityTemplata(ImmutableT)) {
      if (structDef.members.exists(_.tyype.reference.unsubstitutedCoord.ownership != ShareT)) {
        vfail("ImmutableP contains a non-immutable!")
      }
    }
    vassert(!structTemplateNameToDefinition.contains(structDef.templateName))
    structTemplateNameToDefinition += (structDef.templateName -> structDef)
  }

  def add(interfaceDef: InterfaceDefinitionT): Unit = {
    vassert(!interfaceTemplateNameToDefinition.contains(interfaceDef.templateName))
    interfaceTemplateNameToDefinition += (interfaceDef.templateName -> interfaceDef)
  }

//  def addStaticSizedArray(ssaTT: StaticSizedArrayTT): Unit = {
//    val StaticSizedArrayTT(size, elementType, mutability, variability) = ssaTT
//    staticSizedArrayTypes += ((size, elementType, mutability, variability) -> ssaTT)
//  }
//
//  def addRuntimeSizedArray(rsaTT: RuntimeSizedArrayTT): Unit = {
//    val RuntimeSizedArrayTT(elementType, mutability) = rsaTT
//    runtimeSizedArrayTypes += ((elementType, mutability) -> rsaTT)
//  }

  def addImpl(impl: ImplT): Unit = {
    allImpls += impl
    subCitizenTemplateToImpls.put(
      impl.subCitizenTemplateName,
      subCitizenTemplateToImpls.getOrElse(impl.subCitizenTemplateName, Vector()) :+ impl)
    superInterfaceTemplateToImpls.put(
      impl.superInterfaceTemplateName,
      superInterfaceTemplateToImpls.getOrElse(impl.superInterfaceTemplateName, Vector()) :+ impl)
  }

  def getParentImplsForSubCitizenTemplate(subCitizenTemplate: FullNameT[ICitizenTemplateNameT]): Vector[ImplT] = {
    subCitizenTemplateToImpls.getOrElse(subCitizenTemplate, Vector[ImplT]())
  }
  def getChildImplsForSuperInterfaceTemplate(superInterfaceTemplate: FullNameT[IInterfaceTemplateNameT]): Vector[ImplT] = {
    superInterfaceTemplateToImpls.getOrElse(superInterfaceTemplate, Vector[ImplT]())
  }

  def addKindExport(range: RangeS, kind: KindT, packageCoord: PackageCoordinate, exportedName: StrI): Unit = {
    kindExports += KindExportT(range, kind, packageCoord, exportedName)
  }

  def addFunctionExport(range: RangeS, function: PrototypeT, packageCoord: PackageCoordinate, exportedName: StrI): Unit = {
    functionExports += FunctionExportT(range, function, packageCoord, exportedName)
  }

  def addKindExtern(kind: KindT, packageCoord: PackageCoordinate, exportedName: StrI): Unit = {
    kindExterns += KindExternT(kind, packageCoord, exportedName)
  }

  def addFunctionExtern(range: RangeS, function: PrototypeT, packageCoord: PackageCoordinate, exportedName: StrI): Unit = {
    functionExterns += FunctionExternT(range, function, packageCoord, exportedName)
  }

//  def deferEvaluatingFunction(devf: DeferredEvaluatingFunction): Unit = {
//    deferredEvaluatingFunctions.put(devf.prototypeT, devf)
//  }

  def structDeclared(templateName: FullNameT[IStructTemplateNameT]): Boolean = {
    // This is the only place besides StructDefinition2 and declareStruct thats allowed to make one of these
//    val templateName = StructTT(fullName)
    declaredTemplateNames.contains(templateName)
  }

//  def prototypeDeclared(fullName: FullNameT[IFunctionNameT]): Option[PrototypeT] = {
//    declaredSignatures.find(_._1.fullName == fullName) match {
//      case None => None
//      case Some((sig, _)) => {
//        returnTypesBySignature.get(sig) match {
//          case None => None
//          case Some(ret) => Some(ast.PrototypeT(sig.fullName, ret))
//        }
//      }
//    }
//  }

  def lookupMutability(templateName: FullNameT[ITemplateNameT]): ITemplata[MutabilityTemplataType] = {
    // If it has a structTT, then we've at least started to evaluate this citizen
    templateNameToMutability.get(templateName) match {
      case None => vfail("Still figuring out mutability for struct: " + templateName) // See MFDBRE
      case Some(m) => m
    }
  }

//  def lookupCitizen(citizenRef: CitizenRefT): CitizenDefinitionT = {
//    citizenRef match {
//      case s @ StructTT(_) => lookupStruct(s)
//      case i @ InterfaceTT(_) => lookupInterface(i)
//    }
//  }

  def interfaceDeclared(templateName: FullNameT[ITemplateNameT]): Boolean = {
    // This is the only place besides InterfaceDefinition2 and declareInterface thats allowed to make one of these
    declaredTemplateNames.contains(templateName)
  }

  def lookupStruct(structTT: StructTT): StructDefinitionT = {
    lookupStruct(TemplataCompiler.getStructTemplate(structTT.fullName))
  }
  def lookupStruct(templateName: FullNameT[IStructTemplateNameT]): StructDefinitionT = {
    vassertSome(structTemplateNameToDefinition.get(templateName))
  }
  def lookupInterface(interfaceTT: InterfaceTT): InterfaceDefinitionT = {
    lookupInterface(TemplataCompiler.getInterfaceTemplate(interfaceTT.fullName))
  }
  def lookupInterface(templateName: FullNameT[IInterfaceTemplateNameT]): InterfaceDefinitionT = {
    vassertSome(interfaceTemplateNameToDefinition.get(templateName))
  }
  def lookupCitizen(templateName: FullNameT[ICitizenTemplateNameT]): CitizenDefinitionT = {
    val FullNameT(packageCoord, initSteps, last) = templateName
    last match {
      case s @ AnonymousSubstructTemplateNameT(_) => lookupStruct(FullNameT(packageCoord, initSteps, s))
      case s @ StructTemplateNameT(_) => lookupStruct(FullNameT(packageCoord, initSteps, s))
      case s @ InterfaceTemplateNameT(_) => lookupInterface(FullNameT(packageCoord, initSteps, s))
    }
  }
  def lookupCitizen(citizenTT: ICitizenTT): CitizenDefinitionT = {
    citizenTT match {
      case s @ StructTT(_) => lookupStruct(s)
      case s @ InterfaceTT(_) => lookupInterface(s)
    }
  }

  def getAllStructs(): Iterable[StructDefinitionT] = structTemplateNameToDefinition.values
  def getAllInterfaces(): Iterable[InterfaceDefinitionT] = interfaceTemplateNameToDefinition.values
  def getAllFunctions(): Iterable[FunctionT] = functionsBySignature.values
  def getAllImpls(): Iterable[ImplT] = allImpls
//  def getAllStaticSizedArrays(): Iterable[StaticSizedArrayTT] = staticSizedArrayTypes.values
//  def getAllRuntimeSizedArrays(): Iterable[RuntimeSizedArrayTT] = runtimeSizedArrayTypes.values
//  def getKindToDestructorMap(): Map[KindT, PrototypeT] = kindToDestructor.toMap

//  def getStaticSizedArrayType(size: ITemplata[IntegerTemplataType], mutability: ITemplata[MutabilityTemplataType], variability: ITemplata[VariabilityTemplataType], elementType: CoordT): Option[StaticSizedArrayTT] = {
//    staticSizedArrayTypes.get((size, mutability, variability, elementType))
//  }
  def getEnvForFunctionSignature(sig: SignatureT): FunctionEnvironment = {
    vassertSome(envByFunctionSignature.get(sig))
  }
  def getOuterEnvForTemplate(name: FullNameT[ITemplateNameT]): IEnvironment = {
    vassertSome(templateNameToOuterEnv.get(name))
  }
  def getInnerEnvForTemplate(name: FullNameT[ITemplateNameT]): IEnvironment = {
    vassertSome(templateNameToInnerEnv.get(name))
  }
  def getReturnTypeForSignature(sig: SignatureT): Option[CoordT] = {
    returnTypesBySignature.get(sig)
  }
//  def getDeclaredSignatureOrigin(sig: SignatureT): Option[RangeS] = {
//    declaredSignatures.get(sig)
//  }
//  def getDeclaredSignatureOrigin(name: FullNameT[IFunctionNameT]): Option[RangeS] = {
//    declaredSignatures.get(ast.SignatureT(name))
//  }
//  def getRuntimeSizedArray(mutabilityT: ITemplata[MutabilityTemplataType], elementType: CoordT): Option[RuntimeSizedArrayTT] = {
//    runtimeSizedArrayTypes.get((mutabilityT, elementType))
//  }
  def getKindExports: Vector[KindExportT] = {
    kindExports.toVector
  }
  def getFunctionExports: Vector[FunctionExportT] = {
    functionExports.toVector
  }
  def getKindExterns: Vector[KindExternT] = {
    kindExterns.toVector
  }
  def getFunctionExterns: Vector[FunctionExternT] = {
    functionExterns.toVector
  }
}
