package net.verdagon.vale.templar

import net.verdagon.vale.templar.ast.{FunctionExportT, FunctionExternT, FunctionT, ImplT, KindExportT, KindExternT, PrototypeT, ReturnTE, SignatureT, getFunctionLastName}
import net.verdagon.vale.templar.env.{CitizenEnvironment, FunctionEnvironment, PackageEnvironment}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.names.{CitizenNameT, DropNameT, FullNameT, FunctionNameT, ICitizenNameT, IFunctionNameT, INameT}
import net.verdagon.vale.templar.types.{CitizenDefinitionT, CitizenRefT, CoordT, ImmutableT, InterfaceDefinitionT, InterfaceTT, KindT, MutabilityT, NeverT, RawArrayTT, RuntimeSizedArrayTT, ShareT, StaticSizedArrayTT, StructDefinitionT, StructTT}
import net.verdagon.vale.{Collector, PackageCoordinate, RangeS, vassert, vassertOne, vassertSome, vfail}

import scala.collection.immutable.{List, Map}
import scala.collection.mutable


case class DeferredEvaluatingFunction(
  prototypeT: PrototypeT,
  call: (Temputs) => Unit)


case class Temputs() {
  // Signatures that have already started to be compiled.
  // The value is a location for checking where a given function came from, which is useful
  // for detecting when the user makes two functions with identical signatures.
  private val declaredSignatures: mutable.HashMap[SignatureT, RangeS] = mutable.HashMap()

  // Not all signatures/banners will have a return type here, it might not have been processed yet.
  private val returnTypesBySignature: mutable.HashMap[SignatureT, CoordT] = mutable.HashMap()

  // Not all signatures/banners or even return types will have a function here, it might not have
  // been processed yet.
  private val functions: mutable.ArrayBuffer[FunctionT] = mutable.ArrayBuffer()
  private val envByFunctionSignature: mutable.HashMap[SignatureT, FunctionEnvironment] = mutable.HashMap()

//  // Prototypes for extern functions
//  private val packageToExternNameToExtern: mutable.HashMap[PackageCoordinate, mutable.HashMap[String, Prototype2]] = mutable.HashMap()

  // One must fill this in when putting things into declaredKinds.
  private val mutabilitiesByCitizenRef: mutable.HashMap[CitizenRefT, MutabilityT] = mutable.HashMap()

  // declaredKinds is the structs that we're currently in the process of defining
  // Things will appear here before they appear in structDefsByRef/interfaceDefsByRef
  // This is to prevent infinite recursion / stack overflow when templaring recursive types
  private val declaredKinds: mutable.HashSet[KindT] = mutable.HashSet()
  private val structDefsByRef: mutable.HashMap[StructTT, StructDefinitionT] = mutable.HashMap()
  private val envByKind: mutable.HashMap[KindT, CitizenEnvironment[INameT]] = mutable.HashMap()
  private val interfaceDefsByRef: mutable.HashMap[InterfaceTT, InterfaceDefinitionT] = mutable.HashMap()

  private val impls: mutable.ArrayBuffer[ImplT] = mutable.ArrayBuffer()

  private val kindExports: mutable.ArrayBuffer[KindExportT] = mutable.ArrayBuffer()
  private val functionExports: mutable.ArrayBuffer[FunctionExportT] = mutable.ArrayBuffer()
  private val kindExterns: mutable.ArrayBuffer[KindExternT] = mutable.ArrayBuffer()
  private val functionExterns: mutable.ArrayBuffer[FunctionExternT] = mutable.ArrayBuffer()

  // Only PackTemplar can make a PackT2.
  private val packTypes: mutable.HashMap[Vector[CoordT], StructTT] = mutable.HashMap()
  // Only ArrayTemplar can make an RawArrayT2.
  private val staticSizedArrayTypes: mutable.HashMap[(Int, RawArrayTT), StaticSizedArrayTT] = mutable.HashMap()
  // Only ArrayTemplar can make an RawArrayT2.
  private val runtimeSizedArrayTypes: mutable.HashMap[RawArrayTT, RuntimeSizedArrayTT] = mutable.HashMap()

//  private val kindToDestructor: mutable.HashMap[KindT, PrototypeT] = mutable.HashMap()

  // A queue of functions that our code uses, but we don't need to compile them right away.
  // We can compile them later. Perhaps in parallel, someday!
  private val deferredEvaluatingFunctions: mutable.LinkedHashMap[PrototypeT, DeferredEvaluatingFunction] = mutable.LinkedHashMap()
  private var evaluatedDeferredFunctions: mutable.LinkedHashSet[PrototypeT] = mutable.LinkedHashSet()

  def peekNextDeferredEvaluatingFunction(): Option[DeferredEvaluatingFunction] = {
    deferredEvaluatingFunctions.headOption.map(_._2)
  }
  def markDeferredFunctionEvaluated(prototypeT: PrototypeT): Unit = {
    vassert(prototypeT == vassertSome(deferredEvaluatingFunctions.headOption)._1)
    evaluatedDeferredFunctions += prototypeT
    deferredEvaluatingFunctions -= prototypeT
  }

  def lookupFunction(signature2: SignatureT): Option[FunctionT] = {
    functions.find(_.header.toSignature == signature2)
  }

  def findDestructor(kind: KindT): PrototypeT = {
    vassertOne(
      functions
        .filter({
          case getFunctionLastName(DropNameT(_, CoordT(_, _, k))) if k == kind => true
          case getFunctionLastName(FunctionNameT(name, _, Vector(CoordT(_, _, k))))
            if name == CallTemplar.DROP_FUNCTION_NAME && k == kind => true
          case _ => false
        }))
      .header.toPrototype
  }

  // This means we've at least started to evaluate this function's body.
  // We use this to cut short any infinite looping that might happen when,
  // for example, there's a recursive function call.
  def declareFunctionSignature(range: RangeS, signature: SignatureT, maybeEnv: Option[FunctionEnvironment]): Unit = {
    // The only difference between this and declareNonGlobalFunctionSignature is
    // that we put an environment in here.

    // This should have been checked outside
    vassert(!declaredSignatures.contains(signature))

    declaredSignatures += signature -> range
    envByFunctionSignature ++= maybeEnv.map(env => Map(signature -> env)).getOrElse(Map())
    this
  }

  def declareFunctionReturnType(signature: SignatureT, returnType2: CoordT): Unit = {
    returnTypesBySignature.get(signature) match {
      case None =>
      case Some(existingReturnType2) => vassert(existingReturnType2 == returnType2)
    }
    if (!declaredSignatures.contains(signature)) {
      vfail("wot")
    }
    returnTypesBySignature += (signature -> returnType2)
  }

  def addFunction(function: FunctionT): Unit = {
    vassert(declaredSignatures.contains(function.header.toSignature))
    vassert(
      function.body.resultRegister.reference.kind == NeverT() ||
      function.body.resultRegister.reference == function.header.returnType)
    Collector.all(function, {
      case ReturnTE(innerExpr) => {
        vassert(
          innerExpr.resultRegister.reference.kind == NeverT() ||
          innerExpr.resultRegister.reference == function.header.returnType)
      }
    })

    if (functions.exists(_.header == function.header)) {
      vfail("wot")
    }

    functions += function
  }

  // We can't declare the struct at the same time as we declare its mutability or environment,
  // see MFDBRE.
  def declareStruct(
    structTT: StructTT
  ): Unit = {
    vassert(!declaredKinds.contains(structTT))
    declaredKinds += structTT
  }

  // We can't declare the struct at the same time as we declare its mutability or environment,
  // see MFDBRE.
  def declareInterface(
    interfaceTT: InterfaceTT
  ): Unit = {
    vassert(!declaredKinds.contains(interfaceTT))
    declaredKinds += interfaceTT
  }

  def declareCitizenMutability(
    kindTT: CitizenRefT,
    mutability: MutabilityT
  ): Unit = {
    vassert(declaredKinds.contains(kindTT))
    vassert(!mutabilitiesByCitizenRef.contains(kindTT))
    mutabilitiesByCitizenRef += (kindTT -> mutability)
  }

  def declareKindEnv(
    kindTT: KindT,
    env: CitizenEnvironment[INameT],
  ): Unit = {
    vassert(declaredKinds.contains(kindTT))
    vassert(!envByKind.contains(kindTT))
    envByKind += (kindTT -> env)
  }

  def declarePack(members: Vector[CoordT], understructTT: StructTT): Unit = {
    packTypes += (members -> understructTT)
  }

  def add(structDef: StructDefinitionT): Unit = {
    if (structDef.mutability == ImmutableT) {
      if (structDef.members.exists(_.tyype.reference.ownership != ShareT)) {
        vfail("ImmutableP contains a non-immutable!")
      }
    }
    vassert(!structDefsByRef.contains(structDef.getRef))
    structDefsByRef += (structDef.getRef -> structDef)
  }

  def add(interfaceDef: InterfaceDefinitionT): Unit = {
    vassert(!interfaceDefsByRef.contains(interfaceDef.getRef))
    interfaceDefsByRef += (interfaceDef.getRef -> interfaceDef)
  }

  def addStaticSizedArray(array2: StaticSizedArrayTT): Unit = {
    staticSizedArrayTypes += ((array2.size, array2.array) -> array2)
  }

  def addRuntimeSizedArray(array2: RuntimeSizedArrayTT): Unit = {
    runtimeSizedArrayTypes += (array2.array -> array2)
  }

  def addImpl(structTT: StructTT, interfaceTT: InterfaceTT): Unit = {
    impls += ImplT(structTT, interfaceTT)
  }

  def addKindExport(range: RangeS, kind: KindT, packageCoord: PackageCoordinate, exportedName: String): Unit = {
    kindExports += KindExportT(range, kind, packageCoord, exportedName)
  }

  def addFunctionExport(range: RangeS, function: PrototypeT, packageCoord: PackageCoordinate, exportedName: String): Unit = {
    functionExports += FunctionExportT(range, function, packageCoord, exportedName)
  }

  def addKindExtern(kind: KindT, packageCoord: PackageCoordinate, exportedName: String): Unit = {
    kindExterns += KindExternT(kind, packageCoord, exportedName)
  }

  def addFunctionExtern(range: RangeS, function: PrototypeT, packageCoord: PackageCoordinate, exportedName: String): Unit = {
    functionExterns += FunctionExternT(range, function, packageCoord, exportedName)
  }

//  def addDestructor(kind: KindT, destructor: PrototypeT): Unit = {
//    vassert(!kindToDestructor.contains(kind))
//    vassert(prototypeDeclared(destructor.fullName).nonEmpty)
//    kindToDestructor.put(kind, destructor)
//  }

  def deferEvaluatingFunction(devf: DeferredEvaluatingFunction): Unit = {
    deferredEvaluatingFunctions.put(devf.prototypeT, devf)
  }

//  def getDestructor(kind: KindT): PrototypeT = {
//    vassertSome(kindToDestructor.get(kind))
//  }

  def structDeclared(fullName: FullNameT[ICitizenNameT]): Option[StructTT] = {
    // This is the only place besides StructDefinition2 and declareStruct thats allowed to make one of these
    val structTT = StructTT(fullName)
    if (declaredKinds.contains(structTT)) {
      Some(structTT)
    } else {
      None
    }
  }

  def prototypeDeclared(fullName: FullNameT[IFunctionNameT]): Option[PrototypeT] = {
    declaredSignatures.find(_._1.fullName == fullName) match {
      case None => None
      case Some((sig, _)) => {
        returnTypesBySignature.get(sig) match {
          case None => None
          case Some(ret) => Some(PrototypeT(sig.fullName, ret))
        }
      }
    }
  }

  def lookupMutability(citizenRef2: CitizenRefT): MutabilityT = {
    // If it has a structTT, then we've at least started to evaluate this citizen
    mutabilitiesByCitizenRef.get(citizenRef2) match {
      case None => vfail("Still figuring out mutability for struct: " + citizenRef2) // See MFDBRE
      case Some(m) => m
    }
  }

  def lookupStruct(structTT: StructTT): StructDefinitionT = {
    // If it has a structTT, then we're done (or at least have started) stamping it
    // If this throws an error, then you should not use this function, you should
    // do structDefsByRef.get(structTT) yourself and handle the None case
    vassertSome(structDefsByRef.get(structTT))
  }

  def lookupCitizen(citizenRef: CitizenRefT): CitizenDefinitionT = {
    citizenRef match {
      case s @ StructTT(_) => lookupStruct(s)
      case i @ InterfaceTT(_) => lookupInterface(i)
    }
  }

  def interfaceDeclared(fullName: FullNameT[CitizenNameT]): Option[InterfaceTT] = {
    // This is the only place besides InterfaceDefinition2 and declareInterface thats allowed to make one of these
    val interfaceTT = InterfaceTT(fullName)
    if (declaredKinds.contains(interfaceTT)) {
      Some(interfaceTT)
    } else {
      None
    }
  }

  def lookupInterface(interfaceTT: InterfaceTT): InterfaceDefinitionT = {
    // If it has a interfaceTT, then we're done (or at least have started) stamping it.
    // If this throws an error, then you should not use this function, you should
    // do interfaceDefsByRef.get(interfaceTT) yourself and handle the None case
    interfaceDefsByRef(interfaceTT)
  }
//
//  def functionAlreadyDeclared(rangeS: RangeS, fullName: FullName2[IFunctionName2]): Boolean = {
//    declaredSignatures.get(Signature2(fullName)) == Some(rangeS)
//  }
//
//  def functionAlreadyDeclared(rangeS: RangeS, signature: Signature2): Boolean = {
//    declaredSignatures.contains(signature) == Some(rangeS)
//  }

  //  def findFunction(name: String, paramTypes: Vector[Coord]): Option[FunctionHeader2] = {
  //    val matchingFunctions = functions.find(this, name, paramTypes)
  //    vassert(matchingFunctions.size < 2)
  //    matchingFunctions.headOption
  //  }

  def getAllStructs(): Iterable[StructDefinitionT] = structDefsByRef.values
  def getAllInterfaces(): Iterable[InterfaceDefinitionT] = interfaceDefsByRef.values
  def getAllFunctions(): Iterable[FunctionT] = functions
  def getAllImpls(): Iterable[ImplT] = impls
  def getAllStaticSizedArrays(): Iterable[StaticSizedArrayTT] = staticSizedArrayTypes.values
  def getAllRuntimeSizedArrays(): Iterable[RuntimeSizedArrayTT] = runtimeSizedArrayTypes.values
//  def getKindToDestructorMap(): Map[KindT, PrototypeT] = kindToDestructor.toMap

  def getStaticSizedArrayType(size: Int, array: RawArrayTT): Option[StaticSizedArrayTT] = {
    staticSizedArrayTypes.get((size, array))
  }
  def getEnvForFunctionSignature(sig: SignatureT): FunctionEnvironment = {
    envByFunctionSignature(sig)
  }
  def getEnvForKind(sr: KindT): CitizenEnvironment[INameT] = {
    envByKind(sr)
  }
  def getInterfaceDefForRef(ir: InterfaceTT): InterfaceDefinitionT = {
    interfaceDefsByRef(ir)
  }
  def getPackType(coords: Vector[CoordT]): Option[StructTT] = {
    packTypes.get(coords)
  }
  def getReturnTypeForSignature(sig: SignatureT): Option[CoordT] = {
    returnTypesBySignature.get(sig)
  }
  def getDeclaredSignatureOrigin(sig: SignatureT): Option[RangeS] = {
    declaredSignatures.get(sig)
  }
  def getDeclaredSignatureOrigin(name: FullNameT[IFunctionNameT]): Option[RangeS] = {
    declaredSignatures.get(ast.SignatureT(name))
  }
  def getStructDefForRef(sr: StructTT): StructDefinitionT = {
    structDefsByRef(sr)
  }
  def getRuntimeSizedArray(array: RawArrayTT): Option[RuntimeSizedArrayTT] = {
    runtimeSizedArrayTypes.get(array)
  }
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
