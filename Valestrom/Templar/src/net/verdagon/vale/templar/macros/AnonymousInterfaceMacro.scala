package net.verdagon.vale.templar.macros

import net.verdagon.vale.{CodeLocationS, IProfiler, PackageCoordinate, RangeS, vassert, vassertOne, vwat}
import net.verdagon.vale.astronomer.{ConstructorNameS, FunctionA, InterfaceA}
import net.verdagon.vale.scout.{AnonymousSubstructMemberNameS, AnonymousSubstructParentInterfaceRuneS, CodeRuneS, CodeNameS, CoordTemplataType, FunctionNameS, FunctionTemplataType, GeneratedBodyS, GlobalFunctionFamilyNameS, IRuneS, ITemplataType, KindTemplataType, ParameterS, RuneNameS, StructNameRuneS, TemplateTemplataType, TopLevelCitizenDeclarationNameS, UserFunctionS}
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules.{CallSR, IRulexSR, LookupSR, RuneUsage}
import net.verdagon.vale.templar.ast.{AbstractT, ArgLookupTE, BlockTE, ConstructTE, DiscardTE, FunctionCallTE, FunctionHeaderT, FunctionT, LocationInFunctionEnvironment, OverrideT, ParameterT, PrototypeT, ReferenceMemberLookupTE, ReturnTE, SoftLoadTE}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env.{FunctionEnvEntry, FunctionEnvironment, IEnvironment, PackageEnvironment, TemplataEnvEntry, TemplataLookupContext}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplarCore}
import net.verdagon.vale.templar.names.{AnonymousSubstructImplNameT, AnonymousSubstructLambdaNameT, AnonymousSubstructMemberNameT, AnonymousSubstructNameT, ConstructorNameT, FullNameT, FunctionNameT, ICitizenNameT, INameT, ImplDeclareNameT, NameTranslator, RuneNameT, TemplarTemporaryVarNameT}
import net.verdagon.vale.templar.templata.{CoordTemplata, ExternFunctionTemplata, ExternImplTemplata, InterfaceTemplata, KindTemplata, MutabilityTemplata}
import net.verdagon.vale.templar.{ArrayTemplar, CompileErrorExceptionT, IFunctionGenerator, LambdaReturnDoesntMatchInterfaceConstructor, OverloadTemplar, RangedInternalErrorT, Templar, TemplarOptions, Temputs, ast}
import net.verdagon.vale.templar.types.{ConstraintT, CoordT, FinalT, ImmutableT, InterfaceTT, MutabilityT, MutableT, NeverT, ParamFilter, ReadonlyT, ReadwriteT, ReferenceMemberTypeT, ShareT, StructDefinitionT, StructMemberT, StructTT}

import scala.collection.immutable.List
import scala.collection.mutable

class AnonymousInterfaceMacro(
  opts: TemplarOptions,
  profiler: IProfiler,
  overloadTemplar: OverloadTemplar,
  structTemplar: StructTemplar,
  structConstructorMacro: StructConstructorMacro) {

  def onInterfaceDefined(
    coord: PackageCoordinate,
    interfaceA: InterfaceA
  ): Vector[(FullNameT[INameT], FunctionEnvEntry)] = {
//    val functionA = getInterfaceConstructor(coord, interfaceA)
//    Vector(
//      FullNameT(coord, Vector(), NameTranslator.translateNameStep(functionA.name)) ->
//        FunctionEnvEntry(functionA))
    Vector()
  }

//  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
//  def prototypeToAnonymousStruct(
//    outerEnv: IEnvironment,
//    temputs: Temputs,
//    life: LocationInFunctionEnvironment,
//    range: RangeS,
//    prototype: PrototypeT,
//    structFullName: FullNameT[ICitizenNameT]):
//  StructTT = {
//    val structTT = StructTT(structFullName)
//
//    temputs.declareStruct(structTT)
//    temputs.declareStructMutability(structTT, ImmutableT)
//
//    val forwarderParams =
//      Vector(
//        ast.ParameterT(
//          TemplarTemporaryVarNameT(life + -1),
//          None,
//          CoordT(
//            ShareT,
//            ReadonlyT,
//            structTT))) ++
//        prototype.paramTypes.zipWithIndex.map({ case (paramType, index) =>
//          ast.ParameterT(TemplarTemporaryVarNameT(life + index), None, paramType)
//        })
//    val forwarderHeader =
//      ast.FunctionHeaderT(
//        structFullName.addStep(FunctionNameT(CallTemplar.CALL_FUNCTION_NAME, Vector.empty, forwarderParams.map(_.tyype))),
//        Vector.empty,
//        forwarderParams,
//        prototype.returnType,
//        None)
//    temputs.declareFunctionSignature(range, forwarderHeader.toSignature, None)
//
//    val structInnerEnv =
//      PackageEnvironment(
//        Some(outerEnv),
//        structFullName,
//        newTemplataStore().addEntries(
//          opts.useOptimization,
//          Map(forwarderHeader.fullName.last -> Vector(TemplataEnvEntry(ExternFunctionTemplata(forwarderHeader))))))
//    temputs.declareStructEnv(structTT, structInnerEnv)
//
//    val structDef =
//      StructDefinitionT(
//        structFullName,
//        Vector.empty,
//        false,
//        ImmutableT,
//        Vector.empty,
//        false)
//    temputs.add(structDef)
//
//    // If it's immutable, make sure there's a zero-arg destructor.
//    //    if (mutability == Immutable) {
//    temputs.addDestructor(
//      structDef.getRef,
//      delegate.getImmConcreteDestructor(temputs, structInnerEnv, structDef.getRef))
//    //    }
//
//    val forwarderFunction =
//      ast.FunctionT(
//        forwarderHeader,
//        BlockTE(
//          Templar.consecutive(
//            Vector(
//              DiscardTE(ArgLookupTE(0, CoordT(ShareT, ReadonlyT, structTT))),
//              ReturnTE(
//                FunctionCallTE(
//                  prototype,
//                  forwarderHeader.params.tail.zipWithIndex.map({ case (param, index) =>
//                    ArgLookupTE(index + 1, param.tyype)
//                  })))))))
//    temputs.addFunction(forwarderFunction)
//
//    structTT
//  }
//
//  // Makes an anonymous substruct of the given interface, which just forwards its method to the given prototype.
//  // This does NOT make a constructor, because its so easy to just Construct2 it.
//  def prototypeToAnonymousStruct(
//    temputs: Temputs,
//    life: LocationInFunctionEnvironment,
//    range: RangeS,
//    prototype: PrototypeT):
//  StructTT = {
//    //    profiler.newProfile("StructTemplar-prototypeToAnonymousStruct", prototype.toString, () => {
//    val structFullName = prototype.fullName.addStep(AnonymousSubstructLambdaNameT(CodeLocationS.internal(-13)))
//
//    temputs.structDeclared(structFullName) match {
//      case Some(structTT) => return structTT
//      case None =>
//    }
//
//    val outerEnv = temputs.getEnvForFunctionSignature(prototype.toSignature)
//    prototypeToAnonymousStruct(
//      outerEnv, temputs, life, range, prototype, structFullName)
//    //    })
//  }
//  // Makes an anonymous substruct of the given interface, with the given lambdas as its members.
//  def makeAnonymousSubstruct(
//    temputs: Temputs,
//    range: RangeS,
//    interfaceTT: InterfaceTT,
//    members: Vector[CoordT]):
//  StructTT = {
//    //    profiler.newProfile("StructTemplar-makeSeqOrPackUnderstruct", "[" + interfaceTT.toString + " " + members.map(_.toString).mkString(", ") + "]", () => {
//    val anonymousSubstructName =
//      interfaceTT.fullName.addStep(AnonymousSubstructNameT(members))
//
//    temputs.structDeclared(anonymousSubstructName) match {
//      case Some(s) => return s
//      case None =>
//    }
//
//    val interfaceEnv = temputs.getEnvForInterfaceRef(interfaceTT)
//
//    val callables = anonymousSubstructName.last.callables
//
//    val interfaceDef = temputs.lookupInterface(interfaceTT)
//
//    // We don't do:
//    //   val mutability = getCompoundTypeMutability(temputs, callables)
//    // because we want the mutability of the receiving interface. For example,
//    // we want to be able to do something like:
//    //   f = IFunction1<mut, Int, Int>({_})
//    // which wouldnt work if we just did the compound mutability of the closureds
//    // (which is imm in this case).
//    val mutability = temputs.lookupMutability(interfaceTT)
//
//    // Dont want any mutables in our immutable interface's substruct
//    if (mutability == ImmutableT) {
//      if (StructTemplar.getCompoundTypeMutability(callables) == MutableT) {
//        throw CompileErrorExceptionT(RangedInternalErrorT(range, "Trying to make a mutable anonymous substruct of an immutable interface!"))
//      }
//    }
//
//    val structTT = StructTT(anonymousSubstructName)
//
//    val forwarderFunctionHeaders =
//      interfaceDef.internalMethods.zipWithIndex.map({
//        case (FunctionHeaderT(superFunctionName, _, superParams, superReturnType, _), index) => {
//          val params =
//            superParams.map({
//              case ParameterT(name, Some(AbstractT), CoordT(ownership, permission, ir)) => {
//                vassert(ir == interfaceTT)
//                ast.ParameterT(name, Some(OverrideT(interfaceTT)), CoordT(ownership, permission, structTT))
//              }
//              case otherParam => otherParam
//            })
//
//          val FunctionNameT(humanName, _, _) = superFunctionName.last
//          val fowarderName =
//            anonymousSubstructName.addStep(FunctionNameT(humanName, Vector.empty, params.map(_.tyype)))
//          val forwarderHeader =
//            ast.FunctionHeaderT(
//              fowarderName,
//              Vector.empty,
//              params,
//              superReturnType,
//              None)
//
//          temputs.declareFunctionSignature(range, forwarderHeader.toSignature, None)
//          forwarderHeader
//        }
//      })
//
//    val structInnerEnvEntries =
//      forwarderFunctionHeaders
//        .map(header => {
//          (header.fullName.last -> TemplataEnvEntry(ExternFunctionTemplata(header)))
//        }) ++
//        Vector(
//          ImplDeclareNameT(NameTranslator.getImplNameForName(opts.useOptimization, interfaceTT).get.subCitizenHumanName, CodeLocationS.internal(-15)) ->
//            Vector(TemplataEnvEntry(ExternImplTemplata(structTT, interfaceTT))),
//          // This is used later by the interface constructor generator to know what interface to impl.
//          RuneNameT(AnonymousSubstructParentInterfaceRuneS()) -> Vector(TemplataEnvEntry(KindTemplata(interfaceTT))),
//          AnonymousSubstructImplNameT() -> Vector(TemplataEnvEntry(ExternImplTemplata(structTT, interfaceTT))))
//    val structInnerEnv =
//      PackageEnvironment(
//        Some(interfaceEnv),
//        anonymousSubstructName,
//        newTemplataStore().addEntries(opts.useOptimization, structInnerEnvEntries))
//
//
//    temputs.addImpl(structTT, interfaceTT)
//
//    temputs.declareStruct(structTT)
//    temputs.declareStructMutability(structTT, mutability)
//    temputs.declareStructEnv(structTT, structInnerEnv)
//
//    vassert(interfaceDef.internalMethods.size == callables.size)
//
//    val structDef =
//      StructDefinitionT(
//        anonymousSubstructName,
//        Vector.empty,
//        interfaceDef.weakable,
//        mutability,
//        callables.zipWithIndex.map({ case (lambda, index) =>
//          StructMemberT(AnonymousSubstructMemberNameT(index), FinalT, ReferenceMemberTypeT(lambda))
//        }),
//        false)
//    temputs.add(structDef)
//
//    // If it's immutable, make sure there's a zero-arg destructor.
//    if (mutability == ImmutableT) {
//      temputs.addDestructor(
//        structDef.getRef,
//        delegate.getImmConcreteDestructor(temputs, structInnerEnv, structDef.getRef))
//    }
//
//    forwarderFunctionHeaders.zip(callables).zipWithIndex.foreach({
//      case ((forwarderHeader, lambda), methodIndex) => {
//        //        val localVariables =
//        //          forwarderHeader.params.map(param => {
//        //            ReferenceLocalVariableT(forwarderHeader.fullName.addStep(param.name), FinalT, param.tyype)
//        //          })
//
//        // The args for the call inside the forwarding function.
//        val lambdaCoord = CoordT(if (lambda.ownership == ShareT) ShareT else ConstraintT, lambda.permission, lambda.kind)
//        val forwardedCallArgs = (Vector(lambdaCoord) ++ forwarderHeader.paramTypes.tail).map(ParamFilter(_, None))
//
//        //        start here
//        // since IFunction has a drop() method, its looking for a drop() for the
//        // lambda we gave it. but its immutable, so it needs no drop... or wait,
//        // maybe imms have drops?
//
//        val lambdaFunctionPrototype =
//          overloadTemplar.scoutExpectedFunctionForPrototype(
//            interfaceEnv, // Shouldnt matter here, because the callables themselves should have a __call
//            temputs,
//            range,
//            GlobalFunctionFamilyNameS(CallTemplar.CALL_FUNCTION_NAME),
//            Vector.empty,
//            Array.empty,
//            forwardedCallArgs,
//            Vector.empty,
//            true)
//
//        val structParamCoord =
//          CoordT(
//            if (structDef.mutability == ImmutableT) ShareT else ConstraintT,
//            forwarderHeader.paramTypes.head.permission,
//            structDef.getRef)
//        val methodCoord = structDef.members(methodIndex).tyype.reference
//        val loadSelfResultPermission = Templar.intersectPermission(methodCoord.permission, structParamCoord.permission)
//        //        val loadSelfResultCoord = methodCoord.copy(permission = loadSelfResultPermission)
//
//        val loadedThisObjOwnership = if (methodCoord.ownership == ShareT) ShareT else ConstraintT
//        val loadedThisObjPermission = if (methodCoord.ownership == ShareT) ReadonlyT else ReadwriteT
//        val argExpressions =
//          Vector(
//            SoftLoadTE(
//              ReferenceMemberLookupTE(
//                range,
//                ArgLookupTE(0, structParamCoord),
//                structDef.fullName.addStep(structDef.members(methodIndex).name),
//                methodCoord,
//                loadSelfResultPermission,
//                FinalT),
//              loadedThisObjOwnership,
//              loadedThisObjPermission)) ++
//            forwarderHeader.params.tail.zipWithIndex.map({ case (param, index) =>
//              ArgLookupTE(index + 1, param.tyype)
//            })
//
//        if (lambdaFunctionPrototype.returnType.kind != NeverT() &&
//          forwarderHeader.returnType != lambdaFunctionPrototype.returnType) {
//          throw CompileErrorExceptionT(LambdaReturnDoesntMatchInterfaceConstructor(range))
//        }
//
//        val forwarderFunction =
//          FunctionT(
//            forwarderHeader,
//            BlockTE(
//              ReturnTE(
//                FunctionCallTE(lambdaFunctionPrototype, argExpressions))))
//        temputs.addFunction(forwarderFunction)
//      }
//    })
//
//    (structTT, mutability)
//  }
//
//  def getInterfaceConstructor(packageCoord: PackageCoordinate, interfaceA: InterfaceA): FunctionA = {
//    profiler.newProfile("StructTemplarGetInterfaceConstructor", interfaceA.name.name, () => {
//      opts.debugOut("todo: put all the members' rules up in the top of the struct")
//      val identifyingRunes = interfaceA.identifyingRunes
//      val functorRunes = interfaceA.internalMethods.indices.map(i => (CodeRuneS("Functor" + i)))
//      val params =
//        interfaceA.internalMethods.zipWithIndex.map({ case (method, index) =>
//          ParameterS(
//            AtomSP(
//              method.range,
//              Some(CaptureS(AnonymousSubstructMemberNameS(index))),
//              None,
//              Some(RuneUsage(method.range, CodeRuneS("Functor" + index))),
//              None))
//        })
//
//      val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
//      runeToType ++= interfaceA.runeToType
//      runeToType ++= functorRunes.map(functorRune => (functorRune -> CoordTemplataType)).toMap
//      runeToType.put(AnonymousSubstructParentInterfaceRuneS(), KindTemplataType)
//
//      val rules = mutable.ArrayBuffer[IRulexSR]()
//      rules ++= interfaceA.rules
//
//      // We stash the interface type in the env with this rune, so that when the interface constructor
//      // generator runs, it can read this to know what interface it's making a subclass of.
//      val substructRune = RuneUsage(interfaceA.name.range, AnonymousSubstructParentInterfaceRuneS())
//      runeToType += (substructRune.rune -> KindTemplataType)
//      if (interfaceA.isTemplate) {
//        val structNameRune = RuneUsage(interfaceA.name.range, StructNameRuneS(interfaceA.name))
//        runeToType += (structNameRune.rune -> interfaceA.tyype)
//        rules += LookupSR(interfaceA.range, structNameRune, CodeTypeNameS(interfaceA.name.name))
//        rules += CallSR(interfaceA.range, substructRune, structNameRune, interfaceA.identifyingRunes.toArray)
//      } else {
//        rules += LookupSR(interfaceA.range, substructRune, CodeTypeNameS(interfaceA.name.name))
//      }
//
//      val isTemplate = interfaceA.tyype != KindTemplataType
//
//      val templateParams =
//        (interfaceA.tyype match {
//          case KindTemplataType => Vector.empty
//          case TemplateTemplataType(params, KindTemplataType) => params
//        }) ++
//          interfaceA.internalMethods.map(meth => CoordTemplataType)
//      val functionType =
//        if (templateParams.isEmpty) FunctionTemplataType else TemplateTemplataType(templateParams, FunctionTemplataType)
//
//      val TopLevelCitizenDeclarationNameS(name, range) = interfaceA.name
//
//      FunctionA(
//        interfaceA.range,
//        FunctionNameS(name, range.begin),
//        Vector(UserFunctionS),
//        functionType,
//        //        interfaceA.knowableRunes ++ functorRunes ++ (if (isTemplate) Vector.empty else Vector(AnonymousSubstructParentInterfaceRuneS())),
//        identifyingRunes,
//        //        interfaceA.localRunes ++ functorRunes ++ Vector(AnonymousSubstructParentInterfaceRuneS()),
//        runeToType.toMap,
//        params,
//        None,
//        rules.toVector,
//        GeneratedBodyS("interfaceConstructorGenerator"))
//    })
//  }
//
//  // This doesnt make a constructor, but its easy enough to make manually.
//  def prototypeToAnonymousSubstruct(
//    temputs: Temputs,
//    life: LocationInFunctionEnvironment,
//    range: RangeS,
//    interfaceTT: InterfaceTT,
//    prototype: PrototypeT):
//  (StructTT, PrototypeT) = {
//    //    profiler.newProfile("StructTemplar-prototypeToAnonymousSubstruct", prototype.toString + " " + interfaceTT.toString, () => {
//    val functionStructRef = prototypeToAnonymousStruct(temputs, life, range, prototype)
//    val functionStructType = CoordT(ShareT, ReadonlyT, functionStructRef)
//
//    val lambdas = Vector(functionStructType)
//
//    val anonymousSubstructTT =
//      makeAnonymousSubstruct(temputs, range, interfaceTT, lambdas)
//    val anonymousSubstructType = CoordT(ShareT, ReadonlyT, anonymousSubstructTT)
//
//    val constructorName =
//      interfaceTT.fullName
//        .addStep(AnonymousSubstructNameT(Vector(functionStructType)))
//        .addStep(ConstructorNameT(Vector.empty))
//    temputs.prototypeDeclared(constructorName) match {
//      case Some(func) => return (anonymousSubstructTT, func)
//      case None =>
//    }
//
//    // Now we make a function which constructs a functionStruct, then constructs a substruct.
//    val constructor2 =
//      FunctionT(
//        FunctionHeaderT(
//          constructorName,
//          Vector.empty,
//          Vector.empty,
//          anonymousSubstructType,
//          None),
//        BlockTE(
//          ReturnTE(
//            ConstructTE(
//              anonymousSubstructTT,
//              anonymousSubstructType,
//              Vector(
//                ConstructTE(
//                  functionStructRef,
//                  CoordT(ShareT, ReadonlyT, functionStructRef),
//                  Vector.empty))))))
//    temputs.declareFunctionSignature(range, constructor2.header.toSignature, None)
//    temputs.declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
//    temputs.addFunction(constructor2);
//
//    vassert(temputs.getDeclaredSignatureOrigin(constructor2.header.fullName) == Some(range))
//
//    (anonymousSubstructTT, constructor2.header.toPrototype)
//    //    })
//  }
//
//  def prototypeToAnonymousIFunctionSubstruct(
//    env: IEnvironment,
//    temputs: Temputs,
//    life: LocationInFunctionEnvironment,
//    range: RangeS,
//    prototype: PrototypeT):
//  (InterfaceTT, StructTT, PrototypeT) = {
//    //    profiler.newProfile("StructTemplar-prototypeToAnonymousIFunctionSubstruct", prototype.toString, () => {
//    val returnType = prototype.returnType
//    val Vector(paramType) = prototype.fullName.last.parameters
//
//    val ifunction1Templata@InterfaceTemplata(_, _) =
//      vassertOne(env.lookupWithImpreciseName(profiler, CodeTypeNameS("IFunction1"), Set(TemplataLookupContext), true))
//    val ifunction1InterfaceRef =
//      structTemplar.getInterfaceRef(
//        temputs,
//        range,
//        ifunction1Templata,
//        Vector(
//          MutabilityTemplata(ImmutableT),
//          CoordTemplata(paramType),
//          CoordTemplata(returnType)))
//
//    val (elementDropFunctionAsIFunctionSubstructStructRef, constructorPrototype) =
//      prototypeToAnonymousSubstruct(
//        temputs, life, range, ifunction1InterfaceRef, prototype)
//
//    (ifunction1InterfaceRef, elementDropFunctionAsIFunctionSubstructStructRef, constructorPrototype)
//    //    })
//  }
//
//
////  def getFunctionGenerators(): Map[String, IFunctionGenerator] = {
////    Map(
////      "interfaceConstructorGenerator" ->
////        new IFunctionGenerator {
////          override def generate(
////            profiler: IProfiler,
////            functionTemplarCore: FunctionTemplarCore,
////            structTemplar: StructTemplar,
////            destructorTemplar: DestructorTemplar,
////            arrayTemplar: ArrayTemplar,
////            env: FunctionEnvironment,
////            temputs: Temputs,
////            life: LocationInFunctionEnvironment,
////            callRange: RangeS,
////            originFunction: Option[FunctionA],
////            paramCoords: Vector[ParameterT],
////            maybeRetCoord: Option[CoordT]):
////          (FunctionHeaderT) = {
////            // The interface should be in the "__Interface" rune of the function environment.
////            val interfaceTT =
////              env.lookupWithImpreciseName(profiler, RuneNameS(AnonymousSubstructParentInterfaceRuneS()), Set(TemplataLookupContext), true) match {
////                case List(KindTemplata(ir @ InterfaceTT(_))) => ir
////                case other => vwat(other)
////              }
////
////            val structTT =
////              makeAnonymousSubstruct(
////                temputs, callRange, interfaceTT, paramCoords.map(_.tyype))
////            val structDef = temputs.lookupStruct(structTT)
////
////            val constructorFullName = env.fullName
////            val constructor =
////              structConstructorMacro.makeStructConstructor(
////                temputs, originFunction, structDef, constructorFullName)
////
////            constructor
////          }
////        })
////  }
}
