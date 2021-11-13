package net.verdagon.vale.templar.citizen

import net.verdagon.vale.astronomer._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.parser.{FinalP, ImmutableP, MutabilityP, MutableP}
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar.OverloadTemplar.FindFunctionFailure
import net.verdagon.vale.templar.{ast, _}
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.function.{DestructorTemplar, FunctionTemplar, FunctionTemplarCore, FunctionTemplarMiddleLayer, FunctionTemplarOrdinaryOrTemplatedLayer}
import net.verdagon.vale._
import net.verdagon.vale.scout.rules.RuneUsage
import net.verdagon.vale.templar.ast.ProgramT.tupleHumanName
import net.verdagon.vale.templar.ast.{AbstractT, ArgLookupTE, BlockTE, DiscardTE, FunctionCallTE, FunctionHeaderT, FunctionT, ICitizenAttribute2, LocationInFunctionEnvironment, OverrideT, ParameterT, ProgramT, PrototypeT, ReferenceMemberLookupTE, ReturnTE, SoftLoadTE}
import net.verdagon.vale.templar.expression.CallTemplar
import net.verdagon.vale.templar.names.{AnonymousSubstructImplNameT, AnonymousSubstructMemberNameT, AnonymousSubstructNameT, CitizenNameT, ClosureParamNameT, CodeVarNameT, FullNameT, FunctionNameT, FunctionTemplateNameT, ICitizenNameT, INameT, ImplDeclareNameT, LambdaCitizenNameT, NameTranslator, RuneNameT, SelfNameT, TemplarTemporaryVarNameT}

import scala.collection.immutable.List

class StructTemplarCore(
    opts: TemplarOptions,
    profiler: IProfiler,
    ancestorHelper: AncestorHelper,
    delegate: IStructTemplarDelegate) {
  def addBuiltInStructs(env: PackageEnvironment[INameT], temputs: Temputs): Unit = {
//    val emptyTupleFullName = ProgramT.emptyTupleStructRef.fullName
//    val structDefT = StructDefinitionT(emptyTupleFullName, Vector(), false, ImmutableT, Vector.empty, false)
//    temputs.declareKind(structDefT.getRef)
//    temputs.declareCitizenMutability(structDefT.getRef, ImmutableT)
//
////    val packStructA =
////      StructA(
////        RangeS.internal(-1337),
////        TopLevelCitizenDeclarationNameS(ProgramT.tupleHumanName, RangeS.internal(-1337)),
////        Vector(),
////        false,
////        RuneUsage(RangeS.internal(-1338), CodeRuneS("M")),
////        None,
////        TemplateTemplataType(Vector(PackTemplataType(CoordTemplataType)), KindTemplataType),
////        Vector(RuneUsage(RangeS.internal(-1339), CodeRuneS("T"))),
////        Map(
////          CodeRuneS("T") -> PackTemplataType(CoordTemplataType),
////          CodeRuneS("M") -> MutabilityTemplataType),
////        Vector(),
////        Vector(
////          StructMemberS(
////            RangeS.internal(-1339),
////            "members",
////            FinalP,
////            RuneUsage(RangeS.internal(-1340), CodeRuneS("T")))))
//
////    val functionA =
////      env.globalEnv.structDropMacro.makeDropFunction(
////        CodeNameS("Tup"),
////        RangeS.internal(-1337),
////        TemplateTemplataType(Vector(PackTemplataType(CoordTemplataType)), KindTemplataType),
////        Vector(CodeRuneS("M")),
////        Map(CodeRuneS("M") -> PackTemplataType(CoordTemplataType)))
//    val tupleEnv =
//      CitizenEnvironment(
//        env.globalEnv, env, emptyTupleFullName,
//        TemplatasStore(emptyTupleFullName, Map(), Map())
//          .addEntries(
//            env.globalEnv.structDropMacro.getStructChildEntries(emptyTupleFullName, packStructA)
////            Vector[(INameT, IEnvEntry)]((NameTranslator.translateFunctionNameToTemplateName(functionA.name), FunctionEnvEntry(functionA)))
//            .toMap.mapValues(Vector(_))))
//    temputs
//      .declareKindEnv(
//        structDefT.getRef,
//        tupleEnv)
//
//    temputs.add(structDefT)
//    // Normally after adding a struct we would add its destructor. Void is the only one we don't
//    // have a destructor for.
//
//    env.globalEnv.onStructGeneratedMacros.foreach(maacro => {
//      maacro.onStructGenerated(structDefT.getRef)
//    })
//
//    temputs.declarePack(Vector.empty, structDefT.getRef)
  }

  def makeStruct(
    // The environment that the struct was defined in.
    structRunesEnv: CitizenEnvironment[INameT],
    temputs: Temputs,
    structA: StructA,
    coercedFinalTemplateArgs: Vector[ITemplata]):
  (StructDefinitionT) = {
    val TopLevelCitizenDeclarationNameS(humanName, codeLocation) = structA.name
    val fullName = structRunesEnv.fullName.addStep(CitizenNameT(humanName, coercedFinalTemplateArgs))
    val temporaryStructRef = StructTT(fullName)

    val attributesWithoutExport =
      structA.attributes.filter({
        case ExportS(_) => false
        case _ => true
      })
    val maybeExport =
      structA.attributes.collectFirst { case e@ExportS(_) => e }
//
//    val type
//    delegate.scoutExpectedFunctionForPrototype(
//      structRunesEnv,
//      temputs,
//      RangeS.internal(-1663),
//      //          if (type2.ownership == ShareT) {
//      //            ImmConcreteDestructorImpreciseNameS()
//      //          } else {
//      CodeVarNameS(CallTemplar.DROP_FUNCTION_NAME),
//      //          },
//      Vector.empty,
//      Array.empty,
//      Vector(ParamFilter(type2, None)),
//      Vector(temputs.getEnvForKind(type2.kind)),
//      true)
//
//    delegate.scoutExpectedFunctionForPrototype(
//      structRunesEnv, temputs,
//    )

    val structInnerEnv =
      CitizenEnvironment(
        structRunesEnv.globalEnv, structRunesEnv, fullName,
        TemplatasStore(fullName, Map(), Map())
          .addEntries(
            structRunesEnv.globalEnv.structDropMacro.getStructChildEntries(
              fullName, structA).toMap.mapValues(Vector(_))))

    // when we have structs that contain functions, add this back in
//        structA.members
//          .map(_.origin)
//          .map(FunctionEnvEntry)
//          .groupBy(_.function.name))


      temputs
        .declareKindEnv(
          temporaryStructRef,
          structInnerEnv)

    val members = makeStructMembers(structInnerEnv, temputs, structA.members)

    val mutability =
      structInnerEnv.lookupWithImpreciseName(
        profiler,
        RuneNameS(structA.mutabilityRune.rune),
        Set(TemplataLookupContext),
        true).toList match {
        case List(MutabilityTemplata(m)) => m
        case _ => vwat()
      }

    if (mutability == ImmutableT) {
      members.zipWithIndex.foreach({ case (member, index) =>
      if (member.variability == VaryingT) {
          throw CompileErrorExceptionT(
            ImmStructCantHaveVaryingMember(
              structA.members(index).range,
              structA.name,
              structA.members(index).name))
        }
      })
    }

    val structDefT =
      StructDefinitionT(
        fullName,
        translateCitizenAttributes(attributesWithoutExport),
        structA.weakable,
        mutability,
        members,
        false)

    temputs.add(structDefT);

    maybeExport match {
      case None =>
      case Some(exportPackageCoord) => {
        val exportedName =
          fullName.last match {
            case CitizenNameT(humanName, _) => humanName
            case _ => vfail("Can't export something that doesn't have a human readable name!")
          }
        temputs.addKindExport(
          structA.range,
          structDefT.getRef,
          exportPackageCoord.packageCoordinate,
          exportedName)
      }
    }

    structRunesEnv.globalEnv.onStructGeneratedMacros.foreach(maacro => {
      maacro.onStructGenerated(structDefT.getRef)
    })

    profiler.childFrame("struct ancestor interfaces", () => {
      val ancestorInterfaces =
        ancestorHelper.getAncestorInterfaces(temputs, temporaryStructRef)

      ancestorInterfaces.foreach({
        case (ancestorInterface) => {
          val interfaceDefinition2 = temputs.lookupInterface(ancestorInterface)
          if (structDefT.weakable != interfaceDefinition2.weakable) {
            throw WeakableImplingMismatch(structDefT.weakable, interfaceDefinition2.weakable)
          }
          temputs.addImpl(temporaryStructRef, ancestorInterface)

          structRunesEnv.globalEnv.onImplGeneratedMacros.foreach(maacro => {
            maacro.onImplGenerated(structDefT.getRef, ancestorInterface)
          })
        }
      })
    })

    structDefT
  }

  def translateCitizenAttributes(attrs: Vector[ICitizenAttributeS]): Vector[ICitizenAttribute2] = {
    attrs.map({
      case x => vimpl(x.toString)
    })
  }

  // Takes a IEnvironment because we might be inside a:
  // struct<T> Thing<T> {
  //   t: T;
  // }
  // which means we need some way to know what T is.
  def makeInterface(
    interfaceRunesEnv: CitizenEnvironment[INameT],
    temputs: Temputs,
    interfaceA: InterfaceA,
    coercedFinalTemplateArgs2: Vector[ITemplata]):
  (InterfaceDefinitionT) = {
    val TopLevelCitizenDeclarationNameS(humanName, codeLocation) = interfaceA.name
    val fullName = interfaceRunesEnv.fullName.addStep(CitizenNameT(humanName, coercedFinalTemplateArgs2))
    val temporaryInferfaceRef = InterfaceTT(fullName)

    val attributesWithoutExport =
      interfaceA.attributes.filter({
        case ExportS(_) => false
        case _ => true
      })
    val maybeExport =
      interfaceA.attributes.collectFirst { case e@ExportS(_) => e }

    val interfaceInnerEnv =
      CitizenEnvironment(
        interfaceRunesEnv.globalEnv,
        interfaceRunesEnv,
        fullName,
        TemplatasStore(fullName, Map(), Map())
          .addEntries(
            interfaceRunesEnv.globalEnv.interfaceDropMacro.getInterfaceChildEntries(
              interfaceRunesEnv.fullName, interfaceA)
            .toMap.mapValues(Vector(_)))
          .addEntries(
            interfaceA.identifyingRunes.zip(coercedFinalTemplateArgs2)
              .map({ case (rune, templata) => (RuneNameT(rune.rune), Vector(TemplataEnvEntry(templata))) })
              .toMap)
          .addEntries(
            Map(SelfNameT() -> Vector(TemplataEnvEntry(KindTemplata(temporaryInferfaceRef)))))
          .addEntries(
            interfaceA.internalMethods
              .map(internalMethod => {
                val functionName = NameTranslator.translateFunctionNameToTemplateName(internalMethod.name)
                (functionName -> Vector(FunctionEnvEntry(internalMethod)))
              })
              .toMap))

    temputs
      .declareKindEnv(
        temporaryInferfaceRef,
        interfaceInnerEnv)

    val internalMethods2 =
      interfaceA.internalMethods.map(internalMethod => {
        if (internalMethod.isTemplate) {
          delegate.evaluateTemplatedFunctionFromNonCallForHeader(
            temputs,
            FunctionTemplata(
              interfaceInnerEnv,
              internalMethod))
        } else {
          delegate.evaluateOrdinaryFunctionFromNonCallForHeader(
            temputs,
            FunctionTemplata(
              interfaceInnerEnv,
              internalMethod))
        }
      })

    val mutability =
      interfaceInnerEnv.lookupWithImpreciseName(
        profiler,
        RuneNameS(interfaceA.mutabilityRune.rune),
        Set(TemplataLookupContext),
        true).toList match {
        case List(MutabilityTemplata(m)) => m
        case _ => vwat()
      }

    val interfaceDef2 =
      InterfaceDefinitionT(
        fullName,
        translateCitizenAttributes(attributesWithoutExport),
        interfaceA.weakable,
        mutability,
        internalMethods2)
    temputs.add(interfaceDef2)

    maybeExport match {
      case None =>
      case Some(exportPackageCoord) => {
        val exportedName =
          fullName.last match {
            case CitizenNameT(humanName, _) => humanName
            case _ => vfail("Can't export something that doesn't have a human readable name!")
          }
        temputs.addKindExport(
          interfaceA.range,
          interfaceDef2.getRef,
          exportPackageCoord.packageCoordinate,
          exportedName)
      }
    }

    interfaceRunesEnv.globalEnv.onInterfaceGeneratedMacros.foreach(maacro => {
      maacro.onInterfaceGenerated(interfaceDef2.getRef)
    })

    profiler.childFrame("interface ancestor interfaces", () => {
      val _ = ancestorHelper.getParentInterfaces(temputs, temporaryInferfaceRef)

      //
      //      interfaceA.internalMethods.foldLeft(temputs)({
      //        case (ntvFunction1) => {
      //          if (ntvFunction1.isTemplate) {
      //            // Do nothing, can't evaluate it now
      //            temputs
      //          } else {
      //            FunctionTemplar.evaluateOrdinaryLightFunctionFromNonCallForTemputs(
      //              temputs,
      //              FunctionTemplata(interfaceInnerEnv, ntvFunction1))
      //          }
      //        }
      //      })
    })

    (interfaceDef2)
  }

  private def makeStructMembers(env: IEnvironment, temputs: Temputs, members: Vector[StructMemberS]): (Vector[StructMemberT]) = {
    members.map(makeStructMember(env, temputs, _))
  }

  private def makeStructMember(
    env: IEnvironment,
    temputs: Temputs,
    member: StructMemberS):
  (StructMemberT) = {
    start
    // lets maybe add a ...T
    // to structs so we can explode it into members

    val CoordTemplata(coord) =
      vassertOne(
        env.lookupWithImpreciseName(
          profiler, RuneNameS(member.typeRune.rune), Set(TemplataLookupContext), true))
    (StructMemberT(CodeVarNameT(member.name), Conversions.evaluateVariability(member.variability), ReferenceMemberTypeT(coord)))
  }

//  // Makes a functor for the given prototype.
//  def functionToLambda(
//    outerEnv: IEnvironment,
//    temputs: Temputs,
//    header: FunctionHeader2):
//  structTT = {
//    val mutability = Immutable
//
//    val nearName = FunctionScout.CLOSURE_STRUCT_NAME // For example "__Closure<main>:lam1"
//    val fullName = FullName2(header.fullName.steps :+ NamePart2(nearName, Some(Vector.empty), None, None))
//
//    val structTT = structTT(fullName)
//
//    // We declare the function into the environment that we use to compile the
//    // struct, so that those who use the struct can reach into its environment
//    // and see the function and use it.
//    // See CSFMSEO and SAFHE.
//    val structEnv =
//      PackageEnvironment(
//        Some(outerEnv),
//        fullName,
//        Map(
//          CallTemplar.CALL_FUNCTION_NAME -> Vector(TemplataEnvEntry(ExternFunctionTemplata(header))),
//          nearName -> Vector(TemplataEnvEntry(KindTemplata(structTT))),
//          FunctionScout.CLOSURE_STRUCT_ENV_ENTRY_NAME -> Vector(TemplataEnvEntry(KindTemplata(structTT)))))
//
//    temputs.declareStruct(structTT);
//    temputs.declareCitizenMutability(structTT, mutability)
//    temputs.declareKindEnv(structTT, structEnv);
//
//    val closureStructDefinition = StructDefinition2(fullName, mutability, Vector.empty, true);
//    temputs.add(closureStructDefinition)
//
//    val closuredVarsStructRef = closureStructDefinition.getRef;
//
//    closuredVarsStructRef
//  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
    containingFunctionEnv: IEnvironment,
    temputs: Temputs,
    name: IFunctionDeclarationNameS,
    functionA: FunctionA,
    members: Vector[StructMemberT]):
  (StructTT, MutabilityT, FunctionTemplata) = {
    val isMutable =
      members.exists({ case StructMemberT(name, variability, tyype) =>
        if (variability == VaryingT) {
          true
        } else {
          tyype match {
            case AddressMemberTypeT(reference) => true
            case ReferenceMemberTypeT(reference) => {
              reference.ownership match {
                case OwnT | ConstraintT | WeakT => true
                case ShareT => false
              }
            }
          }
        }
      })
    val mutability = if (isMutable) MutableT else ImmutableT

    val nearName = LambdaCitizenNameT(NameTranslator.translateCodeLocation(functionA.range.begin))
    val fullName = containingFunctionEnv.fullName.addStep(nearName)

    val structTT = StructTT(fullName)

    // We declare the function into the environment that we use to compile the
    // struct, so that those who use the struct can reach into its environment
    // and see the function and use it.
    // See CSFMSEO and SAFHE.
    val structEnv =
      CitizenEnvironment(
        containingFunctionEnv.globalEnv,
        containingFunctionEnv,
        fullName,
        TemplatasStore(fullName, Map(), Map())
          .addEntries(
            Map(
              FunctionTemplateNameT(CallTemplar.CALL_FUNCTION_NAME, functionA.range.begin) ->
                Vector(FunctionEnvEntry(functionA)),
              FunctionTemplateNameT(CallTemplar.DROP_FUNCTION_NAME, functionA.range.begin) ->
                Vector(FunctionEnvEntry(containingFunctionEnv.globalEnv.structDropMacro.makeImplicitDropFunction(SelfNameS(), functionA.range))),
              nearName -> Vector(TemplataEnvEntry(KindTemplata(structTT))),
              SelfNameT() -> Vector(TemplataEnvEntry(KindTemplata(structTT))))))
    // We return this from the function in case we want to eagerly compile it (which we do
    // if it's not a template).
    val functionTemplata =
        FunctionTemplata(
          structEnv,
          functionA)

    temputs.declareKind(structTT);
    temputs.declareCitizenMutability(structTT, mutability)
    temputs.declareKindEnv(structTT, structEnv);

    val closureStructDefinition = StructDefinitionT(fullName, Vector.empty, false, mutability, members, true);
    temputs.add(closureStructDefinition)

    structEnv.globalEnv.onStructGeneratedMacros.foreach(maacro => {
      maacro.onStructGenerated(structTT)
    })

    val closuredVarsStructRef = closureStructDefinition.getRef;

    (closuredVarsStructRef, mutability, functionTemplata)
  }

//  // Makes a struct to back a pack or tuple
//  def makeSeqOrPackUnderstruct(
//    outerEnv: PackageEnvironment[INameT],
//    temputs: Temputs,
//    memberCoords: Vector[CoordT],
//    name: ICitizenNameT):
//  (StructTT, MutabilityT) = {
//    temputs.getPackType(memberCoords) match {
//      case Some(structTT) => return (structTT, temputs.lookupStruct(structTT).mutability)
//      case None =>
//    }
//    val packMutability = StructTemplar.getCompoundTypeMutability(memberCoords)
//    val members =
//      memberCoords.zipWithIndex.map({
//        case (pointerType, index) => StructMemberT(CodeVarNameT(index.toString), FinalT, ReferenceMemberTypeT(pointerType))
//      })
//
//    val fullName = outerEnv.fullName.addStep(CitizenNameT(tupleHumanName, Vector(CoordListTemplata(memberCoords))))
//    val structInnerEnv =
//      CitizenEnvironment(outerEnv.globalEnv, outerEnv, fullName, TemplatasStore(outerEnv.fullName, Map(), Map()))
//
//    val newStructDef = StructDefinitionT(structInnerEnv.fullName, Vector.empty, false, packMutability, members, false);
//    if (memberCoords.isEmpty && packMutability != ImmutableT)
//      vfail("curiosity")
//
//    temputs.declareKind(newStructDef.getRef);
//    temputs.declareCitizenMutability(newStructDef.getRef, packMutability)
//    temputs.declareKindEnv(newStructDef.getRef, structInnerEnv);
//    temputs.add(newStructDef)
//
//    outerEnv.globalEnv.onStructGeneratedMacros.foreach(maacro => {
//      maacro.onStructGenerated(newStructDef.getRef)
//    })
//
//    temputs.declarePack(memberCoords, newStructDef.getRef);
//
//    (newStructDef.getRef, packMutability)
//  }

}
