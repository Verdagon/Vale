package dev.vale.typing.macros

import dev.vale.highertyping.{FunctionA, ImplA, InterfaceA, StructA}
import dev.vale.{Accumulator, CodeLocationS, Interner, Keywords, PackageCoordinate, Profiler, RangeS, StrI, vassert, vassertOne, vassertSome, vfail, vimpl, vwat}
import dev.vale.parsing.ast.{BorrowP, FinalP, OwnP, UseP}
import dev.vale.postparsing.patterns.{AbstractSP, AtomSP, CaptureS}
import dev.vale.postparsing.{SealedS, _}
import dev.vale.postparsing.rules.{AugmentSR, CallSR, CallSiteFuncSR, CoerceToCoordSR, CoordComponentsSR, CoordIsaSR, DefinitionFuncSR, EqualsSR, Equivalencies, IRulexSR, IsConcreteSR, IsInterfaceSR, IsStructSR, KindComponentsSR, KindIsaSR, LiteralSR, LookupSR, OneOfSR, PackSR, PrototypeComponentsSR, RefListCompoundMutabilitySR, ResolveSR, RuleScout, RuneParentEnvLookupSR, RuneUsage, RuntimeSizedArraySR, StaticSizedArraySR}
import dev.vale.typing.{OverloadResolver, TypingPassOptions}
import dev.vale.typing.citizen.StructCompiler
import dev.vale.typing.env.{FunctionEnvEntry, IEnvEntry, ImplEnvEntry, StructEnvEntry}
import dev.vale.typing.expression.CallCompiler
import dev.vale.typing.macros.citizen.{ImplDropMacro, InterfaceFreeMacro, StructDropMacro, StructFreeMacro}
import dev.vale.typing.names.{FullNameT, INameT, NameTranslator}
import dev.vale.typing.types.MutabilityT
import dev.vale.highertyping.FunctionA
import dev.vale.postparsing.patterns._
import dev.vale.typing.ast._
import dev.vale.typing.env.PackageEnvironment
import dev.vale.typing.function.FunctionCompilerCore
import dev.vale.typing.macros.citizen.StructDropMacro
import dev.vale.typing.names.AnonymousSubstructImplNameT
import dev.vale.typing.templata.ExternFunctionTemplata
import dev.vale.typing.ast
import dev.vale.typing.types.CoordT

import scala.collection.immutable.List
import scala.collection.mutable

class AnonymousInterfaceMacro(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords,
    nameTranslator: NameTranslator,
    overloadCompiler: OverloadResolver,
    structCompiler: StructCompiler,
    structConstructorMacro: StructConstructorMacro,
    structDropMacro: StructDropMacro,
    structFreeMacro: StructFreeMacro,
    interfaceFreeMacro: InterfaceFreeMacro,
    implDropMacro: ImplDropMacro
) extends IOnInterfaceDefinedMacro {

  val macroName: StrI = keywords.DeriveAnonymousSubstruct

//  val generatorId: String = "interfaceConstructorGenerator"

  override def getInterfaceChildEntries(interfaceName: FullNameT[INameT], interfaceA: InterfaceA, mutability: MutabilityT): Vector[(FullNameT[INameT], IEnvEntry)] = {
    vimpl()
  }

  override def getInterfaceSiblingEntries(interfaceName: FullNameT[INameT], interfaceA: InterfaceA): Vector[(FullNameT[INameT], IEnvEntry)] = {
    if (interfaceA.attributes.contains(SealedS)) {
      return Vector()
    }

    val memberRunes =
      interfaceA.internalMethods.zipWithIndex.map({ case (method, index) =>
        RuneUsage(RangeS(method.range.begin, method.range.begin), AnonymousSubstructMemberRuneS(interfaceA.name, method.name))
      })
    val members =
      interfaceA.internalMethods.zip(memberRunes).zipWithIndex.map({ case ((method, rune), index) =>
        NormalStructMemberS(method.range, interner.intern(StrI(index.toString)), FinalP, rune)
      })

    val structNameS = interner.intern(AnonymousSubstructTemplateNameS(interfaceA.name))
    val structNameT = interfaceName.copy(last = nameTranslator.translateNameStep(structNameS))
    val structA = makeStruct(interfaceA, memberRunes, members, structNameS)

    val moreEntries =
        interfaceFreeMacro.getInterfaceSiblingEntries(structNameT, interfaceA) ++
        structConstructorMacro.getStructSiblingEntries(structNameT, structA) ++
        structDropMacro.getStructSiblingEntries(structNameT, structA) ++
        structFreeMacro.getStructSiblingEntries(structNameT, structA)

    val forwarderMethods =
      interfaceA.internalMethods.zip(memberRunes).zipWithIndex.map({ case ((method, rune), methodIndex) =>
        val name = structNameT.copy(last = nameTranslator.translateFunctionNameToTemplateName(method.name))
        (name, FunctionEnvEntry(makeForwarderFunction(structNameS, interfaceA, structA, method, methodIndex)))
      })

    val rules =
      (structA.headerRules ++ structA.memberRules) :+
        LookupSR(
          interfaceA.range,
          RuneUsage(interfaceA.range, AnonymousSubstructTemplateRuneS()),
          structA.name.getImpreciseName(interner)) :+
        CallSR(
          structA.range,
          RuneUsage(structA.range, AnonymousSubstructRuneS()),
          RuneUsage(structA.range, AnonymousSubstructTemplateRuneS()),
          structA.genericParameters.map(_.rune).toArray) :+
        LookupSR(
          interfaceA.range,
          RuneUsage(interfaceA.range, AnonymousSubstructParentInterfaceTemplateRuneS()),
          interfaceA.name.getImpreciseName(interner)) :+
        CallSR(
          interfaceA.range,
          RuneUsage(interfaceA.range, AnonymousSubstructParentInterfaceRuneS()),
          RuneUsage(interfaceA.range, AnonymousSubstructParentInterfaceTemplateRuneS()),
          interfaceA.genericParameters.map(_.rune).toArray)
    val runeToType =
      structA.headerRuneToType ++
      structA.membersRuneToType +
        (AnonymousSubstructRuneS() -> KindTemplataType()) +
        (AnonymousSubstructTemplateRuneS() -> structA.tyype) +
        (AnonymousSubstructParentInterfaceRuneS() -> KindTemplataType()) +
        (AnonymousSubstructParentInterfaceTemplateRuneS() -> interfaceA.tyype)
    val structKindRuneS = RuneUsage(interfaceA.range, AnonymousSubstructRuneS())
    val interfaceKindRuneS = RuneUsage(interfaceA.range, AnonymousSubstructParentInterfaceRuneS())

    val implNameS = interner.intern(AnonymousSubstructImplDeclarationNameS(interfaceA.name))
//    val implImpreciseNameS = interner.intern(ImplImpreciseNameS(RuleScout.getRuneKindTemplate(rules, structKindRuneS.rune)))

    val implA =
      ImplA(
        interfaceA.range,
        implNameS,
//        // Just getting the template name (or the kind name if not template), see INSHN.
//        implImpreciseNameS,
        structA.genericParameters,
        rules.toVector,
        runeToType,
        structKindRuneS,
        structA.name.getImpreciseName(interner),
        interfaceKindRuneS,
        interfaceA.name.getImpreciseName(interner))
    val implNameT = structNameT.copy(last = nameTranslator.translateNameStep(implA.name))
    val implSiblingEntries =
      implDropMacro.getImplSiblingEntries(implNameT, implA)

    Vector[(FullNameT[INameT], IEnvEntry)](
      (structNameT, StructEnvEntry(structA)),
      (implNameT, ImplEnvEntry(implA))) ++
      moreEntries ++
      forwarderMethods ++
      implSiblingEntries
  }

  private def mapRunes(rule: IRulexSR, func: IRuneS => IRuneS): IRulexSR = {
    rule match {
      case LookupSR(range, RuneUsage(a, rune), name) => LookupSR(range, RuneUsage(a, func(rune)), name)
      case RuneParentEnvLookupSR(range, RuneUsage(a, rune)) => RuneParentEnvLookupSR(range, RuneUsage(a, func(rune)))
      case EqualsSR(range, RuneUsage(a, left), RuneUsage(b, right)) => EqualsSR(range, RuneUsage(a, func(left)), RuneUsage(b, func(right)))
      case CoordIsaSR(range, RuneUsage(a, sub), RuneUsage(b, suuper)) => CoordIsaSR(range, RuneUsage(a, func(sub)), RuneUsage(b, func(suuper)))
      case KindIsaSR(range, RuneUsage(a, sub), RuneUsage(b, suuper)) => KindIsaSR(range, RuneUsage(a, func(sub)), RuneUsage(b, func(suuper)))
      case KindComponentsSR(range, RuneUsage(a, resultRune), RuneUsage(b, mutabilityRune)) => KindComponentsSR(range, RuneUsage(a, func(resultRune)), RuneUsage(b, func(mutabilityRune)))
      case CoordComponentsSR(range, RuneUsage(a, resultRune), RuneUsage(b, ownershipRune), RuneUsage(c, kindRune)) => CoordComponentsSR(range, RuneUsage(a, func(resultRune)), RuneUsage(b, func(ownershipRune)), RuneUsage(c, func(kindRune)))
      case PrototypeComponentsSR(range, RuneUsage(a, resultRune), RuneUsage(b, paramsRune), RuneUsage(c, returnRune)) => PrototypeComponentsSR(range, RuneUsage(a, func(resultRune)), RuneUsage(b, func(paramsRune)), RuneUsage(c, func(returnRune)))
      case ResolveSR(range, RuneUsage(a, resultRune), name, RuneUsage(b, paramsListRune), RuneUsage(c, returnRune)) => ResolveSR(range, RuneUsage(a, func(resultRune)), name, RuneUsage(b, func(paramsListRune)), RuneUsage(c, func(returnRune)))
      case CallSiteFuncSR(range, RuneUsage(a, resultRune), name, RuneUsage(b, paramsListRune), RuneUsage(c, returnRune)) => CallSiteFuncSR(range, RuneUsage(a, func(resultRune)), name, RuneUsage(b, func(paramsListRune)), RuneUsage(c, func(returnRune)))
      case DefinitionFuncSR(range, RuneUsage(a, resultRune), name, RuneUsage(b, paramsListRune), RuneUsage(c, returnRune)) => DefinitionFuncSR(range, RuneUsage(a, func(resultRune)), name, RuneUsage(b, func(paramsListRune)), RuneUsage(c, func(returnRune)))
      case OneOfSR(range, RuneUsage(a, rune), literals) => OneOfSR(range, RuneUsage(a, func(rune)), literals)
      case IsConcreteSR(range, RuneUsage(a, rune)) => IsConcreteSR(range, RuneUsage(a, func(rune)))
      case IsInterfaceSR(range, RuneUsage(a, rune)) => IsInterfaceSR(range, RuneUsage(a, func(rune)))
      case IsStructSR(range, RuneUsage(a, rune)) => IsStructSR(range, RuneUsage(a, func(rune)))
      case CoerceToCoordSR(range, RuneUsage(a, coordRune), RuneUsage(b, kindRune)) => CoerceToCoordSR(range, RuneUsage(a, func(coordRune)), RuneUsage(b, func(kindRune)))
      case LiteralSR(range, RuneUsage(a, rune), literal) => LiteralSR(range, RuneUsage(a, func(rune)), literal)
      case AugmentSR(range, RuneUsage(a, resultRune), ownership, RuneUsage(b, innerRune)) => AugmentSR(range, RuneUsage(a, func(resultRune)), ownership, RuneUsage(b, func(innerRune)))
      case CallSR(range, RuneUsage(a, resultRune), RuneUsage(b, templateRune), args) => CallSR(range, RuneUsage(a, func(resultRune)), RuneUsage(b, func(templateRune)), args.map({ case RuneUsage(c, rune) => RuneUsage(c, func(rune)) }))
      case PackSR(range, RuneUsage(a, resultRune), members) => PackSR(range, RuneUsage(a, resultRune), members.map({ case RuneUsage(c, rune) => RuneUsage(c, func(rune)) }))
      case StaticSizedArraySR(range, RuneUsage(a, resultRune), RuneUsage(b, mutabilityRune), RuneUsage(c, variabilityRune), RuneUsage(d, sizeRune), RuneUsage(e, elementRune)) => StaticSizedArraySR(range, RuneUsage(a, func(resultRune)), RuneUsage(b, func(mutabilityRune)), RuneUsage(c, func(variabilityRune)), RuneUsage(d, func(sizeRune)), RuneUsage(e, func(elementRune)))
      case RuntimeSizedArraySR(range, RuneUsage(a, resultRune), RuneUsage(b, mutabilityRune), RuneUsage(c, elementRune)) => RuntimeSizedArraySR(range, RuneUsage(a, func(resultRune)), RuneUsage(b, func(mutabilityRune)), RuneUsage(c, func(elementRune)))
      case RefListCompoundMutabilitySR(range, RuneUsage(a, resultRune), RuneUsage(b, coordListRune)) => RefListCompoundMutabilitySR(range, RuneUsage(a, func(resultRune)), RuneUsage(b, func(coordListRune)))
      case other => vimpl(other)
    }
  }

  private def inheritedMethodRune(interfaceA: InterfaceA, method: FunctionA, rune: IRuneS): IRuneS = {
    AnonymousSubstructMethodInheritedRuneS(interfaceA.name, method.name, rune)
  }

  private def makeStruct(interfaceA: InterfaceA, memberRunes: Vector[RuneUsage], members: Vector[NormalStructMemberS], structTemplateNameS: AnonymousSubstructTemplateNameS) = {
    val rulesBuilder = new Accumulator[IRulexSR]()
    val runeToType = mutable.HashMap[IRuneS, ITemplataType]()

    interfaceA.rules.foreach(x => rulesBuilder.add(x))

    runeToType ++= interfaceA.runeToType
    runeToType ++= memberRunes.map(_.rune -> CoordTemplataType())

    val voidRune = AnonymousSubstructVoidRuneS()
    runeToType += voidRune -> CoordTemplataType()
    rulesBuilder.add(
      LookupSR(
        interfaceA.range, RuneUsage(interfaceA.range, voidRune), interner.intern(CodeNameS(keywords.void))))

    val methodGenericParams =
      interfaceA.genericParameters ++ memberRunes.map(mr => GenericParameterS(mr.range, mr, Vector(), None))

    interfaceA.internalMethods.zip(memberRunes).zipWithIndex.foreach({ case ((internalMethod, memberRune), methodIndex) =>
      val methodRuneToType =
        internalMethod.runeToType.map({ case (methodRune, tyype) =>
          inheritedMethodRune(interfaceA, internalMethod, methodRune) -> tyype
        })
      runeToType ++= methodRuneToType
      val methodRules =
        internalMethod.rules.map(rule => mapRunes(rule, methodRune => {
          inheritedMethodRune(interfaceA, internalMethod, methodRune)
        }))
      rulesBuilder.addAll(methodRules)

      val returnRune = {
        val originalRetRune = vassertSome(internalMethod.maybeRetCoordRune)
        RuneUsage(
          originalRetRune.range,
          inheritedMethodRune(interfaceA, internalMethod, originalRetRune.rune))
      }

      {
        val selfBorrowCoordRuneS =
          AnonymousSubstructMethodSelfBorrowCoordRuneS(interfaceA.name, internalMethod.name)
        runeToType += selfBorrowCoordRuneS -> CoordTemplataType()
        rulesBuilder.add(
          AugmentSR(internalMethod.range, RuneUsage(internalMethod.range, selfBorrowCoordRuneS), BorrowP, memberRune))

        val paramRunes =
          internalMethod.params.map(_.pattern).map({
            case AtomSP(range, name, None, coordRune, destructure) => {
              RuneUsage(
                range,
                inheritedMethodRune(interfaceA, internalMethod, vassertSome(coordRune).rune))
            }
            case AtomSP(range, name, Some(_), coordRune, destructure) => {
              RuneUsage(range, selfBorrowCoordRuneS)
            }
          })
        val methodParamsListRune =
          RuneUsage(internalMethod.range, AnonymousSubstructFunctionBoundParamsListRuneS(interfaceA.name, internalMethod.name))
        rulesBuilder.add(PackSR(internalMethod.range, methodParamsListRune, paramRunes.toArray))
        runeToType.put(methodParamsListRune.rune, PackTemplataType(CoordTemplataType()))

        val methodPrototypeRune =
          RuneUsage(
            internalMethod.range,
            AnonymousSubstructFunctionBoundPrototypeRuneS(interfaceA.name, internalMethod.name))
        rulesBuilder.add(
          DefinitionFuncSR(
            internalMethod.range, methodPrototypeRune, keywords.underscoresCall, methodParamsListRune, returnRune))
        rulesBuilder.add(
          CallSiteFuncSR(
            internalMethod.range, methodPrototypeRune, keywords.underscoresCall, methodParamsListRune, returnRune))
        rulesBuilder.add(
          ResolveSR(
            internalMethod.range, methodPrototypeRune, keywords.underscoresCall, methodParamsListRune, returnRune))
        runeToType.put(methodPrototypeRune.rune, PrototypeTemplataType())
      }

      {
        val selfOwnCoordRuneS =
          AnonymousSubstructMethodSelfOwnCoordRuneS(interfaceA.name, internalMethod.name)
        runeToType += selfOwnCoordRuneS -> CoordTemplataType()
        rulesBuilder.add(
          AugmentSR(internalMethod.range, RuneUsage(internalMethod.range, selfOwnCoordRuneS), OwnP, memberRune))

        val dropParamsListRune =
          RuneUsage(internalMethod.range, AnonymousSubstructDropBoundParamsListRuneS(interfaceA.name, internalMethod.name))
        rulesBuilder.add(
          PackSR(
            internalMethod.range,
            dropParamsListRune,
            Array(RuneUsage(internalMethod.range, selfOwnCoordRuneS))))
        runeToType.put(dropParamsListRune.rune, PackTemplataType(CoordTemplataType()))

        val dropPrototypeRune =
          RuneUsage(
            internalMethod.range,
            AnonymousSubstructDropBoundPrototypeRuneS(interfaceA.name, internalMethod.name))
        rulesBuilder.add(
          DefinitionFuncSR(
            internalMethod.range, dropPrototypeRune, keywords.drop, dropParamsListRune, RuneUsage(internalMethod.range, voidRune)))
        rulesBuilder.add(
          CallSiteFuncSR(
            internalMethod.range, dropPrototypeRune, keywords.drop, dropParamsListRune, RuneUsage(internalMethod.range, voidRune)))
        rulesBuilder.add(
          ResolveSR(
            internalMethod.range, dropPrototypeRune, keywords.drop, dropParamsListRune, RuneUsage(internalMethod.range, voidRune)))
        runeToType.put(dropPrototypeRune.rune, PrototypeTemplataType())
      }
    })

    StructA(
      interfaceA.range,
      structTemplateNameS,
      Vector(),
      false,
      interfaceA.mutabilityRune,
      interfaceA.maybePredictedMutability,
      TemplateTemplataType(
        (interfaceA.tyype match {
          case KindTemplataType() => Vector()
          case TemplateTemplataType(paramTypes, KindTemplataType()) => paramTypes
        }) ++ memberRunes.map(_ => CoordTemplataType()),
        KindTemplataType()),
      methodGenericParams,
      runeToType.toMap,
      rulesBuilder.buildArray(),
      Map(),
      Array(),
      members)
  }

  private def makeForwarderFunction(
    structNameS: AnonymousSubstructTemplateNameS,
    interface: InterfaceA,
    struct: StructA,
    method: FunctionA,
    methodIndex: Int):
  FunctionA = {
    val structType = struct.tyype
    val FunctionA(methodRange, name, attributes, methodOriginalType, methodOriginalIdentifyingRunes, methodOriginalRuneToType, originalParams, maybeRetCoordRune, methodOriginalRules, body) = method

    vassert(struct.genericParameters.map(_.rune).startsWith(methodOriginalIdentifyingRunes.map(_.rune)))
    val genericParams = struct.genericParameters

    val runeToType = mutable.HashMap[IRuneS, ITemplataType]()
    runeToType ++= struct.headerRuneToType
    runeToType ++= struct.membersRuneToType

    val selfOwnershipRune = SelfOwnershipRuneS()
    runeToType.put(selfOwnershipRune, OwnershipTemplataType())
    val interfaceRune = AnonymousSubstructParentInterfaceTemplateRuneS()
    runeToType.put(interfaceRune, KindTemplataType())
    val selfKindRune = SelfKindRuneS()
    runeToType.put(selfKindRune, KindTemplataType())
    val selfCoordRune = SelfRuneS()
    runeToType.put(selfCoordRune, CoordTemplataType())
    val selfKindTemplateRune = SelfKindTemplateRuneS()
    runeToType.put(selfKindTemplateRune, structType)

    val rules = new Accumulator[IRulexSR]()
//    rules.addAll(methodOriginalRules)
    rules.addAll(struct.headerRules.toIterable)
    rules.addAll(struct.memberRules.toIterable)

    val abstractParamIndex =
      originalParams.indexWhere(param => {
        param.pattern.virtuality match {
          case Some(AbstractSP(_, _)) => true
          case _ => false
        }
      })
    vassert(abstractParamIndex >= 0)
    val abstractParam = originalParams(abstractParamIndex)
    val abstractParamRange = abstractParam.pattern.range
    val abstractParamCoordRune =
      RuneUsage(
        abstractParamRange,
        inheritedMethodRune(interface, method, vassertSome(abstractParam.pattern.coordRune).rune)) // https://github.com/ValeLang/Vale/issues/370

    val destructuringInterfaceRule =
      CoordComponentsSR(
        abstractParamRange,
        abstractParamCoordRune,
        RuneUsage(abstractParamRange, selfOwnershipRune),
        RuneUsage(abstractParamRange, interfaceRune))

    rules.add(destructuringInterfaceRule)
    val lookupStructTemplateRule =
      LookupSR(
        abstractParamRange,
        RuneUsage(abstractParamRange, selfKindTemplateRune),
        interner.intern(AnonymousSubstructTemplateImpreciseNameS(structNameS.interfaceName.getImpreciseName(interner))))
    rules.add(lookupStructTemplateRule)
    val lookupStructRule =
      CallSR(
        abstractParamRange,
        RuneUsage(abstractParamRange, selfKindRune),
        RuneUsage(abstractParamRange, selfKindTemplateRune),
        genericParams.map(_.rune).toArray)
    rules.add(lookupStructRule)

    val assemblingStructRule =
      CoordComponentsSR(
        abstractParamRange,
        RuneUsage(abstractParamRange, selfCoordRune),
        RuneUsage(abstractParamRange, selfOwnershipRune),
        RuneUsage(abstractParamRange, selfKindRune))
    rules.add(assemblingStructRule)

    val newParam =
      ParameterS(
        AtomSP(
          abstractParamRange,
          Some(CaptureS(interner.intern(SelfNameS()))),
          None,//Some(OverrideSP(abstractParamRange, RuneUsage(abstractParamCoordRune.range, AnonymousSubstructParentInterfaceTemplateRuneS()))),
          Some(RuneUsage(abstractParamCoordRune.range, selfCoordRune)),
          None))

    val newParams = originalParams.updated(abstractParamIndex, newParam)

    val newBody =
      FunctionCallSE(
        methodRange,
        DotSE(
          methodRange,
          LocalLoadSE(methodRange, interner.intern(SelfNameS()), UseP),
          interner.intern(StrI(methodIndex.toString)),
          false),
        // Params minus the abstract param
        (newParams.slice(0, abstractParamIndex) ++ newParams.slice(abstractParamIndex + 1, newParams.length))
          .map(param => vassertSome(param.pattern.name).name)
          .map(name => LocalLoadSE(methodRange, name, UseP)))

    FunctionA(
      methodRange,
      interner.intern(ForwarderFunctionDeclarationNameS(name, methodIndex)),
      attributes,
      TemplateTemplataType(
        (methodOriginalType match {
          case FunctionTemplataType() => Vector()
          case TemplateTemplataType(paramTypes, FunctionTemplataType()) => paramTypes
        }) ++ struct.genericParameters.map(_ => CoordTemplataType()),
        FunctionTemplataType()),
      genericParams,
      runeToType.toMap,
      newParams,
      maybeRetCoordRune.map({ case RuneUsage(range, retCoordRune) =>
        RuneUsage(range, inheritedMethodRune(interface, method, retCoordRune))
      }),
      rules.buildArray().toVector,
      CodeBodyS(
        BodySE(
          methodRange,
          Vector(),
          BlockSE(
            methodRange,
            newParams.map(param => vassertSome(param.pattern.name).name).map(LocalS(_, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            newBody))))
  }
}
