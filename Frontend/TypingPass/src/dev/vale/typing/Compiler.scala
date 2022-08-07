package dev.vale.typing

import dev.vale.{Err, Interner, Keywords, Ok, PackageCoordinate, PackageCoordinateMap, Profiler, RangeS, Result, vassert, vassertOne, vcurious, vfail, vimpl, vwat, _}
import dev.vale.options.GlobalOptions
import dev.vale.parsing.ast.{CallMacroP, DontCallMacroP, UseP}
import dev.vale.postparsing.patterns.AtomSP
import dev.vale.postparsing.rules.IRulexSR
import dev.vale.postparsing._
import dev.vale.typing.OverloadResolver.FindFunctionFailure
import dev.vale.typing.citizen._
import dev.vale.typing.expression.{ExpressionCompiler, IExpressionCompilerDelegate}
import dev.vale.typing.function.{DestructorCompiler, FunctionCompiler, FunctionCompilerCore, IFunctionCompilerDelegate, VirtualCompiler}
import dev.vale.typing.infer.IInfererDelegate
import dev.vale.typing.types._
import dev.vale.highertyping._
import OverloadResolver.FindFunctionFailure
import dev.vale
import dev.vale.highertyping.{ExportAsA, FunctionA, InterfaceA, ProgramA, StructA}
import dev.vale.typing.ast.{ConsecutorTE, EdgeT, FunctionHeaderT, LocationInFunctionEnvironment, ParameterT, PrototypeT, ReferenceExpressionTE, VoidLiteralTE}
import dev.vale.typing.env.{FunctionEnvEntry, FunctionEnvironment, GlobalEnvironment, IEnvEntry, IEnvironment, ImplEnvEntry, InterfaceEnvEntry, NodeEnvironment, NodeEnvironmentBox, PackageEnvironment, StructEnvEntry, TemplataEnvEntry, TemplatasStore}
import dev.vale.typing.macros.{AbstractBodyMacro, AnonymousInterfaceMacro, AsSubtypeMacro, FunctorHelper, IOnImplDefinedMacro, IOnInterfaceDefinedMacro, IOnStructDefinedMacro, LockWeakMacro, SameInstanceMacro, StructConstructorMacro}
import dev.vale.typing.macros.citizen.{ImplDropMacro, ImplFreeMacro, InterfaceDropMacro, InterfaceFreeMacro, StructDropMacro, StructFreeMacro}
import dev.vale.typing.macros.rsa.{RSADropIntoMacro, RSAFreeMacro, RSAImmutableNewMacro, RSALenMacro, RSAMutableCapacityMacro, RSAMutableNewMacro, RSAMutablePopMacro, RSAMutablePushMacro}
import dev.vale.typing.macros.ssa.{SSADropIntoMacro, SSAFreeMacro, SSALenMacro}
import dev.vale.typing.names._
import dev.vale.typing.templata._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.env._
import dev.vale.typing.expression.LocalHelper
import dev.vale.typing.types._
import dev.vale.typing.templata._
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.macros.citizen.StructDropMacro
import dev.vale.typing.macros.rsa.RSALenMacro
import dev.vale.typing.macros.ssa.SSALenMacro

import scala.collection.immutable.{List, ListMap, Map, Set}
import scala.collection.mutable
import scala.util.control.Breaks._

trait IFunctionGenerator {
  def generate(
    // These serve as the API that a function generator can use.
    // TODO: Give a trait with a reduced API.
    // Maybe this functionCompilerCore can be a lambda we can use to finalize and add &This* function.

    functionCompilerCore: FunctionCompilerCore,
    structCompiler: StructCompiler,
    destructorCompiler: DestructorCompiler,
    arrayCompiler: ArrayCompiler,
    env: FunctionEnvironment,
    coutputs: CompilerOutputs,
    life: LocationInFunctionEnvironment,
    callRange: RangeS,
    // We might be able to move these all into the function environment... maybe....
    originFunction: Option[FunctionA],
    paramCoords: Vector[ParameterT],
    maybeRetCoord: Option[CoordT]):
  (FunctionHeaderT)
}

object DefaultPrintyThing {
  def print(x: => Object) = {
//    println("###: " + x)
  }
}



class Compiler(
    opts: TypingPassOptions,
    interner: Interner,
    keywords: Keywords) {
  val debugOut = opts.debugOut
  val globalOptions = opts.globalOptions

  val nameTranslator = new NameTranslator(interner)

  val templataCompiler =
    new TemplataCompiler(
      interner,
      opts,
      nameTranslator,
      new ITemplataCompilerDelegate {
        override def isParent(coutputs: CompilerOutputs, descendantCitizenRef: ICitizenTT, ancestorInterfaceRef: InterfaceTT): IsParentResult = {
          implCompiler.isParent(coutputs, descendantCitizenRef, ancestorInterfaceRef, true)
        }

        override def resolveStruct(
          coutputs: CompilerOutputs,
          callingEnv: IEnvironment,
          callRange: RangeS,
          structTemplata: StructTemplata,
          uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
        StructTT = {
          structCompiler.resolveStruct(
            coutputs, callingEnv, callRange, structTemplata, uncoercedTemplateArgs)
        }

        override def resolveInterface(
            coutputs: CompilerOutputs,
            callingEnv: IEnvironment, // See CSSNCE
            callRange: RangeS,
            interfaceTemplata: InterfaceTemplata,
            uncoercedTemplateArgs: Vector[ITemplata[ITemplataType]]):
        InterfaceTT = {
          structCompiler.resolveInterface(
            coutputs, callingEnv, callRange, interfaceTemplata, uncoercedTemplateArgs)
        }

        override def resolveStaticSizedArrayKind(
            env: IEnvironment,
            coutputs: CompilerOutputs,
            mutability: ITemplata[MutabilityTemplataType],
            variability: ITemplata[VariabilityTemplataType],
            size: ITemplata[IntegerTemplataType],
            type2: CoordT
        ): StaticSizedArrayTT = {
          arrayCompiler.resolveStaticSizedArray(mutability, variability, size, type2)
        }

        override def resolveRuntimeSizedArrayKind(
            env: IEnvironment,
            state: CompilerOutputs,
            element: CoordT,
            arrayMutability: ITemplata[MutabilityTemplataType]):
        RuntimeSizedArrayTT = {
          arrayCompiler.resolveRuntimeSizedArray(element, arrayMutability)
        }
      })
  val inferCompiler: InferCompiler =
    new InferCompiler(
      opts,
      interner,
      nameTranslator,
      new IInfererDelegate {
        override def sanityCheckConclusion(env: InferEnv, state: CompilerOutputs, rune: IRuneS, templata: ITemplata[ITemplataType]): Unit = {
          val placeholderFullNames =
            Collector.all(templata, {
              case n@FullNameT(_, _, PlaceholderTemplateNameT(_)) => n
              case n@FullNameT(_, _, PlaceholderNameT(_)) => n
            })

          val originalCallingEnvTemplateName =
            env.originalCallingEnv.fullName match {
              case FullNameT(packageCoord, initSteps, x : IInstantiationNameT) => {
                FullNameT(packageCoord, initSteps, x.template)
              }
              // This might be the case if we're doing some preliminary solving without placeholders.
              case FullNameT(packageCoord, initSteps, x : ITemplateNameT) => {
                // (also, if we're here there really shouldnt be any placeholders)
                FullNameT(packageCoord, initSteps, x)
              }
              case other => vfail(other)
            }
          placeholderFullNames.foreach({ case FullNameT(paackage, initSteps, _) =>
            val placeholderDeclaringEnvName = FullNameT(paackage, initSteps.init, initSteps.last)
            vassert(placeholderDeclaringEnvName == originalCallingEnvTemplateName)
          })
        }

        override def lookupTemplata(
          envs: InferEnv,
          coutputs: CompilerOutputs,
          range: RangeS,
          name: INameT):
        ITemplata[ITemplataType] = {
          templataCompiler.lookupTemplata(envs.selfEnv, coutputs, range, name)
        }

        override def isDescendant(
          envs: InferEnv,
          coutputs: CompilerOutputs,
          kind: KindT):
        Boolean = {
          kind match {
            case PlaceholderT(_) => false // at least until we have type bounds
            case RuntimeSizedArrayTT(_, _) => false
            case OverloadSetT(_, _) => false
            case StaticSizedArrayTT(_, _, _, _) => false
            case s @ StructTT(_) => implCompiler.isDescendant(coutputs, s, false)
            case i @ InterfaceTT(_) => implCompiler.isDescendant(coutputs, i, false)
            case IntT(_) | BoolT() | FloatT() | StrT() | VoidT() => false
          }
        }

        override def isAncestor(
          envs: InferEnv,
          coutputs: CompilerOutputs,
          kind: KindT):
        Boolean = {
          kind match {
            case InterfaceTT(_) => true
            case _ => false
          }
        }

        def coerce(envs: InferEnv, state: CompilerOutputs, range: RangeS, toType: ITemplataType, templata: ITemplata[ITemplataType]): ITemplata[ITemplataType] = {
          templataCompiler.coerce(state, envs.originalCallingEnv, range, templata, toType)
        }

        override def lookupTemplataImprecise(envs: InferEnv, state: CompilerOutputs, range: RangeS, name: IImpreciseNameS): Option[ITemplata[ITemplataType]] = {
          templataCompiler.lookupTemplata(envs.selfEnv, state, range, name)
        }

        override def getMutability(state: CompilerOutputs, kind: KindT): ITemplata[MutabilityTemplataType] = {
            Compiler.getMutability(state, kind)
        }

        override def predictStaticSizedArrayKind(envs: InferEnv, state: CompilerOutputs, mutability: ITemplata[MutabilityTemplataType], variability: ITemplata[VariabilityTemplataType], size: ITemplata[IntegerTemplataType], element: CoordT): (StaticSizedArrayTT) = {
            arrayCompiler.resolveStaticSizedArray(mutability, variability, size, element)
        }

        override def predictRuntimeSizedArrayKind(envs: InferEnv, state: CompilerOutputs, element: CoordT, arrayMutability: ITemplata[MutabilityTemplataType]): RuntimeSizedArrayTT = {
            arrayCompiler.resolveRuntimeSizedArray(element, arrayMutability)
        }

        override def predictInterface(
          env: InferEnv,
          state: CompilerOutputs,
          callRange: RangeS,
          templata: InterfaceTemplata,
          templateArgs: Vector[ITemplata[ITemplataType]]):
        (KindT) = {
            structCompiler.predictInterface(
              state, env.originalCallingEnv, callRange, templata, templateArgs)
        }

        override def predictStruct(
          env: InferEnv,
          state: CompilerOutputs,
          callRange: RangeS,
          templata: StructTemplata,
          templateArgs: Vector[ITemplata[ITemplataType]]):
        (KindT) = {
          structCompiler.predictStruct(
            state, env.originalCallingEnv, callRange, templata, templateArgs)
        }

        override def kindIsFromTemplate(
          coutputs: CompilerOutputs,
          actualCitizenRef: KindT,
          expectedCitizenTemplata: ITemplata[ITemplataType]):
        Boolean = {
          actualCitizenRef match {
            case s : ICitizenTT => templataCompiler.citizenIsFromTemplate(s, expectedCitizenTemplata)
            case RuntimeSizedArrayTT(_, _) => (expectedCitizenTemplata == RuntimeSizedArrayTemplateTemplata())
            case StaticSizedArrayTT(_, _, _, _) => (expectedCitizenTemplata == StaticSizedArrayTemplateTemplata())
            case _ => false
          }
        }

        override def getAncestors(coutputs: CompilerOutputs, descendant: KindT, includeSelf: Boolean): Set[KindT] = {
            (if (includeSelf) {
              Set[KindT](descendant)
            } else {
              Set[KindT]()
            }) ++
              (descendant match {
                case s : ICitizenTT => implCompiler.getParents(coutputs, s, true)
                case _ => Array()
              })
        }

        override def getInterfaceTemplataType()(it: InterfaceTemplata): ITemplataType = {
            it.originInterface.tyype
        }

        override def getStructTemplataType()(st: StructTemplata): ITemplataType = {
            st.originStruct.tyype
        }

        override def structIsClosure(state: CompilerOutputs, structTT: StructTT): Boolean = {
            val structDef = state.lookupStruct(structTT)
            structDef.isClosure
        }

        def predictFunction(
          envs: InferEnv,
          state: CompilerOutputs,
          range: RangeS,
          name: StrI,
          paramCoords: Vector[CoordT],
          returnCoord: CoordT):
        PrototypeTemplata = {
          PrototypeTemplata(
            range,
            PrototypeT(
              envs.selfEnv.fullName.addStep(
                FunctionNameT(
                  FunctionTemplateNameT(name, range.begin),
                  Vector(),
                  paramCoords)),
              returnCoord))
        }

        override def assemblePrototype(
            envs: InferEnv,
            state: CompilerOutputs,
            range: RangeS,
            name: StrI,
            coords: Vector[CoordT],
            returnType: CoordT):
        PrototypeT = {
          PrototypeT(
            envs.selfEnv.fullName.addStep(
              interner.intern(
                FunctionNameT(
                  interner.intern(FunctionTemplateNameT(name, range.begin)), Vector(), coords))),
            returnType)
        }
      },
      new IInferCompilerDelegate {
        override def resolveInterface(
          callingEnv: IEnvironment,
          state: CompilerOutputs,
          callRange: RangeS,
          templata: InterfaceTemplata,
          templateArgs: Vector[ITemplata[ITemplataType]],
          verifyConclusions: Boolean):
        InterfaceTT = {
          vassert(verifyConclusions) // If we dont want to be verifying, we shouldnt be calling this func
          structCompiler.resolveInterface(state, callingEnv, callRange, templata, templateArgs)
        }

        override def resolveStruct(
          callingEnv: IEnvironment,
          state: CompilerOutputs,
          callRange: RangeS,
          templata: StructTemplata,
          templateArgs: Vector[ITemplata[ITemplataType]],
          verifyConclusions: Boolean):
        StructTT = {
          vassert(verifyConclusions) // If we dont want to be verifying, we shouldnt be calling this func
          structCompiler.resolveStruct(state, callingEnv, callRange, templata, templateArgs)
        }

        override def resolveFunction(
          callingEnv: IEnvironment,
          state: CompilerOutputs,
          range: RangeS,
          name: StrI,
          coords: Vector[CoordT],
          verifyConclusions: Boolean):
        Result[PrototypeTemplata, FindFunctionFailure] = {
          overloadCompiler.findFunction(
            callingEnv,
            state,
            range,
            interner.intern(CodeNameS(interner.intern(name))),
            Vector.empty,
            Array.empty,
            coords,
            Vector.empty,
            true,
            verifyConclusions)
        }

        override def resolveStaticSizedArrayKind(coutputs: CompilerOutputs, mutability: ITemplata[MutabilityTemplataType], variability: ITemplata[VariabilityTemplataType], size: ITemplata[IntegerTemplataType], element: CoordT): StaticSizedArrayTT = {
          arrayCompiler.resolveStaticSizedArray(mutability, variability, size, element)
        }

        override def resolveRuntimeSizedArrayKind(coutputs: CompilerOutputs, element: CoordT, arrayMutability: ITemplata[MutabilityTemplataType]): RuntimeSizedArrayTT = {
          arrayCompiler.resolveRuntimeSizedArray(element, arrayMutability)
        }
      })
  val convertHelper =
    new ConvertHelper(
      opts,
      new IConvertHelperDelegate {
        override def isParent(coutputs: CompilerOutputs, descendantCitizenRef: ICitizenTT, ancestorInterfaceRef: InterfaceTT): IsParentResult = {
          implCompiler.isParent(coutputs, descendantCitizenRef, ancestorInterfaceRef, true)
        }
      })

  val structCompiler: StructCompiler =
    new StructCompiler(
      opts,
      interner,
      keywords,
      nameTranslator,
      templataCompiler,
      inferCompiler,
      new IStructCompilerDelegate {
        override def evaluateOrdinaryFunctionFromNonCallForHeader(coutputs: CompilerOutputs, functionTemplata: FunctionTemplata, verifyConclusions: Boolean): FunctionHeaderT = {
          functionCompiler.evaluateOrdinaryFunctionFromNonCallForHeader(coutputs, functionTemplata, verifyConclusions)
        }

        override def evaluateTemplatedFunctionFromNonCallForHeader(coutputs: CompilerOutputs, functionTemplata: FunctionTemplata, verifyConclusions: Boolean): FunctionHeaderT = {
          functionCompiler.evaluateTemplatedFunctionFromNonCallForHeader(coutputs, functionTemplata, verifyConclusions)
        }

        override def scoutExpectedFunctionForPrototype(
          env: IEnvironment, coutputs: CompilerOutputs, callRange: RangeS, functionName: IImpreciseNameS,
          explicitTemplateArgRulesS: Vector[IRulexSR],
          explicitTemplateArgRunesS: Array[IRuneS],
          args: Vector[CoordT], extraEnvsToLookIn: Vector[IEnvironment], exact: Boolean, verifyConclusions: Boolean):
        PrototypeT = {
          overloadCompiler.findFunction(env, coutputs, callRange, functionName,
            explicitTemplateArgRulesS,
            explicitTemplateArgRunesS, args, extraEnvsToLookIn, exact, verifyConclusions) match {
            case Err(e) => throw CompileErrorExceptionT(CouldntFindFunctionToCallT(callRange, e))
            case Ok(x) => x.prototype
          }
        }
      })

  val implCompiler: ImplCompiler =
    new ImplCompiler(opts, interner, structCompiler, templataCompiler, inferCompiler)

  val functionCompiler: FunctionCompiler =
    new FunctionCompiler(opts, interner, keywords, nameTranslator, templataCompiler, inferCompiler, convertHelper, structCompiler,
      new IFunctionCompilerDelegate {
    override def evaluateBlockStatements(
        coutputs: CompilerOutputs,
        startingNenv: NodeEnvironment,
        nenv: NodeEnvironmentBox,
        life: LocationInFunctionEnvironment,
        exprs: BlockSE
    ): (ReferenceExpressionTE, Set[CoordT]) = {
      expressionCompiler.evaluateBlockStatements(coutputs, startingNenv, nenv, life, exprs)
    }

    override def translatePatternList(
      coutputs: CompilerOutputs,
      nenv: NodeEnvironmentBox,
      life: LocationInFunctionEnvironment,
      patterns1: Vector[AtomSP],
      patternInputExprs2: Vector[ReferenceExpressionTE]
    ): ReferenceExpressionTE = {
      expressionCompiler.translatePatternList(coutputs, nenv, life, patterns1, patternInputExprs2)
    }

//    override def evaluateParent(env: IEnvironment, coutputs: CompilerOutputs, callRange: RangeS, sparkHeader: FunctionHeaderT): Unit = {
//      virtualCompiler.evaluateParent(env, coutputs, callRange, sparkHeader)
//    }

    override def generateFunction(
      functionCompilerCore: FunctionCompilerCore,
      generator: IFunctionGenerator,
      fullEnv: FunctionEnvironment,
      coutputs: CompilerOutputs,
      life: LocationInFunctionEnvironment,
      callRange: RangeS,
      originFunction: Option[FunctionA],
      paramCoords: Vector[ParameterT],
      maybeRetCoord: Option[CoordT]):
    FunctionHeaderT = {
      generator.generate(

        functionCompilerCore, structCompiler, destructorCompiler, arrayCompiler, fullEnv, coutputs, life, callRange, originFunction, paramCoords, maybeRetCoord)
    }
  })
  val overloadCompiler: OverloadResolver = new OverloadResolver(opts, interner, keywords, templataCompiler, inferCompiler, functionCompiler)
  val destructorCompiler: DestructorCompiler = new DestructorCompiler(opts, interner, keywords, structCompiler, overloadCompiler)

  val virtualCompiler = new VirtualCompiler(opts, interner, overloadCompiler)

  val sequenceCompiler = new SequenceCompiler(opts, interner, keywords, structCompiler, templataCompiler)

  val arrayCompiler =
    new ArrayCompiler(
      opts,
      interner,
      keywords,
      inferCompiler,
      overloadCompiler)

  val expressionCompiler: ExpressionCompiler =
    new ExpressionCompiler(
      opts,
      interner,
      keywords,
      nameTranslator,
      templataCompiler,
      inferCompiler,
      arrayCompiler,
      structCompiler,
      implCompiler,
      sequenceCompiler,
      overloadCompiler,
      destructorCompiler,
      convertHelper,
      new IExpressionCompilerDelegate {
        override def evaluateTemplatedFunctionFromCallForPrototype(
            coutputs: CompilerOutputs,
            callingEnv: IEnvironment, // See CSSNCE
            callRange: RangeS,
            functionTemplata: FunctionTemplata,
            explicitTemplateArgs: Vector[ITemplata[ITemplataType]],
            args: Vector[CoordT]):
        FunctionCompiler.IEvaluateFunctionResult[PrototypeTemplata] = {
          functionCompiler.evaluateTemplatedFunctionFromCallForPrototype(coutputs, callRange, callingEnv, functionTemplata, explicitTemplateArgs, args, true)
        }

        override def evaluateClosureStruct(
            coutputs: CompilerOutputs,
            containingNodeEnv: NodeEnvironment,
            callRange: RangeS,
            name: IFunctionDeclarationNameS,
            function1: FunctionA):
        StructTT = {
          functionCompiler.evaluateClosureStruct(coutputs, containingNodeEnv, callRange, name, function1, true)
        }
      })

  val edgeCompiler = new EdgeCompiler(interner, keywords, overloadCompiler, implCompiler)

  val functorHelper = new FunctorHelper(interner, keywords)
  val structConstructorMacro = new StructConstructorMacro(opts, interner, keywords, nameTranslator)
  val structDropMacro = new StructDropMacro(interner, keywords, nameTranslator, destructorCompiler)
  val structFreeMacro = new StructFreeMacro(interner, keywords, nameTranslator, destructorCompiler)
  val interfaceFreeMacro = new InterfaceFreeMacro(interner, keywords, overloadCompiler)
  val asSubtypeMacro = new AsSubtypeMacro(keywords, implCompiler, expressionCompiler)
  val rsaLenMacro = new RSALenMacro(keywords)
  val rsaMutNewMacro = new RSAMutableNewMacro(interner, keywords)
  val rsaImmNewMacro = new RSAImmutableNewMacro(interner, keywords, overloadCompiler)
  val rsaPushMacro = new RSAMutablePushMacro(interner, keywords)
  val rsaPopMacro = new RSAMutablePopMacro(interner, keywords)
  val rsaCapacityMacro = new RSAMutableCapacityMacro(interner, keywords)
  val ssaLenMacro = new SSALenMacro(keywords)
  val rsaDropMacro = new RSADropIntoMacro(keywords, arrayCompiler)
  val ssaDropMacro = new SSADropIntoMacro(keywords, arrayCompiler)
  val rsaFreeMacro = new RSAFreeMacro(interner, keywords, arrayCompiler, overloadCompiler, destructorCompiler)
  val ssaFreeMacro = new SSAFreeMacro(interner, keywords, arrayCompiler, overloadCompiler, destructorCompiler)
//  val ssaLenMacro = new SSALenMacro(keywords)
  val implDropMacro = new ImplDropMacro(interner, nameTranslator)
  val implFreeMacro = new ImplFreeMacro(interner, keywords, nameTranslator)
  val interfaceDropMacro = new InterfaceDropMacro(interner, keywords, nameTranslator)
  val abstractBodyMacro = new AbstractBodyMacro(keywords)
  val lockWeakMacro = new LockWeakMacro(keywords, expressionCompiler)
  val sameInstanceMacro = new SameInstanceMacro(keywords)
  val anonymousInterfaceMacro =
    new AnonymousInterfaceMacro(
      opts, interner, keywords, nameTranslator, overloadCompiler, structCompiler, structConstructorMacro, structDropMacro, structFreeMacro, interfaceFreeMacro, implDropMacro)


  def evaluate(packageToProgramA: PackageCoordinateMap[ProgramA]): Result[Hinputs, ICompileErrorT] = {
    try {
      Profiler.frame(() => {
        val nameToStructDefinedMacro =
          Map(
            structConstructorMacro.macroName -> structConstructorMacro,
            structDropMacro.macroName -> structDropMacro,
            structFreeMacro.macroName -> structFreeMacro,
            implFreeMacro.macroName -> implFreeMacro)
        val nameToInterfaceDefinedMacro =
          Map(
            interfaceDropMacro.macroName -> interfaceDropMacro,
            interfaceFreeMacro.macroName -> interfaceFreeMacro,
            anonymousInterfaceMacro.macroName -> anonymousInterfaceMacro)
        val nameToImplDefinedMacro = Map[StrI, IOnImplDefinedMacro]()
        val nameToFunctionBodyMacro =
          Map(
            abstractBodyMacro.generatorId -> abstractBodyMacro,
            structConstructorMacro.generatorId -> structConstructorMacro,
            structFreeMacro.freeGeneratorId -> structFreeMacro,
            structDropMacro.dropGeneratorId -> structDropMacro,
            rsaLenMacro.generatorId -> rsaLenMacro,
            rsaMutNewMacro.generatorId -> rsaMutNewMacro,
            rsaImmNewMacro.generatorId -> rsaImmNewMacro,
            rsaPushMacro.generatorId -> rsaPushMacro,
            rsaPopMacro.generatorId -> rsaPopMacro,
            rsaCapacityMacro.generatorId -> rsaCapacityMacro,
            ssaLenMacro.generatorId -> ssaLenMacro,
            rsaDropMacro.generatorId -> rsaDropMacro,
            ssaDropMacro.generatorId -> ssaDropMacro,
            rsaFreeMacro.generatorId -> rsaFreeMacro,
            ssaFreeMacro.generatorId -> ssaFreeMacro,
            lockWeakMacro.generatorId -> lockWeakMacro,
            sameInstanceMacro.generatorId -> sameInstanceMacro,
            asSubtypeMacro.generatorId -> asSubtypeMacro)

        val fullNameAndEnvEntry: Vector[(FullNameT[INameT], IEnvEntry)] =
          packageToProgramA.flatMap({ case (coord, programA) =>
            val packageName = FullNameT(coord, Vector(), interner.intern(PackageTopLevelNameT()))
            programA.structs.map(structA => {
              val structNameT = packageName.addStep(nameTranslator.translateNameStep(structA.name))
              Vector((structNameT, StructEnvEntry(structA))) ++
                preprocessStruct(nameToStructDefinedMacro, structNameT, structA)
            }) ++
            programA.interfaces.map(interfaceA => {
              val interfaceNameT = packageName.addStep(nameTranslator.translateNameStep(interfaceA.name))

              Vector((interfaceNameT, InterfaceEnvEntry(interfaceA))) ++
                preprocessInterface(nameToInterfaceDefinedMacro, interfaceNameT, interfaceA)
            }) ++
            programA.impls.map(implA => {
              val implNameT = packageName.addStep(nameTranslator.translateImplName(implA.name))
              Vector((implNameT, ImplEnvEntry(implA))) ++
              implDropMacro.getImplSiblingEntries(implNameT, implA)
            }) ++
            programA.functions.map(functionA => {
              val functionNameT = packageName.addStep(nameTranslator.translateFunctionNameToTemplateName(functionA.name))
              Vector((functionNameT, FunctionEnvEntry(functionA)))
            })
          }).flatten.flatten.toVector

        val namespaceNameToTemplatas =
          fullNameAndEnvEntry
            .map({
              case (name, envEntry) => {
                (name.copy(last = interner.intern(PackageTopLevelNameT())), name.last, envEntry)
              }
            })
            .groupBy(_._1)
            .map({ case (namespaceFullName, envEntries) =>
              namespaceFullName ->
              TemplatasStore(namespaceFullName, Map(), Map())
                .addEntries(interner, envEntries.map({ case (_, b, c) => (b, c) }))
             }).toMap

        val globalEnv =
          GlobalEnvironment(
            functorHelper,
            structConstructorMacro,
            structDropMacro,
            structFreeMacro,
            interfaceDropMacro,
            interfaceFreeMacro,
            anonymousInterfaceMacro,
            nameToStructDefinedMacro,
            nameToInterfaceDefinedMacro,
            nameToImplDefinedMacro,
            nameToFunctionBodyMacro,
            namespaceNameToTemplatas,
            // Bulitins
            env.TemplatasStore(FullNameT(PackageCoordinate.BUILTIN(interner, keywords), Vector(), interner.intern(PackageTopLevelNameT())), Map(), Map()).addEntries(
              interner,
              Vector[(INameT, IEnvEntry)](
                interner.intern(PrimitiveNameT(keywords.int)) -> TemplataEnvEntry(KindTemplata(IntT.i32)),
                interner.intern(PrimitiveNameT(keywords.i64)) -> TemplataEnvEntry(KindTemplata(IntT.i64)),
                interner.intern(PrimitiveNameT(keywords.Array)) -> TemplataEnvEntry(RuntimeSizedArrayTemplateTemplata()),
                interner.intern(PrimitiveNameT(keywords.bool)) -> TemplataEnvEntry(KindTemplata(BoolT())),
                interner.intern(PrimitiveNameT(keywords.float)) -> TemplataEnvEntry(KindTemplata(FloatT())),
                interner.intern(PrimitiveNameT(keywords.__Never)) -> TemplataEnvEntry(KindTemplata(NeverT(false))),
                interner.intern(PrimitiveNameT(keywords.str)) -> TemplataEnvEntry(KindTemplata(StrT())),
                interner.intern(PrimitiveNameT(keywords.void)) -> TemplataEnvEntry(KindTemplata(VoidT())))))

        val coutputs = CompilerOutputs()

//        val emptyTupleStruct =
//          sequenceCompiler.makeTupleKind(
//            PackageEnvironment.makeTopLevelEnvironment(
//              globalEnv, FullNameT(PackageCoordinate.BUILTIN, Vector(), PackageTopLevelNameT())),
//            coutputs,
//            Vector())
//        val emptyTupleStructRef =
//          sequenceCompiler.makeTupleCoord(
//            PackageEnvironment.makeTopLevelEnvironment(
//              globalEnv, FullNameT(PackageCoordinate.BUILTIN, Vector(), PackageTopLevelNameT())),
//            coutputs,
//            Vector())

        arrayCompiler.compileStaticSizedArray(globalEnv, coutputs)
        arrayCompiler.compileRuntimeSizedArray(globalEnv, coutputs)


        globalEnv.nameToTopLevelEnvironment.foreach({ case (namespaceCoord, templatas) =>
          val env = PackageEnvironment.makeTopLevelEnvironment(globalEnv, namespaceCoord)
          templatas.entriesByNameT.map({ case (name, entry) =>
            entry match {
              case StructEnvEntry(structA) => {
                val templata = StructTemplata(env, structA)
                structCompiler.precompileStruct(coutputs, templata)
              }
              case InterfaceEnvEntry(interfaceA) => {
                val templata = InterfaceTemplata(env, interfaceA)
                structCompiler.precompileInterface(coutputs, templata)
              }
              case _ =>
            }
          })
        })

        // Indexing phase

        globalEnv.nameToTopLevelEnvironment.foreach({ case (namespaceCoord, templatas) =>
          val env = PackageEnvironment.makeTopLevelEnvironment(globalEnv, namespaceCoord)
          templatas.entriesByNameT.map({ case (name, entry) =>
            entry match {
              case StructEnvEntry(structA) => {
                val templata = StructTemplata(env, structA)
                structCompiler.compileStruct(coutputs, templata)
              }
              case InterfaceEnvEntry(interfaceA) => {
                val templata = InterfaceTemplata(env, interfaceA)
                structCompiler.compileInterface(coutputs, templata)
              }
              case _ =>
            }
          })
        })

        globalEnv.nameToTopLevelEnvironment.foreach({ case (namespaceCoord, templatas) =>
          val env = PackageEnvironment.makeTopLevelEnvironment(globalEnv, namespaceCoord)
          templatas.entriesByNameT.map({ case (name, entry) =>
            entry match {
              case ImplEnvEntry(impl) => {
                implCompiler.compileImpl(coutputs, ImplTemplata(env, impl))
              }
              case _ =>
            }
          })
        })

        globalEnv.nameToTopLevelEnvironment.foreach({ case (namespaceCoord, templatas) =>
          val env = PackageEnvironment.makeTopLevelEnvironment(globalEnv, namespaceCoord)
          templatas.entriesByNameT.map({ case (name, entry) =>
            entry match {
              case FunctionEnvEntry(functionA) => {
                functionCompiler.evaluateGenericFunctionFromNonCall(
                  coutputs, FunctionTemplata(env, functionA), true)
              }
              case _ =>
            }
          })
        })

        packageToProgramA.flatMap({ case (packageCoord, programA) =>
          val env =
            PackageEnvironment.makeTopLevelEnvironment(
              globalEnv, FullNameT(packageCoord, Vector(), interner.intern(PackageTopLevelNameT())))

          programA.exports.foreach({ case ExportAsA(range, exportedName, rules, runeToType, typeRuneA) =>
            val typeRuneT = typeRuneA

            val templataByRune =
              inferCompiler.solveExpectComplete(
                InferEnv(env, env), coutputs, rules, runeToType, range, Vector(), Vector(), true, true)
            val kind =
              templataByRune.get(typeRuneT.rune) match {
                case Some(KindTemplata(kind)) => {
                  coutputs.addKindExport(range, kind, range.file.packageCoordinate, exportedName)
                }
                case Some(prototype) => {
                  vimpl()
                }
                case _ => vfail()
              }
          })
        })

//        breakable {
//          while (true) {
        val denizensAtStart = coutputs.countDenizens()

        val builtinPackageCoord = PackageCoordinate.BUILTIN(interner, keywords)
        val rootPackageEnv =
          PackageEnvironment.makeTopLevelEnvironment(
            globalEnv,
            FullNameT(builtinPackageCoord, Vector(), interner.intern(PackageTopLevelNameT())))

        val freeImpreciseName = interner.intern(FreeImpreciseNameS())
        val dropImpreciseName = interner.intern(CodeNameS(keywords.drop))

//        val immutableKinds =
//          coutputs.getAllStructs().filter(_.mutability == MutabilityTemplata(ImmutableT)).map(_.templateName) ++
//            coutputs.getAllInterfaces().filter(_.mutability == ImmutableT).map(_.templateName) ++
//            coutputs.getAllRuntimeSizedArrays().filter(_.mutability == MutabilityTemplata(ImmutableT)) ++
//            coutputs.getAllStaticSizedArrays().filter(_.mutability == MutabilityTemplata(ImmutableT))
//        immutableKinds.foreach(kind => {
//          val kindEnv = coutputs.getEnvForTemplate(kind)
//          functionCompiler.evaluateGenericFunctionFromNonCall(
//            coutputs,
//            kindEnv.lookupNearestWithImpreciseName(freeImpreciseName, Set(ExpressionLookupContext)) match {
//              case Some(ft@FunctionTemplata(_, _)) => ft
//              case _ => throw CompileErrorExceptionT(RangedInternalErrorT(RangeS.internal(interner, -1663), "Couldn't find free for immutable struct!"))
//            })
//          functionCompiler.evaluateGenericFunctionFromNonCall(
//            coutputs,
//            kindEnv.lookupNearestWithImpreciseName(dropImpreciseName, Set(ExpressionLookupContext)) match {
//              case Some(ft@FunctionTemplata(_, _)) => ft
//              case _ => throw CompileErrorExceptionT(RangedInternalErrorT(RangeS.internal(interner, -1663), "Couldn't find free for immutable struct!"))
//            })
//        })

        Profiler.frame(() => {
//                val env =
//                  PackageEnvironment.makeTopLevelEnvironment(
//                    globalEnv, FullNameT(PackageCoordinate.BUILTIN, Vector(), interner.intern(PackageTopLevelNameT())))

                // Returns the number of overrides stamped
                // This doesnt actually stamp *all* overrides, just the ones we can immediately
                // see missing. We don't know if, in the process of stamping these, we'll find more.
                // Also note, these don't stamp them right now, they defer them for later evaluating.
          edgeCompiler.compileITables(coutputs)
        })

//            var deferredFunctionsEvaluated = 0
//            while (coutputs.peekNextDeferredEvaluatingFunction().nonEmpty) {
//              val nextDeferredEvaluatingFunction = coutputs.peekNextDeferredEvaluatingFunction().get
//              deferredFunctionsEvaluated += 1
//              // No, IntelliJ, I assure you this has side effects
//              (nextDeferredEvaluatingFunction.call) (coutputs)
//              coutputs.markDeferredFunctionEvaluated(nextDeferredEvaluatingFunction.prototypeT)
//            }


//            val denizensAtEnd = coutputs.countDenizens()
//            if (denizensAtStart == denizensAtEnd)
//              break
//          }
//        }

        val (interfaceEdgeBlueprints, interfaceToStructToMethods) = edgeCompiler.compileITables(coutputs)
        val edges =
          interfaceToStructToMethods.flatMap({ case (interface, structToMethods) =>
            structToMethods.map({ case (struct, methods) =>
              EdgeT(struct, interface, methods)
            })
          })

//        // NEVER ZIP TWO SETS TOGETHER
//        val edgeBlueprintsAsList = edgeBlueprints.toVector
//        val edgeBlueprintsByInterface = edgeBlueprintsAsList.map(_.interface).zip(edgeBlueprintsAsList).toMap;
//
//        edgeBlueprintsByInterface.foreach({ case (interfaceTT, edgeBlueprint) =>
//          vassert(edgeBlueprint.interface == interfaceTT)
//        })

        ensureDeepExports(coutputs)

        val (
          reachableInterfaces,
          reachableStructs,
//          reachableSSAs,
//          reachableRSAs,
          reachableFunctions) =
        if (opts.treeShakingEnabled) {
          vimpl()
//          Profiler.frame(() => {
//            val reachables = Reachability.findReachables(coutputs, interfaceEdgeBlueprints, interfaceToStructToMethods)
//
//            val categorizedFunctions = coutputs.getAllFunctions().groupBy(f => reachables.functions.contains(f.header.toSignature))
//            val reachableFunctions = categorizedFunctions.getOrElse(true, Vector.empty)
//            val unreachableFunctions = categorizedFunctions.getOrElse(false, Vector.empty)
//            unreachableFunctions.foreach(f => debugOut("Shaking out unreachable: " + f.header.fullName))
//            reachableFunctions.foreach(f => debugOut("Including: " + f.header.fullName))
//
//            val categorizedSSAs = coutputs.getAllStaticSizedArrays().groupBy(f => reachables.staticSizedArrays.contains(f))
//            val reachableSSAs = categorizedSSAs.getOrElse(true, Vector.empty)
//            val unreachableSSAs = categorizedSSAs.getOrElse(false, Vector.empty)
//            unreachableSSAs.foreach(f => debugOut("Shaking out unreachable: " + f))
//            reachableSSAs.foreach(f => debugOut("Including: " + f))
//
//            val categorizedRSAs = coutputs.getAllRuntimeSizedArrays().groupBy(f => reachables.runtimeSizedArrays.contains(f))
//            val reachableRSAs = categorizedRSAs.getOrElse(true, Vector.empty)
//            val unreachableRSAs = categorizedRSAs.getOrElse(false, Vector.empty)
//            unreachableRSAs.foreach(f => debugOut("Shaking out unreachable: " + f))
//            reachableRSAs.foreach(f => debugOut("Including: " + f))
//
//            val categorizedStructs = coutputs.getAllStructs().groupBy(f => reachables.structs.contains(f.getRef))
//            val reachableStructs = categorizedStructs.getOrElse(true, Vector.empty)
//            val unreachableStructs = categorizedStructs.getOrElse(false, Vector.empty)
//            unreachableStructs.foreach(f => {
//              debugOut("Shaking out unreachable: " + f.fullName)
//            })
//            reachableStructs.foreach(f => debugOut("Including: " + f.fullName))
//
//            val categorizedInterfaces = coutputs.getAllInterfaces().groupBy(f => reachables.interfaces.contains(f.getRef))
//            val reachableInterfaces = categorizedInterfaces.getOrElse(true, Vector.empty)
//            val unreachableInterfaces = categorizedInterfaces.getOrElse(false, Vector.empty)
//            unreachableInterfaces.foreach(f => debugOut("Shaking out unreachable: " + f.fullName))
//            reachableInterfaces.foreach(f => debugOut("Including: " + f.fullName))
//
//            val categorizedEdges =
//              edges.groupBy(f => reachables.edges.contains(f))
//            val reachableEdges = categorizedEdges.getOrElse(true, Vector.empty)
//            val unreachableEdges = categorizedEdges.getOrElse(false, Vector.empty)
//            unreachableEdges.foreach(f => debugOut("Shaking out unreachable: " + f))
//            reachableEdges.foreach(f => debugOut("Including: " + f))
//
//            (reachableInterfaces, reachableStructs, reachableSSAs, reachableRSAs, reachableFunctions)
//          })
        } else {
          (
            coutputs.getAllInterfaces(),
            coutputs.getAllStructs(),
//            coutputs.getAllStaticSizedArrays(),
//            coutputs.getAllRuntimeSizedArrays(),
            coutputs.getAllFunctions())
        }

//      val allKinds =
//        reachableStructs.map(_.place) ++ reachableInterfaces.map(_.getRef) ++ reachableSSAs ++ reachableRSAs
//      val reachableImmKinds: Vector[KindT] =
//        allKinds
//          .filter({
//            case s@StructTT(_) => coutputs.lookupMutability(s) == ImmutableT
//            case i@InterfaceTT(_) => coutputs.lookupMutability(i) == ImmutableT
//            case StaticSizedArrayTT(_, m, _, _) => m == ImmutableT
//            case RuntimeSizedArrayTT(m, _) => m == ImmutableT
//            case _ => true
//          })
//          .toVector
//      val reachableImmKindToDestructor = reachableImmKinds.zip(reachableImmKinds.map(coutputs.findImmDestructor)).toMap

      val hinputs =
          vale.typing.Hinputs(
            reachableInterfaces.toVector,
            reachableStructs.toVector,
            reachableFunctions.toVector,
            //reachableImmKindToDestructor,
            interfaceEdgeBlueprints.groupBy(_.interface).mapValues(vassertOne(_)),
            edges.toVector,
            coutputs.getKindExports,
            coutputs.getFunctionExports,
            coutputs.getKindExterns,
            coutputs.getFunctionExterns)

        vassert(reachableFunctions.toVector.map(_.header.fullName).distinct.size == reachableFunctions.toVector.map(_.header.fullName).size)

        Ok(hinputs)
      })
    } catch {
      case CompileErrorExceptionT(err) => Err(err)
    }
  }

  private def preprocessStruct(
    nameToStructDefinedMacro: Map[StrI, IOnStructDefinedMacro],
    structNameT: FullNameT[INameT],
    structA: StructA): Vector[(FullNameT[INameT], IEnvEntry)] = {
    val defaultCalledMacros =
      Vector(
        MacroCallS(structA.range, CallMacroP, keywords.DeriveStructConstructor),
        MacroCallS(structA.range, CallMacroP, keywords.DeriveStructDrop),
        MacroCallS(structA.range, CallMacroP, keywords.DeriveStructFree),
        MacroCallS(structA.range, CallMacroP, keywords.DeriveImplFree))
    determineMacrosToCall(nameToStructDefinedMacro, defaultCalledMacros, structA.range, structA.attributes)
      .flatMap(_.getStructSiblingEntries(structNameT, structA))
  }

  private def preprocessInterface(
    nameToInterfaceDefinedMacro: Map[StrI, IOnInterfaceDefinedMacro],
    interfaceNameT: FullNameT[INameT],
    interfaceA: InterfaceA): Vector[(FullNameT[INameT], IEnvEntry)] = {
    val defaultCalledMacros =
      Vector(
        MacroCallS(interfaceA.range, CallMacroP, keywords.DeriveInterfaceDrop),
        MacroCallS(interfaceA.range, CallMacroP, keywords.DeriveInterfaceFree),
        MacroCallS(interfaceA.range, CallMacroP, keywords.DeriveAnonymousSubstruct))
    determineMacrosToCall(nameToInterfaceDefinedMacro, defaultCalledMacros, interfaceA.range, interfaceA.attributes)
      .flatMap(_.getInterfaceSiblingEntries(interfaceNameT, interfaceA))
  }

  private def determineMacrosToCall[T](
      nameToMacro: Map[StrI, T],
      defaultCalledMacros: Vector[MacroCallS],
      range: RangeS,
      attributes: Vector[ICitizenAttributeS]):
  Vector[T] = {
    attributes.foldLeft(defaultCalledMacros)({
      case (macrosToCall, mc@MacroCallS(range, CallMacroP, macroName)) => {
        if (macrosToCall.exists(_.macroName == macroName)) {
          throw CompileErrorExceptionT(RangedInternalErrorT(range, "Calling macro twice: " + macroName))
        }
        macrosToCall :+ mc
      }
      case (macrosToCall, MacroCallS(_, DontCallMacroP, macroName)) => macrosToCall.filter(_.macroName != macroName)
      case (macrosToCall, _) => macrosToCall
    }).map(macroCall => {
      nameToMacro.get(macroCall.macroName) match {
        case None => {
          throw CompileErrorExceptionT(RangedInternalErrorT(range, "Macro not found: " + macroCall.macroName))
        }
        case Some(m) => m
      }
    })
  }

  def ensureDeepExports(coutputs: CompilerOutputs): Unit = {
    val packageToKindToExport =
      coutputs.getKindExports
        .map(kindExport => (kindExport.packageCoordinate, kindExport.tyype, kindExport))
        .groupBy(_._1)
        .mapValues(
          _.map(x => (x._2, x._3))
            .groupBy(_._1)
            .mapValues({
              case Vector() => vwat()
              case Vector(only) => only
              case multiple => {
                val exports = multiple.map(_._2)
                throw CompileErrorExceptionT(
                  TypeExportedMultipleTimes(
                    exports.head.range,
                    exports.head.packageCoordinate,
                    exports))
              }
            }))

    coutputs.getFunctionExports.foreach(funcExport => {
      val exportedKindToExport = packageToKindToExport.getOrElse(funcExport.packageCoordinate, Map())
      (Vector(funcExport.prototype.returnType) ++ funcExport.prototype.paramTypes)
        .foreach(paramType => {
          if (!Compiler.isPrimitive(paramType.kind) && !exportedKindToExport.contains(paramType.kind)) {
            throw CompileErrorExceptionT(
              ExportedFunctionDependedOnNonExportedKind(
                funcExport.range, funcExport.packageCoordinate, funcExport.prototype.toSignature, paramType.kind))
          }
        })
    })
    coutputs.getFunctionExterns.foreach(functionExtern => {
      val exportedKindToExport = packageToKindToExport.getOrElse(functionExtern.packageCoordinate, Map())
      (Vector(functionExtern.prototype.returnType) ++ functionExtern.prototype.paramTypes)
        .foreach(paramType => {
          if (!Compiler.isPrimitive(paramType.kind) && !exportedKindToExport.contains(paramType.kind)) {
            throw CompileErrorExceptionT(
              ExternFunctionDependedOnNonExportedKind(
                functionExtern.range, functionExtern.packageCoordinate, functionExtern.prototype.toSignature, paramType.kind))
          }
        })
    })
    packageToKindToExport.foreach({ case (packageCoord, exportedKindToExport) =>
      exportedKindToExport.foreach({ case (exportedKind, (kind, export)) =>
        exportedKind match {
          case sr@StructTT(_) => {
            val structDef = coutputs.lookupStruct(sr)
            structDef.members.foreach({ case StructMemberT(_, _, member) =>
              val UnsubstitutedCoordT(CoordT(_, memberKind)) = member.reference
              if (structDef.mutability == MutabilityTemplata(ImmutableT) && !Compiler.isPrimitive(memberKind) && !exportedKindToExport.contains(memberKind)) {
                throw CompileErrorExceptionT(
                  vale.typing.ExportedImmutableKindDependedOnNonExportedKind(
                    export.range, packageCoord, exportedKind, memberKind))
              }
            })
          }
          case StaticSizedArrayTT(_, mutability, _, CoordT(_, elementKind)) => {
            if (mutability == MutabilityTemplata(ImmutableT) && !Compiler.isPrimitive(elementKind) && !exportedKindToExport.contains(elementKind)) {
              throw CompileErrorExceptionT(
                vale.typing.ExportedImmutableKindDependedOnNonExportedKind(
                  export.range, packageCoord, exportedKind, elementKind))
            }
          }
          case RuntimeSizedArrayTT(mutability, CoordT(_, elementKind)) => {
            if (mutability == MutabilityTemplata(ImmutableT) && !Compiler.isPrimitive(elementKind) && !exportedKindToExport.contains(elementKind)) {
              throw CompileErrorExceptionT(
                vale.typing.ExportedImmutableKindDependedOnNonExportedKind(
                  export.range, packageCoord, exportedKind, elementKind))
            }
          }
          case InterfaceTT(_) =>
        }
      })
    })
  }

  // Returns whether we should eagerly compile this and anything it depends on.
  def isRootFunction(functionA: FunctionA): Boolean = {
    functionA.name match {
      case FunctionNameS(StrI("main"), _) => return true
      case _ =>
    }
    functionA.attributes.exists({
      case ExportS(_) => true
      case ExternS(_) => true
      case _ => false
    })
  }

  // Returns whether we should eagerly compile this and anything it depends on.
  def isRootStruct(structA: StructA): Boolean = {
    structA.attributes.exists({ case ExportS(_) => true case _ => false })
  }

  // Returns whether we should eagerly compile this and anything it depends on.
  def isRootInterface(interfaceA: InterfaceA): Boolean = {
    interfaceA.attributes.exists({ case ExportS(_) => true case _ => false })
  }
}


object Compiler {
  // Flattens any nested ConsecutorTEs
  def consecutive(exprs: Vector[ReferenceExpressionTE]): ReferenceExpressionTE = {
    exprs match {
      case Vector() => vwat("Shouldn't have zero-element consecutors!")
      case Vector(only) => only
      case _ => {
        val flattened =
          exprs.flatMap({
            case ConsecutorTE(exprs) => exprs
            case other => Vector(other)
          })

        val withoutInitVoids =
          flattened.init
            .filter({ case VoidLiteralTE() => false case _ => true }) :+
            flattened.last

        withoutInitVoids match {
          case Vector() => vwat("Shouldn't have zero-element consecutors!")
          case Vector(only) => only
          case _ => ConsecutorTE(withoutInitVoids)
        }
      }
    }
  }

  def isPrimitive(kind: KindT): Boolean = {
    kind match {
      case VoidT() | IntT(_) | BoolT() | StrT() | NeverT(_) | FloatT() => true
//      case TupleTT(_, understruct) => isPrimitive(understruct)
      case StructTT(_) => false
      case InterfaceTT(_) => false
      case StaticSizedArrayTT(_, _, _, _) => false
      case RuntimeSizedArrayTT(_, _) => false
    }
  }

  def getMutabilities(coutputs: CompilerOutputs, concreteValues2: Vector[KindT]):
  Vector[ITemplata[MutabilityTemplataType]] = {
    concreteValues2.map(concreteValue2 => getMutability(coutputs, concreteValue2))
  }

  def getMutability(coutputs: CompilerOutputs, concreteValue2: KindT):
  ITemplata[MutabilityTemplataType] = {
    concreteValue2 match {
      case PlaceholderT(_) => MutabilityTemplata(MutableT)
      case NeverT(_) => MutabilityTemplata(ImmutableT)
      case IntT(_) => MutabilityTemplata(ImmutableT)
      case FloatT() => MutabilityTemplata(ImmutableT)
      case BoolT() => MutabilityTemplata(ImmutableT)
      case StrT() => MutabilityTemplata(ImmutableT)
      case VoidT() => MutabilityTemplata(ImmutableT)
      case RuntimeSizedArrayTT(mutability, _) => mutability
      case StaticSizedArrayTT(_, mutability, _, _) => mutability
      case sr @ StructTT(name) => coutputs.lookupMutability(TemplataCompiler.getStructTemplate(name))
      case ir @ InterfaceTT(name) => coutputs.lookupMutability(TemplataCompiler.getInterfaceTemplate(name))
//      case PackTT(_, sr) => coutputs.lookupMutability(sr)
//      case TupleTT(_, sr) => coutputs.lookupMutability(sr)
      case OverloadSetT(_, _) => {
        // Just like FunctionT2
        MutabilityTemplata(ImmutableT)
      }
    }
  }
}
