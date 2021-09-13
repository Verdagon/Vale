package net.verdagon.vale.astronomer

import net.verdagon.vale.scout.predictor.AstronomySolver
import net.verdagon.vale.templar.types._
import net.verdagon.vale.parser.{CaptureP, FailedParse, FileP, ImmutableP, MutabilityP, MutableP}
import net.verdagon.vale.scout.{ExportS, ExternS, Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, CaptureS, OverrideSP}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.{Err, FileCoordinateMap, IPackageResolver, Ok, PackageCoordinate, PackageCoordinateMap, Result, vassert, vassertSome, vcurious, vfail, vimpl, vwat}

import scala.collection.immutable.List
import scala.collection.mutable

case class Astrouts(
  codeLocationToMaybeType: mutable.HashMap[CodeLocationS, Option[ITemplataType]],
  codeLocationToStruct: mutable.HashMap[CodeLocationS, StructA],
  codeLocationToInterface: mutable.HashMap[CodeLocationS, InterfaceA])

// Environments dont have an AbsoluteName, because an environment can span multiple
// files.
case class Environment(
    maybeName: Option[INameS],
    maybeParentEnv: Option[Environment],
    primitives: Map[String, ITemplataType],
    codeMap: PackageCoordinateMap[ProgramS],
    typeByRune: Map[IRuneS, ITemplataType]) {
  override def hashCode(): Int = vcurious()

  val structsS: Vector[StructS] = codeMap.moduleToPackagesToContents.values.flatMap(_.values.flatMap(_.structs)).toVector
  val interfacesS: Vector[InterfaceS] = codeMap.moduleToPackagesToContents.values.flatMap(_.values.flatMap(_.interfaces)).toVector
  val implsS: Vector[ImplS] = codeMap.moduleToPackagesToContents.values.flatMap(_.values.flatMap(_.impls)).toVector
  val functionsS: Vector[FunctionS] = codeMap.moduleToPackagesToContents.values.flatMap(_.values.flatMap(_.implementedFunctions)).toVector
  val exportsS: Vector[ExportAsS] = codeMap.moduleToPackagesToContents.values.flatMap(_.values.flatMap(_.exports)).toVector
  val imports: Vector[ImportS] = codeMap.moduleToPackagesToContents.values.flatMap(_.values.flatMap(_.imports)).toVector

  def addRunes(newTypeByRune: Map[IRuneS, ITemplataType]): Environment = {
    Environment(maybeName, maybeParentEnv, primitives, codeMap, typeByRune ++ newTypeByRune)
  }

  // Returns whether the imprecise name could be referring to the absolute name.
  // See MINAAN for what we're doing here.
  def impreciseNameMatchesAbsoluteName(
    absoluteName: INameS,
    needleImpreciseNameS: IImpreciseNameStepS):
  Boolean = {
    (absoluteName, needleImpreciseNameS) match {
      case (TopLevelCitizenDeclarationNameS(humanNameA, _), CodeTypeNameS(humanNameB)) => humanNameA == humanNameB
      case _ => vimpl()
    }

//    val envNameSteps = maybeName.map(_.steps).getOrElse(Vector.empty)
//
//    // See MINAAN for what we're doing here.
//    absoluteNameEndsWithImpreciseName(absoluteName, needleImpreciseNameS) match {
//      case None => false
//      case Some(absoluteNameFirstHalf) => {
//        if (absoluteNameFirstHalf.steps.size > envNameSteps.size) {
//          false
//        } else {
//          (absoluteNameFirstHalf.steps.map(Some(_)) ++ envNameSteps.map(_ => None))
//            .zip(envNameSteps)
//            .forall({
//              case (None, _) => true
//              case (Some(firstHalfNameStep), envNameStep) => firstHalfNameStep == envNameStep
//            })
//        }
//      }
//    }
  }

  def lookupType(needleImpreciseNameS: IImpreciseNameStepS):
  (Option[ITemplataType], Vector[StructS], Vector[InterfaceS]) = {
    // See MINAAN for what we're doing here.

    val nearStructs = structsS.filter(struct => {
      impreciseNameMatchesAbsoluteName(struct.name, needleImpreciseNameS)
    })
    val nearInterfaces = interfacesS.filter(interface => {
      impreciseNameMatchesAbsoluteName(interface.name, needleImpreciseNameS)
    })
    val nearPrimitives =
      needleImpreciseNameS match {
        case CodeTypeNameS(nameStr) => primitives.get(nameStr)
        case _ => None
      }

    if (nearPrimitives.nonEmpty || nearStructs.nonEmpty || nearInterfaces.nonEmpty) {
      return (nearPrimitives, nearStructs.toVector, nearInterfaces.toVector)
    }
    maybeParentEnv match {
      case None => (None, Vector.empty, Vector.empty)
      case Some(parentEnv) => parentEnv.lookupType(needleImpreciseNameS)
    }
  }

  def lookupType(name: INameS):
  (Vector[StructS], Vector[InterfaceS]) = {
    val nearStructs = structsS.filter(_.name == name)
    val nearInterfaces = interfacesS.filter(_.name == name)

    if (nearStructs.nonEmpty || nearInterfaces.nonEmpty) {
      return (nearStructs.toVector, nearInterfaces.toVector)
    }
    maybeParentEnv match {
      case None => (Vector.empty, Vector.empty)
      case Some(parentEnv) => parentEnv.lookupType(name)
    }
  }

  def lookupRune(name: IRuneS): ITemplataType = {
    typeByRune.get(name) match {
      case Some(tyype) => tyype
      case None => {
        maybeParentEnv match {
          case None => vfail()
          case Some(parentEnv) => parentEnv.lookupRune(name)
        }
      }
    }
  }
}

object Astronomer {
  val primitives =
    Map(
      "int" -> KindTemplataType,
      "i64" -> KindTemplataType,
      "str" -> KindTemplataType,
      "bool" -> KindTemplataType,
      "float" -> KindTemplataType,
      "void" -> KindTemplataType,
//      "IFunction1" -> TemplateTypeSR(Vector(MutabilityTypeSR, CoordTypeSR, CoordTypeSR), KindTypeSR),
      "Array" -> TemplateTemplataType(Vector(MutabilityTemplataType, VariabilityTemplataType, CoordTemplataType), KindTemplataType))


  def lookupType(astrouts: Astrouts,  env: Environment, range: RangeS, name: INameS): ITemplataType = {
    // When the scout comes across a lambda, it doesn't put the e.g. main:lam1:__Closure struct into
    // the environment or anything, it lets templar to do that (because templar knows the actual types).
    // However, this means that when the lambda function gets to the astronomer, the astronomer doesn't
    // know what to do with it.

    name match {
      case LambdaNameS(_) =>
      case FunctionNameS(_, _) =>
      case TopLevelCitizenDeclarationNameS(_, _) =>
      case LambdaStructNameS(_) => return KindTemplataType
      case ImplNameS(_, _) => vwat()
      case LetNameS(_) => vwat()
//      case UnnamedLocalNameS(_) => vwat()
      case ClosureParamNameS() => vwat()
      case MagicParamNameS(_) => vwat()
      case CodeVarNameS(_) => vwat()
    }

    val (structsS, interfacesS) = env.lookupType(name)

    if (structsS.isEmpty && interfacesS.isEmpty) {
      ErrorReporter.report(RangedInternalErrorA(range, "Nothing found with name " + name))
    }
    if (structsS.size.signum + interfacesS.size.signum > 1) {
      ErrorReporter.report(RangedInternalErrorA(range, "Name doesn't correspond to only one of primitive or struct or interface: " + name))
    }

    if (structsS.nonEmpty) {
      val types = structsS.map(getStructType(astrouts, env, _))
      if (types.toSet.size > 1) {
        ErrorReporter.report(RangedInternalErrorA(range, "'" + name + "' has multiple types: " + types.toSet))
      }
      val tyype = types.head
      tyype
    } else if (interfacesS.nonEmpty) {
      val types = interfacesS.map(getInterfaceType(astrouts, env, _))
      if (types.toSet.size > 1) {
        ErrorReporter.report(RangedInternalErrorA(range, "'" + name + "' has multiple types: " + types.toSet))
      }
      val tyype = types.head
      tyype
    } else vfail()
  }

  def lookupType(astrouts: Astrouts, env: Environment, range: RangeS, name: CodeTypeNameS): ITemplataType = {
    // When the scout comes across a lambda, it doesn't put the e.g. __Closure<main>:lam1 struct into
    // the environment or anything, it lets templar to do that (because templar knows the actual types).
    // However, this means that when the lambda function gets to the astronomer, the astronomer doesn't
    // know what to do with it.

    val (primitivesS, structsS, interfacesS) = env.lookupType(name)

    if (primitivesS.isEmpty && structsS.isEmpty && interfacesS.isEmpty) {
      ErrorReporter.report(CouldntFindTypeA(range, name.name))
    }
    if (primitivesS.size.signum + structsS.size.signum + interfacesS.size.signum > 1) {
      ErrorReporter.report(RangedInternalErrorA(range, "Name doesn't correspond to only one of primitive or struct or interface: " + name))
    }

    if (primitivesS.nonEmpty) {
      vassert(primitivesS.size == 1)
      primitivesS.get
    } else if (structsS.nonEmpty) {
      val types = structsS.map(getStructType(astrouts, env, _))
      if (types.toSet.size > 1) {
        ErrorReporter.report(RangedInternalErrorA(range, "'" + name + "' has multiple types: " + types.toSet))
      }
      val tyype = types.head
      tyype
    } else if (interfacesS.nonEmpty) {
      val types = interfacesS.map(getInterfaceType(astrouts, env, _))
      if (types.toSet.size > 1) {
        ErrorReporter.report(RangedInternalErrorA(range, "'" + name + "' has multiple types: " + types.toSet))
      }
      val tyype = types.head
      tyype
    } else vfail()
  }

  def getStructType(
    astrouts: Astrouts,
    env: Environment,
    structS: StructS):
  ITemplataType = {
    structS.maybePredictedType match {
      case Some(value) => return value
      case None =>
    }

    astrouts.codeLocationToMaybeType.get(structS.range.begin) match {
      case Some(Some(value)) => return value
      case _ =>
    }

    val struct = translateStruct(astrouts, env, structS)
    struct.tyype
  }

  def translateStruct(
    astrouts: Astrouts,
    env: Environment,
    structS: StructS):
  StructA = {
    val StructS(rangeS, nameS, attributesS, weakable, identifyingRunesS, runeToExplicitType, mutabilityRuneS, maybePredictedMutability, predictedRuneToType, maybePredictedType, rulesS, members) = structS

    astrouts.codeLocationToStruct.get(rangeS.begin) match {
      case Some(value) => return value
      case None =>
    }

    astrouts.codeLocationToMaybeType.get(rangeS.begin) match {
      // Weird because this means we already evaluated it, in which case we should have hit the above return
      case Some(Some(_)) => vwat()
      case Some(None) => {
        throw CompileErrorExceptionA(RangedInternalErrorA(rangeS, "Cycle in determining struct type!"))
      }
      case None =>
    }
    astrouts.codeLocationToMaybeType.put(rangeS.begin, None)

    val runeAToType =
      calculateRuneTypes(astrouts, rangeS, identifyingRunesS, runeToExplicitType, Vector(), rulesS, env)

    // Shouldnt fail because we got a complete solve earlier
    val tyype = Scout.determineDenizenType(identifyingRunesS, runeAToType).getOrDie()
    astrouts.codeLocationToMaybeType.put(rangeS.begin, Some(tyype))

    val structA =
      StructA(
        rangeS,
        nameS,
        attributesS,
        weakable,
        mutabilityRuneS,
        maybePredictedMutability,
        tyype,
        identifyingRunesS,
        runeAToType,
        rulesS.toVector,
        members)
    astrouts.codeLocationToStruct.put(rangeS.begin, structA)
    structA
  }

  def getInterfaceType(
    astrouts: Astrouts,
    env: Environment,
    interfaceS: InterfaceS):
  ITemplataType = {
    interfaceS.maybePredictedType match {
      case Some(value) => return value
      case None =>
    }

    astrouts.codeLocationToMaybeType.get(interfaceS.range.begin) match {
      case Some(Some(value)) => return value
      case _ =>
    }

    val struct = translateInterface(astrouts, env, interfaceS)
    struct.tyype
  }

  def translateInterface(astrouts: Astrouts,  env: Environment, interfaceS: InterfaceS): InterfaceA = {
    val InterfaceS(rangeS, nameS, attributesS, weakable, identifyingRunesS, runeToExplicitType, mutabilityRuneS, maybePredictedMutability, predictedRuneToType, maybePredictedType, rulesS, internalMethodsS) = interfaceS

    astrouts.codeLocationToInterface.get(rangeS.begin) match {
      case Some(value) => return value
      case None =>
    }

    astrouts.codeLocationToMaybeType.get(rangeS.begin) match {
      // Weird because this means we already evaluated it, in which case we should have hit the above return
      case Some(Some(_)) => vwat()
      case Some(None) => {
        throw CompileErrorExceptionA(RangedInternalErrorA(rangeS, "Cycle in determining interface type!"))
      }
      case None =>
    }
    astrouts.codeLocationToMaybeType.put(rangeS.begin, None)

    val runeAToType =
      calculateRuneTypes(astrouts, rangeS, identifyingRunesS, runeToExplicitType, Vector(), rulesS, env)

    // getOrDie because we should have gotten a complete solve
    val tyype = Scout.determineDenizenType(identifyingRunesS, runeAToType).getOrDie()
    astrouts.codeLocationToMaybeType.put(rangeS.begin, Some(tyype))

    val methodsEnv = env.addRunes(runeAToType)
    val internalMethodsA = internalMethodsS.map(translateFunction(astrouts, methodsEnv, _))

    val interfaceA =
      InterfaceA(
        rangeS,
        nameS,
        attributesS,
        weakable,
        mutabilityRuneS,
        maybePredictedMutability,
        tyype,
        //        knowableRunesS,
        identifyingRunesS,
        //        localRunesS,
        //        conclusions,
        runeAToType,
        rulesS.toVector,
        internalMethodsA)
    astrouts.codeLocationToInterface.put(rangeS.begin, interfaceA)
    interfaceA
  }

  def translateImpl(astrouts: Astrouts,  env: Environment, implS: ImplS): ImplA = {
    vimpl()
//    val ImplS(range, nameS, rulesFromStructDirection, rulesFromInterfaceDirection, knowableRunesS, localRunesS, isTemplate, structKindRuneS, interfaceKindRuneS) = implS
//    val nameA = translateImplName(nameS)
//    val localRunesS = localRunesS.map()
//    val knowableRunesS = knowableRunesS.map()
//
//    astrouts.getImpl(nameA) match {
//      case Some(existingImplA) => return existingImplA
//      case _ =>
//    }
//
//    val (conclusionsForRulesFromStructDirection, rulesFromStructDirectionA) = (vimpl(), vimpl())
////      makeRuleTyper().solve(astrouts, env, rulesFromStructDirection, range, Vector.empty, Some(knowableRunesS ++ localRunesS)) match {
////        case (_, rtsf @ RuleTyperSolveFailure(_, _, _, _)) => throw CompileErrorExceptionA(CouldntSolveRulesA(range, rtsf))
////        case (c, RuleTyperSolveSuccess(r)) => (c, r)
////      }
//    val (conclusionsForRulesFromInterfaceDirection, rulesFromInterfaceDirectionA) = (vimpl(), vimpl())
////      makeRuleTyper().solve(astrouts, env, rulesFromInterfaceDirection, range, Vector.empty, Some(knowableRunesS ++ localRunesS)) match {
////        case (_, rtsf @ RuleTyperSolveFailure(_, _, _, _)) => throw CompileErrorExceptionA(CouldntSolveRulesA(range, rtsf))
////        case (c, RuleTyperSolveSuccess(r)) => (c, r)
////      }
//    vimpl()//vassert(conclusionsForRulesFromStructDirection == conclusionsForRulesFromInterfaceDirection)
//    val conclusions = conclusionsForRulesFromStructDirection
//
//    ImplA(
//      range,
//      nameA,
//      rulesFromStructDirectionA,
//      rulesFromInterfaceDirectionA,
//      vimpl(),//conclusions,
//      localRunesS,
//      structKindRuneS),
//      interfaceKindRuneS))
  }

  def translateExport(astrouts: Astrouts,  env: Environment, exportS: ExportAsS): ExportAsA = {
    vimpl()
//    val ExportAsS(range, exportName, templexS, exportedName) = exportS
//
//    val runeS = ImplicitRuneS(exportName, 0)
//    val runeA = runeS)
//    val rulesS = Vector(EqualsSR(range, TypedSR(range, runeS, KindTypeSR), templexS))
//
//    val (runeToIndex, runeToType, rulesA) = RuleFlattener.flattenAndCompileRules(rulesS)
//
//    val conclusions =
//      makeRuleTyper().solve(astrouts, env, rulesA, range) match {
//        case Err(e) => throw CompileErrorExceptionA(e)
//        case Ok(x) => runeToIndex.mapValues(index => vassertSome(x(index)))
//      }
//
//    ExportAsA(range, exportedName, rulesA, conclusions, runeA)
  }
//
//  def translateParameter(env: Environment, paramS: ParameterS): ParameterA = {
//    val ParameterS(atomS) = paramS
//    ParameterA(translateAtom(astrouts, env, atomS))
//  }

//  def translateAtom(env: Environment, atomS: AtomSP): AtomAP = {
//    val AtomSP(range, maybeCaptureS, virtualityS, coordRuneS, destructureS) = atomS
//
//    val virtualityA =
//      virtualityS.map({
//        case AbstractSP => AbstractAP
//        case OverrideSP(range, kindRune) => OverrideAP(range, kindRune)
//      })
//
//    val destructureA = destructureS.map(_.map(translateAtom(astrouts, env, _)))
//
//    val maybeCaptureA =
//      maybeCaptureS match {
//        case None => None
//        case Some(CaptureS(nameS)) => {
//          val nameA = nameS
//          val local = env.locals.find(_.varName == nameA).get
//          Some(local)
//        }
//      }
//
//    AtomAP(range, maybeCaptureA, virtualityA, coordRuneS, destructureA)
//  }

  def translateFunction(astrouts: Astrouts, env: Environment, functionS: FunctionS): FunctionA = {
    val FunctionS(rangeS, nameS, attributesS, identifyingRunesS, runeToExplicitType, paramsS, maybeRetCoordRune, rulesS, bodyS) = functionS

    val runeAToType =
      calculateRuneTypes(astrouts, rangeS, identifyingRunesS, runeToExplicitType, paramsS, rulesS, env)

    // Shouldnt fail because we got a complete solve on the rules
    val tyype = Scout.determineDenizenType(identifyingRunesS, runeAToType).getOrDie()

    FunctionA(
      rangeS,
      nameS,
      attributesS ++ Vector(UserFunctionS),
      tyype,
      identifyingRunesS,
      runeAToType ++ env.typeByRune,
      paramsS,
      maybeRetCoordRune,
      rulesS.toVector,
      bodyS)
  }

  private def calculateRuneTypes(
    astrouts: Astrouts,
    rangeS: RangeS,
    identifyingRunesS: Vector[IRuneS],
    runeToExplicitType: Map[IRuneS, ITemplataType],
    paramsS: Vector[ParameterS],
    rulesS: Array[IRulexSR],
    env: Environment):
  Map[IRuneS, ITemplataType] = {
    val runeSToPreKnownTypeA =
      runeToExplicitType ++
        paramsS.map(_.pattern.coordRune -> CoordTemplataType).toMap ++
        // We manually figure out the types of runes from lookup rules beforehand, see AMPLR.
        rulesS.flatMap({
          case LookupSR(range, rune, name) => {
            val tyype =
              name match {
                case AbsoluteNameSN(name) => {
                  Astronomer.lookupType(astrouts, env, range, name)
                }
                case ImpreciseNameSN(name @ CodeTypeNameS(_)) => {
                  Astronomer.lookupType(astrouts, env, range, name)
                }
              }
            tyype match {
              case KindTemplataType => List()
              case _ => List(rune -> tyype)
            }
          }
          case _ => List()
        })
    val runeSToType =
      AstronomySolver.solve(env, false, rulesS, identifyingRunesS, true, runeSToPreKnownTypeA) match {
        case Ok(t) => t
        case Err(e) => throw CompileErrorExceptionA(CouldntSolveRulesA(rangeS, e))
      }

    val runeAToType =
      runeSToType.map({ case (runeS, tyype) =>
        (runeS) -> tyype
      })
    runeAToType
  }

  def translateProgram(
      codeMap: PackageCoordinateMap[ProgramS],
      primitives: Map[String, ITemplataType],
      suppliedFunctions: Vector[FunctionA],
      suppliedInterfaces: Vector[InterfaceA]):
  ProgramA = {
    val env = Environment(None, None, primitives, codeMap, Map())

    // If something is absence from the map, we haven't started evaluating it yet
    // If there is a None in the map, we started evaluating it
    // If there is a Some in the map, we know the type
    // If we are asked to evaluate something but there is already a None in the map, then we are
    // caught in a cycle.
    val astrouts =
      Astrouts(
        mutable.HashMap[CodeLocationS, Option[ITemplataType]](),
        mutable.HashMap[CodeLocationS, StructA](),
        mutable.HashMap[CodeLocationS, InterfaceA]())

    val structsA = env.structsS.map(translateStruct(astrouts, env, _))

    val interfacesA = env.interfacesS.map(translateInterface(astrouts, env, _))

    val implsA = env.implsS.map(translateImpl(astrouts, env, _))

    val functionsA = env.functionsS.map(translateFunction(astrouts, env, _))

    val exportsA = env.exportsS.map(translateExport(astrouts, env, _))

    ProgramA(structsA, suppliedInterfaces ++ interfacesA, implsA, suppliedFunctions ++ functionsA, exportsA)
  }


//  val stlFunctions =
//    Forwarders.forwarders ++
//    Vector(
//      NotEquals.function,
//      Printing.printInt,
//      Printing.printlnInt,
//      Printing.printBool,
//      Printing.printlnBool,
//      Printing.printlnStr)

//  val wrapperFunctions = Arrays.makeArrayFunctions()

  def runAstronomer(separateProgramsS: FileCoordinateMap[ProgramS]):
  Either[PackageCoordinateMap[ProgramA], ICompileErrorA] = {
    val mergedProgramS =
      PackageCoordinateMap(
        separateProgramsS.moduleToPackagesToFilenameToContents.mapValues(packagesToFilenameToContents => {
          packagesToFilenameToContents.mapValues(filenameToContents => {
            ProgramS(
              filenameToContents.values.flatMap(_.structs).toVector,
              filenameToContents.values.flatMap(_.interfaces).toVector,
              filenameToContents.values.flatMap(_.impls).toVector,
              filenameToContents.values.flatMap(_.implementedFunctions).toVector,
              filenameToContents.values.flatMap(_.exports).toVector,
              filenameToContents.values.flatMap(_.imports).toVector)
          })
        }))

//    val orderedModules = orderModules(mergedProgramS)

    try {
      val suppliedFunctions = Vector()
      val suppliedInterfaces = Vector()
      val ProgramA(structsA, interfacesA, implsA, functionsA, exportsA) =
        Astronomer.translateProgram(
          mergedProgramS, primitives, suppliedFunctions, suppliedInterfaces)

      val packageToStructsA = structsA.groupBy(_.name.codeLocation.file.packageCoordinate)
      val packageToInterfacesA = interfacesA.groupBy(_.name.codeLocation.file.packageCoordinate)
      val packageToFunctionsA = functionsA.groupBy(_.name.packageCoordinate)
      val packageToImplsA = implsA.groupBy(_.name.codeLocation.file.packageCoordinate)
      val packageToExportsA = exportsA.groupBy(_.range.file.packageCoordinate)

      val allPackages =
        packageToStructsA.keySet ++
        packageToInterfacesA.keySet ++
        packageToFunctionsA.keySet ++
        packageToImplsA.keySet ++
        packageToExportsA.keySet
      val packageToContents =
        allPackages.toVector.map(paackage => {
          val contents =
            ProgramA(
              packageToStructsA.getOrElse(paackage, Vector.empty),
              packageToInterfacesA.getOrElse(paackage, Vector.empty),
              packageToImplsA.getOrElse(paackage, Vector.empty),
              packageToFunctionsA.getOrElse(paackage, Vector.empty),
              packageToExportsA.getOrElse(paackage, Vector.empty))
          (paackage -> contents)
        }).toMap
      val moduleToPackageToContents =
        packageToContents.keys.toVector.groupBy(_.module).mapValues(packageCoordinates => {
          packageCoordinates.map(packageCoordinate => {
            (packageCoordinate.packages -> packageToContents(packageCoordinate))
          }).toMap
        })
      Left(PackageCoordinateMap(moduleToPackageToContents))
    } catch {
      case CompileErrorExceptionA(err) => {
        Right(err)
      }
    }
  }
}

class AstronomerCompilation(
  packagesToBuild: Vector[PackageCoordinate],
  packageToContentsResolver: IPackageResolver[Map[String, String]]) {
  var scoutCompilation = new ScoutCompilation(packagesToBuild, packageToContentsResolver)
  var astroutsCache: Option[PackageCoordinateMap[ProgramA]] = None

  def getCodeMap(): Result[FileCoordinateMap[String], FailedParse] = scoutCompilation.getCodeMap()
  def getParseds(): Result[FileCoordinateMap[(FileP, Vector[(Int, Int)])], FailedParse] = scoutCompilation.getParseds()
  def getVpstMap(): Result[FileCoordinateMap[String], FailedParse] = scoutCompilation.getVpstMap()
  def getScoutput(): Result[FileCoordinateMap[ProgramS], ICompileErrorS] = scoutCompilation.getScoutput()

  def getAstrouts(): Result[PackageCoordinateMap[ProgramA], ICompileErrorA] = {
    astroutsCache match {
      case Some(astrouts) => Ok(astrouts)
      case None => {
        Astronomer.runAstronomer(scoutCompilation.getScoutput().getOrDie()) match {
          case Right(err) => Err(err)
          case Left(astrouts) => {
            astroutsCache = Some(astrouts)
            Ok(astrouts)
          }
        }
      }
    }
  }
  def expectAstrouts(): PackageCoordinateMap[ProgramA] = {
    getAstrouts().getOrDie()
  }
}
