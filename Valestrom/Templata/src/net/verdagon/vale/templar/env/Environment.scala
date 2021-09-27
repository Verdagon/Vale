package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.{IProfiler, PackageCoordinate, vassert, vcurious, vfail, vimpl, vwat}

import scala.collection.immutable.{List, Map}


trait IEnvironment {
  override def toString: String = {
    "#Environment"
  }
  def getParentEnv(): Option[IEnvironment]
  def globalEnv: PackageEnvironment[INameT]

  def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata]

  def lookupWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata]

  def lookupAllWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext]):
  Iterable[ITemplata] = {
    lookupWithImpreciseName(profiler, nameS, lookupFilter, false)
  }

  def lookupAllWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Iterable[ITemplata] = {
    lookupWithName(profiler, nameS, lookupFilter, false)
  }

  def lookupNearestWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata] = {
    lookupWithImpreciseName(profiler, nameS, lookupFilter, true).toList match {
      case List() => None
      case List(only) => Some(only)
      case _ => vfail("Too many with name: " + nameS)
    }
  }

  def lookupNearestWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata] = {
    lookupWithName(profiler, nameS, lookupFilter, true).toList match {
      case List() => None
      case List(only) => Some(only)
      case multiple => vfail("Too many with name " + nameS + ": " + multiple)
    }
  }

  def fullName: FullNameT[INameT]
}

trait IEnvironmentBox {
  def snapshot: IEnvironment
  override def toString: String = {
    "#Environment"
  }
  def globalEnv: PackageEnvironment[INameT]

  def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata]

  def lookupWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata]

  def lookupAllWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext]):
  Iterable[ITemplata]

  def lookupAllWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Iterable[ITemplata]

  def lookupNearestWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata]

  def lookupNearestWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata]


  def fullName: FullNameT[INameT]
}

sealed trait ILookupContext
case object TemplataLookupContext extends ILookupContext
case object ExpressionLookupContext extends ILookupContext

case class PackageEnvironment[+T <: INameT](
  maybeParentEnv: Option[IEnvironment],
  fullName: FullNameT[T],
  templatas: TemplatasStore
) extends IEnvironment {
  val hash = runtime.ScalaRunTime._hashCode(fullName); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IEnvironment]) {
      return false
    }
    return fullName.equals(obj.asInstanceOf[IEnvironment].fullName)
  }


  maybeParentEnv match {
    case None =>
    case Some(parentEnv) => vassert(fullName.steps.startsWith(parentEnv.fullName.steps))
  }

  override def globalEnv: PackageEnvironment[INameT] = {
    maybeParentEnv match {
      case None => this
      case Some(parentEnv) => parentEnv.globalEnv
    }
  }

  override def lookupWithName(
    profiler: IProfiler,
    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    templatas.lookupWithName(profiler, this, name, lookupFilter, getOnlyNearest)
  }

  override def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    templatas.lookupWithImpreciseName(profiler, this, nameS, lookupFilter, getOnlyNearest)
  }

  def addUnevaluatedFunction(
    useOptimization: Boolean,
    function: FunctionA
  ): PackageEnvironment[T] = {
    PackageEnvironment(
      maybeParentEnv,
      fullName,
      templatas.addUnevaluatedFunction(useOptimization, function))
  }

  def addEntry(useOptimization: Boolean, name: INameT, entry: IEnvEntry): PackageEnvironment[T] = {
    PackageEnvironment(
      maybeParentEnv,
      fullName,
      templatas.addEntry(useOptimization, name, entry))
  }

  def addEntries(useOptimization: Boolean, newEntries: Map[INameT, Vector[IEnvEntry]]): PackageEnvironment[T] = {
    PackageEnvironment(
      maybeParentEnv,
      fullName,
      templatas.addEntries(useOptimization: Boolean, newEntries))
  }

  override def getParentEnv(): Option[IEnvironment] = maybeParentEnv
}

case class TemplatasStore(
  // This is the source of truth. Anything in the environment is in here.
  entriesByNameT: Map[INameT, Vector[IEnvEntry]],
  // This is just an index for quick looking up of things by their imprecise name.
  // Not everything in the above entriesByNameT will have something in here.
  entriesByImpreciseNameS: Map[INameS, Vector[IEnvEntry]]
) {
  override def hashCode(): Int = vcurious()

  //  // The above map, indexed by human name. If it has no human name, it won't be in here.
  //  private var entriesByHumanName = Map[String, Vector[IEnvEntry]]()

  def entryToTemplata(env: IEnvironment, entry: IEnvEntry): ITemplata = {
//    vassert(env.fullName != FullName2(PackageCoordinate.BUILTIN, Vector.empty, PackageTopLevelName2()))
    entry match {
      case FunctionEnvEntry(func) => FunctionTemplata.make(env, func)
      case StructEnvEntry(struct) => StructTemplata.make(env, struct)
      case InterfaceEnvEntry(interface) => InterfaceTemplata.make(env, interface)
      case ImplEnvEntry(impl) => ImplTemplata.make(env, impl)
      case TemplataEnvEntry(templata) => templata
    }
  }

  def addEntries(useOptimization: Boolean, newEntries: Map[INameT, Vector[IEnvEntry]]): TemplatasStore = {
    val oldEntries = entriesByNameT

    val combinedEntries =
      oldEntries ++
        newEntries ++
        oldEntries.keySet.intersect(newEntries.keySet)
          .map(key => (key -> (oldEntries(key) ++ newEntries(key))))
          .toMap

    newEntries.keys.foreach(newEntryName => {
      val entriesWithThisName = combinedEntries(newEntryName)
      val (unflattenedNumTemplatas, unflattenedNumNonTemplatas) =
        entriesWithThisName
          .map({
            case tee @ TemplataEnvEntry(_) => (1, 0)
            case other => (0, 1)
          })
          .unzip
      val numTemplatas = unflattenedNumTemplatas.sum
      val numNonTemplatas = unflattenedNumNonTemplatas.sum
      // Itd be weird to have two templatas directly in this env, there would be
      // no way to distinguish them.
      vassert(numTemplatas <= 1)
      // We dont want both a templata and a non templata directly in this env,
      // the templata would always take precedence.
      vassert(numTemplatas == 0 || numNonTemplatas == 0)
    })

    val newEntriesByNameS =
      newEntries
        .toVector
        .map({ case (key, value) => (getImpreciseName(key), value) })
        .filter(_._1.nonEmpty)
        .map({ case (key, value) => (key.get, value) })
        .toMap
    vassert(newEntriesByNameS.size <= newEntries.size)
    val combinedEntriesByNameS =
      entriesByImpreciseNameS ++
        newEntriesByNameS ++
        entriesByImpreciseNameS.keySet.intersect(newEntriesByNameS.keySet)
          .map(key => (key -> (entriesByImpreciseNameS(key) ++ newEntriesByNameS(key))))
          .toMap

//    val newImplEntriesByStringName =
//      newEntries
//        .mapValues({ _.collect({ case i @ ImplEnvEntry(_) => i }) })
//        .values
//        .flatten
//        .toVector
//        .groupBy(_.impl.name.subCitizenHumanName)
//        .toMap
//    vassert(newImplEntriesByStringName.size <= newEntries.size)
//    val combinedImplEntriesByStringName =
//      implEntriesBySubCitizenName ++
//        newImplEntriesByStringName ++
//        implEntriesBySubCitizenName.keySet.intersect(newImplEntriesByStringName.keySet)
//          .map(key => (key -> (implEntriesBySubCitizenName(key) ++ newImplEntriesByStringName(key))))
//          .toMap

    TemplatasStore(combinedEntries, combinedEntriesByNameS)
  }

  def addUnevaluatedFunction(useOptimization: Boolean, functionA: FunctionA): TemplatasStore = {
    val functionName = NameTranslator.translateFunctionNameToTemplateName(functionA.name)
    addEntry(useOptimization, functionName, FunctionEnvEntry(functionA))
  }


  def entryMatchesFilter(entry: IEnvEntry, contexts: Set[ILookupContext]): Boolean = {
    entry match {
      case FunctionEnvEntry(_) => contexts.contains(ExpressionLookupContext)
      case ImplEnvEntry(_) => contexts.contains(ExpressionLookupContext)
      case StructEnvEntry(_) => contexts.contains(TemplataLookupContext)
      case InterfaceEnvEntry(_) => contexts.contains(TemplataLookupContext)
      case TemplataEnvEntry(templata) => {
        templata match {
          case PrototypeTemplata(_) => true
          case CoordTemplata(_) => contexts.contains(TemplataLookupContext)
          case CoordListTemplata(_) => contexts.contains(TemplataLookupContext)
          case KindTemplata(_) => contexts.contains(TemplataLookupContext)
          case StructTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case InterfaceTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case RuntimeSizedArrayTemplateTemplata() => contexts.contains(TemplataLookupContext)
          case BooleanTemplata(_) => true
          case FunctionTemplata(_, _) => contexts.contains(ExpressionLookupContext)
          case ImplTemplata(_, _) => contexts.contains(ExpressionLookupContext)
          case IntegerTemplata(_) => true
          case StringTemplata(_) => true
          case LocationTemplata(_) => contexts.contains(TemplataLookupContext)
          case MutabilityTemplata(_) => contexts.contains(TemplataLookupContext)
          case OwnershipTemplata(_) => contexts.contains(TemplataLookupContext)
          case PermissionTemplata(_) => contexts.contains(TemplataLookupContext)
          case VariabilityTemplata(_) => contexts.contains(TemplataLookupContext)
          case ExternImplTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case ExternFunctionTemplata(_) => contexts.contains(ExpressionLookupContext)
        }
      }
    }
  }

//  def impreciseNamesMatch(nameA: INameS, name2: INameT): Boolean = {
//    // If something's in these two switch statements, then we've factored them into the main one below.
//    // When you add something to the main list, make sure you handle all its cases and add it to one of
//    // these too.
//    nameA match {
//      case CodeTypeNameS(_) =>
//      case GlobalFunctionFamilyNameS(_) =>
//      case ImplImpreciseNameS(_) =>
////      case ImmConcreteDestructorStringNameS() =>
////      case ImmInterfaceDestructorStringNameS() =>
////      case ImmDropStringNameS() =>
//      case _ => vimpl()
//    }
//    name2 match {
//      case CitizenTemplateNameT(_, _) =>
//      case FunctionTemplateNameT(_, _) =>
//      case PrimitiveNameT(_) =>
////      case ReturnRuneS() =>
////      case ImplicitRuneS(_, _) =>
////      case CodeRuneS(_) =>
//      case LambdaCitizenNameT(_) =>
//      case ClosureParamNameT() =>
//      case FunctionNameT(_, _, _) =>
////      case AnonymousSubstructParentInterfaceRuneS() =>
//      case AnonymousSubstructImplNameT() =>
////      case SolverKindRuneS(_) =>
//      case ImplDeclareNameT(_, _) =>
////      case LetImplicitRuneS(_, _) =>
////      case MemberRuneS(_) =>
//      case CitizenNameT(_, _) =>
////      case MagicImplicitRuneS(_) =>
//      case ImmConcreteDestructorTemplateNameT() =>
//      case ImmInterfaceDestructorTemplateNameT() =>
//      case ImmDropTemplateNameT() =>
//      case _ => vimpl()
//    }
//    (nameA, name2) match {
//      case (CodeTypeNameS(humanNameS), CitizenTemplateNameT(humanNameT, _)) => humanNameS == humanNameT
//      case (CodeTypeNameS(humanNameS), CitizenTemplateNameT(humanNameT, _)) => humanNameS == humanNameT
//      case (CodeTypeNameS(humanNameS), FunctionTemplateNameT(humanNameT, _)) => humanNameS == humanNameT
//      case (CodeTypeNameS(humanNameS), PrimitiveNameT(humanNameT)) => humanNameS == humanNameT
//      case (CodeTypeNameS(humanNameS), CitizenNameT(humanNameT, _)) => humanNameS == humanNameT
//      case (GlobalFunctionFamilyNameS(humanNameS), FunctionTemplateNameT(humanNameT, _)) => humanNameS == humanNameT
//      case (GlobalFunctionFamilyNameS(humanNameS), FunctionNameT(humanNameT, _, _)) => humanNameS == humanNameT
//      case (ImplImpreciseNameS(subCitizenHumanNameS), ImplDeclareNameT(subCitizenHumanNameT, _)) => subCitizenHumanNameS == subCitizenHumanNameT
////      case (ImmDropStringNameS(), ImmDropTemplateNameT()) => true
////      case (ImmConcreteDestructorStringNameS(), ImmConcreteDestructorTemplateNameT()) => true
////      case (ImmInterfaceDestructorStringNameS(), ImmInterfaceDestructorTemplateNameT()) => true
//      //      case (ImplStringNameS(), AnonymousSubstructImplName2()) => true // not really needed if we use ImplDeclareName?
//      case _ => false
//    }
//  }

  def getImpreciseName(name2: INameT): Option[INameS] = {
    name2 match {
      case CitizenTemplateNameT(humanName, _) => Some(CodeTypeNameS(humanName))
      case CitizenTemplateNameT(humanName, _) => Some(CodeTypeNameS(humanName))
      case PrimitiveNameT(humanName) => Some(CodeTypeNameS(humanName))
      case CitizenNameT(humanName, _) => Some(CodeTypeNameS(humanName))
      case FunctionTemplateNameT(humanName, _) => Some(GlobalFunctionFamilyNameS(humanName))
      case FunctionNameT(humanName, _, _) => Some(GlobalFunctionFamilyNameS(humanName))
      case RuneNameT(r) => Some(RuneNameS(r))

      case ImplDeclareNameT(subCitizenHumanName, _) => Some(ImplImpreciseNameS(subCitizenHumanName))
      case ImmDropTemplateNameT() => Some(ImmDropImpreciseNameS())
      case ImmConcreteDestructorTemplateNameT() => Some(ImmConcreteDestructorImpreciseNameS())
      case ImmInterfaceDestructorTemplateNameT() => Some(ImmInterfaceDestructorImpreciseNameS())
//      case RuneNameT(ImplicitRuneS(_)) => None
//      case RuneNameT(LetImplicitRuneS(_)) => None
//      case RuneNameT(SolverKindRuneS(_)) => None
//      case RuneNameT(ReturnRuneS()) => None
//      case RuneNameT(MemberRuneS(_)) => None
      case LambdaCitizenNameT(codeLoc) => Some(LambdaStructNameS(LambdaNameS(codeLoc)))
      case ClosureParamNameT() => None
//      case AnonymousSubstructParentInterfaceRuneS() => None
      case AnonymousSubstructImplNameT() => None
//      case MagicImplicitRuneS(_) => None
      case other => vimpl(other.toString)
    }
  }

  //  def runesMatch(runeA: IRuneS, rune2: IRuneT): Boolean = {
  //    (runeA, rune2) match {
  //      case (CodeRuneA(nameA), CodeRune2(name2)) => nameA == name2
  //      case (ImplicitRuneA(nameA), ImplicitRune2(name2)) => nameA == name2
  //      case (MemberRuneA(memberIndexA), MemberRune2(memberIndex2)) => memberIndexA == memberIndex2
  //      case (MagicImplicitRuneA(magicParamIndexA), MagicImplicitRune2(magicParamIndex2)) => magicParamIndexA == magicParamIndex2
  //      case (ReturnRuneA(), ReturnRune2()) => true
  //    }
  //  }

  def codeLocationsMatch(codeLocationA: CodeLocationS, codeLocation2: CodeLocationS): Boolean = {
    val CodeLocationS(lineS, charS) = codeLocationA
    val CodeLocationS(line2, char2) = codeLocation2
    lineS == line2 && charS == char2
  }

  def lookupWithName(
    profiler: IProfiler,
    from: IEnvironment,
    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    profiler.childFrame("lookupWithName", () => {
      val allFromThisEnv =
        entriesByNameT
          .get(name)
          .toVector
          .flatten
          .filter(entryMatchesFilter(_, lookupFilter))
          .map(entryToTemplata(from, _))
      if (getOnlyNearest) {
        allFromThisEnv match {
          case Vector(entry) => Some(entry)
          case Vector() => from.getParentEnv().toList.flatMap(_.lookupWithName(profiler, name, lookupFilter, getOnlyNearest))
          case multiple => vfail("Too many things named " + name + ":" + multiple);
        }
      } else {
        allFromThisEnv ++
          from.getParentEnv().toList.flatMap(_.lookupWithName(profiler, name, lookupFilter, getOnlyNearest))
      }
    })
  }

  def lookupWithImpreciseName(
    profiler: IProfiler,
    from: IEnvironment,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    profiler.childFrame("lookupWithImpreciseName", () => {
      val allEntriesFromThisEnv = entriesByImpreciseNameS.get(nameS).toVector.flatten
      val allTemplatasFromThisEnv =
        allEntriesFromThisEnv
          .filter(entryMatchesFilter(_, lookupFilter))
          .map(entryToTemplata(from, _))
      if (getOnlyNearest) {
        allTemplatasFromThisEnv match {
          case Vector(entry) => Some(entry)
          case Vector() => from.getParentEnv().toList.flatMap(_.lookupWithImpreciseName(profiler, nameS, lookupFilter, getOnlyNearest))
          case multiple => vfail("Too many things named " + nameS + ":" + multiple);
        }
      } else {
        allTemplatasFromThisEnv ++
          from.getParentEnv().toList.flatMap(_.lookupWithImpreciseName(profiler, nameS, lookupFilter, getOnlyNearest))
      }
    })
  }

  def addEntry(useOptimization: Boolean, name: INameT, entry: IEnvEntry): TemplatasStore = {
    addEntries(useOptimization, Map(name -> Vector(entry)))
  }
}
