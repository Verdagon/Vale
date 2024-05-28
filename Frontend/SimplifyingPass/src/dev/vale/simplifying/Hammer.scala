package dev.vale.simplifying

import dev.vale.{Builtins, FileCoordinateMap, IPackageResolver, Interner, Keywords, PackageCoordinate, PackageCoordinateMap, Profiler, Result, finalast, vassert, vcurious, vfail, vimpl, vwat}
import dev.vale.finalast.{ConsecutorH, ConstantVoidH, CoordH, ExpressionH, Final, IdH, KindHT, Local, NeverHT, PackageH, ProgramH, PrototypeH, StackifyH, Variability, VariableIdH, VoidHT}
import dev.vale.highertyping.ICompileErrorA
import dev.vale.finalast._
import dev.vale.instantiating.ast.{IdI, _}
import dev.vale.postparsing.ICompileErrorS

import scala.collection.immutable.List

case class FunctionRefH(prototype: PrototypeH) {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();
  //  def functionType = prototype.functionType
  def fullName = prototype.id
}

case class LocalsBox(var inner: Locals) {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vfail() // Shouldnt hash, is mutable

  def snapshot = inner

  def typingPassLocals: Map[IVarNameI[cI], VariableIdH] = inner.typingPassLocals
  def unstackifiedVars: Set[VariableIdH] = inner.unstackifiedVars
  def locals: Map[VariableIdH, Local] = inner.locals
  def nextLocalIdNumber: Int = inner.nextLocalIdNumber

  def get(id: IVarNameI[cI]) = inner.get(id)
  def get(id: VariableIdH) = inner.get(id)

  def markUnstackified(varId2: IVarNameI[cI]): Unit = {
    inner = inner.markUnstackified(varId2)
  }
  def markRestackified(varId2: IVarNameI[cI]): Unit = {
    inner = inner.markRestackified(varId2)
  }

  def markUnstackified(varIdH: VariableIdH): Unit = {
    inner = inner.markUnstackified(varIdH)
  }
  def setNextLocalIdNumber(nextLocalIdNumber: Int): Unit = {
    inner = inner.copy(nextLocalIdNumber = nextLocalIdNumber)
  }

  def addHammerLocal(
    tyype: CoordH[KindHT],
    variability: Variability):
  Local = {
    val (newInner, local) = inner.addHammerLocal(tyype, variability)
    inner = newInner
    local
  }

  def addTypingPassLocal(
    varId2: IVarNameI[cI],
    varIdNameH: IdH,
    variability: Variability,
    tyype: CoordH[KindHT]):
  Local = {
    val (newInner, local) = inner.addCompilerLocal(varId2, varIdNameH, variability, tyype)
    inner = newInner
    local
  }

}

// This represents the locals for the entire function.
// Note, some locals will have the same index, that just means they're in
// different blocks.
case class Locals(
     // This doesn't have all the locals that are in the locals list, this just
     // has any locals added by typingpass.
     typingPassLocals: Map[IVarNameI[cI], VariableIdH],

     unstackifiedVars: Set[VariableIdH],

     // This has all the locals for the function, a superset of typingpassLocals.
     locals: Map[VariableIdH, Local],

     nextLocalIdNumber: Int) {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vcurious()

  def addCompilerLocal(
    varId2: IVarNameI[cI],
    varIdNameH: IdH,
    variability: Variability,
    tyype: CoordH[KindHT]):
  (Locals, Local) = {
    if (typingPassLocals.contains(varId2)) {
      vfail("There's already a typingpass local named: " + varId2)
    }
    val newLocalHeight = locals.size
    val newLocalIdNumber = nextLocalIdNumber
    val newLocalId = VariableIdH(newLocalIdNumber, newLocalHeight, Some(varIdNameH))
//    // Temporary until catalyst fills in stuff here
//    val keepAlive = newLocalId.name.map(_.readableName).getOrElse("").endsWith("__tether");
    val newLocal = Local(newLocalId, variability, tyype)
    val newLocals =
      Locals(
        typingPassLocals + (varId2 -> newLocalId),
        unstackifiedVars,
        locals + (newLocalId -> newLocal),
        newLocalIdNumber + 1)
    (newLocals, newLocal)
  }

  def addHammerLocal(
    tyype: CoordH[KindHT],
    variability: Variability):
  (Locals, Local) = {
    val newLocalHeight = locals.size
    val newLocalIdNumber = nextLocalIdNumber
    val newLocalId = VariableIdH(newLocalIdNumber, newLocalHeight, None)
    val newLocal = Local(newLocalId, variability, tyype)
    val newLocals =
      Locals(
        typingPassLocals,
        unstackifiedVars,
        locals + (newLocalId -> newLocal),
        newLocalIdNumber + 1)
    (newLocals, newLocal)
  }

  def markUnstackified(varId2: IVarNameI[cI]): Locals = {
    markUnstackified(typingPassLocals(varId2))
  }

  def markRestackified(varId2: IVarNameI[cI]): Locals = {
    markRestackified(typingPassLocals(varId2))
  }

  def markUnstackified(varIdH: VariableIdH): Locals = {
    // Make sure it existed and wasnt already unstackified
    vassert(locals.contains(varIdH))
    if (unstackifiedVars.contains(varIdH)) {
      vfail("Already unstackified " + varIdH)
    }
    Locals(typingPassLocals, unstackifiedVars + varIdH, locals, nextLocalIdNumber)
  }

  def markRestackified(varIdH: VariableIdH): Locals = {
    // Make sure it existed and was unstackified
    vassert(locals.contains(varIdH))
    if (!unstackifiedVars.contains(varIdH)) {
      vfail("Already unstackified " + varIdH)
    }
    Locals(typingPassLocals, unstackifiedVars - varIdH, locals, nextLocalIdNumber)
  }

  def get(varId: IVarNameI[cI]): Option[Local] = {
    typingPassLocals.get(varId) match {
      case None => None
      case Some(index) => Some(locals(index))
    }
  }

  def get(varId: VariableIdH): Option[Local] = {
    locals.get(varId)
  }
}

class Hammer(interner: Interner, keywords: Keywords) {
  val nameHammer: NameHammer = new NameHammer()
  val structHammer: StructHammer =
    new StructHammer(
      interner,
      keywords,
      nameHammer,
      (hinputs, hamuts, prototypeI) => typeHammer.translatePrototype(hinputs, hamuts, prototypeI),
      (hinputs, hamuts, referenceI) => typeHammer.translateCoord(hinputs, hamuts, referenceI))
  val typeHammer: TypeHammer = new TypeHammer(interner, keywords, nameHammer, structHammer)
  val functionHammer = new FunctionHammer(keywords, typeHammer, nameHammer, structHammer)
  val vonHammer = new VonHammer(nameHammer, typeHammer)

  def mangleFunc(id: IdI[cI, IFunctionNameI[cI]]): String = {
    val IdI(packageCoord, initSteps, localName) = id
    (localName match {
      case FunctionNameIX(FunctionTemplateNameI(humanName, _), templateArgs, _) => {
        (if (templateArgs.nonEmpty) {templateArgs.length + "_"} else {""}) +
        packageCoord.packages.map(_.str + "_").mkString("") +
        initSteps.map(mangleName).map(_ + "_").mkString("") +
        humanName.str +
        templateArgs.map("__" + mangleTemplata(_)).mkString("")
      }
      case ExternFunctionNameI(humanName, templateArgs, _) => {
        (if (templateArgs.nonEmpty) {templateArgs.length + "_"} else {""}) +
        packageCoord.packages.map(_.str + "_").mkString("") +
        initSteps.map(mangleName).map(_ + "_").mkString("") +
        humanName.str +
        templateArgs.map("__" + mangleTemplata(_)).mkString("")
      }
      case other => vimpl(other)
    })
  }

  def mangleName(name: INameI[cI]): String = {
    name match {
      case other => vimpl(other)
    }
  }

  def mangleStruct(id: IdI[cI, IStructNameI[cI]]): String = {
    val IdI(packageCoord, initSteps, localName) = id
    localName match {
      case StructNameI(StructTemplateNameI(humanName), templateArgs) => {
        (if (templateArgs.nonEmpty) {templateArgs.length + "_"} else {""}) +
        humanName.str +
        templateArgs.map("__" + mangleTemplata(_)).mkString("")
      }
//      case FunctionNameIX(FunctionTemplateNameI(humanName, _), templateArgs, _) => {
//        (if (templateArgs.nonEmpty) {templateArgs.length + "_"} else {""}) +
//        humanName.str +
//        templateArgs.map("__" + mangleTemplata(_)).mkString("")
//      }
//      case ExternFunctionNameI(humanName, templateArgs, _) => {
//        (if (templateArgs.nonEmpty) {templateArgs.length + "_"} else {""}) +
//        humanName.str +
//        templateArgs.map("__" + mangleTemplata(_)).mkString("")
//      }
      case other => vimpl(other)
    }
  }

  def mangleKind(kind: KindIT[cI]): String = {
    kind match {
      case IntIT(bits) => "i" + bits
      case other => vimpl(other)
    }
  }

  def mangleCoord(coord: CoordI[cI]): String = {
    val CoordI(ownership, kind) = coord
    (ownership match {
      case ImmutableShareI => "1_IRef_"
      case MutableShareI => ""
      case OwnI => ""
      case WeakI => "1_Weak_"
      case ImmutableBorrowI => "1_IRef_"
      case MutableBorrowI => "1_Ref_"
    }) + mangleKind(kind)
  }

  def mangleTemplata(templata: ITemplataI[cI]): String = {
    templata match {
      case CoordTemplataI(region, coord) => mangleCoord(coord)
      case other => vimpl(other)
    }
  }

  def simplifyId(id: IdI[cI, INameI[cI]]): SimpleId = {
    val IdI(packageCoord, initSteps, localName) = id
    val PackageCoordinate(module, packages) = packageCoord
    SimpleId(
      (SimpleIdStep(module.str, Vector()) +:
          packages.map(paackage => SimpleIdStep(paackage.str, Vector()))) ++
          initSteps.map(step => simplifyName(step)) :+
          simplifyName(localName))
  }

  def simplifyName(name: INameI[cI]): SimpleIdStep = {
    name match {
      case StructNameI(StructTemplateNameI(humanName), templateArgs) => {
        SimpleIdStep(humanName.str, templateArgs.map(simplifyTemplata))
      }
      case ExternFunctionNameI(humanName, templateArgs, parameters) => {
        SimpleIdStep(humanName.str, templateArgs.map(simplifyTemplata))
      }
      case other => vimpl(other)
    }
  }

  def simplifyTemplata(templata: ITemplataI[cI]): SimpleId = {
    templata match {
      case CoordTemplataI(region, coord) => simplifyCoord(coord)
      case other => vimpl(other)
    }
  }

  def simplifyKind(value: KindIT[cI]): SimpleId = {
    value match {
      case IntIT(bits) => SimpleId(Vector(SimpleIdStep("i" + bits, Vector())))
      case other => vimpl(other)
    }
  }

  def simplifyCoord(value: CoordI[cI]): SimpleId = {
    val CoordI(ownership, kind) = value
    val kindId = simplifyKind(kind)
    (ownership match {
      case ImmutableShareI => kindId
      case MutableShareI => kindId
      case OwnI => kindId
      case WeakI => vimpl()
      case ImmutableBorrowI => SimpleId(Vector(SimpleIdStep("&", Vector(kindId))))
      case MutableBorrowI => SimpleId(Vector(SimpleIdStep("&mut", Vector(kindId))))
    })
  }

  def translate(hinputs: HinputsI): ProgramH = {
    val HinputsI(
    interfaces,
    structs,
    functions,
//    kindToDestructor,
    interfaceToEdgeBlueprints,
    edges,
    kindExports,
    functionExports,
    kindExterns,
    functionExterns) = hinputs


    val hamuts = HamutsBox(Hamuts(Map(), Map(), Map(), Vector.empty, Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map()))
    //    val emptyPackStructRefH = structHammer.translateStructRef(hinputs, hamuts, emptyPackStructRef)
    //    vassert(emptyPackStructRefH == ProgramH.emptyTupleStructRef)

    kindExports.foreach({ case KindExportI(_, tyype, exportId, exportName) =>
      val kindH = typeHammer.translateKind(hinputs, hamuts, tyype)
      hamuts.addKindExport(kindH, exportId.packageCoord, exportName)
    })

    functionExports.foreach({ case FunctionExportI(_, prototype, exportId, exportName) =>
      val prototypeH = typeHammer.translatePrototype(hinputs, hamuts, prototype)
      hamuts.addFunctionExport(prototypeH, exportId.packageCoord, exportName)
    })

//    kindExterns.foreach({ case KindExternI(tyype, packageCoordinate, exportName) =>
//      val kindH = typeHammer.translateKind(hinputs, hamuts, tyype)
//      hamuts.addKindExtern(kindH, packageCoordinate, exportName)
//    })

    kindExterns.foreach({ case KindExternI(struct) =>
      val exportName = mangleStruct(struct.id)
      val exportSimplifiedId = simplifyId(struct.id)
      val structH = structHammer.translateStructI(hinputs, hamuts, struct)
      hamuts.addKindExtern(structH, exportSimplifiedId, exportName)
    })

    functionExterns.foreach({ case FunctionExternI(prototype) =>
      val exportName = mangleFunc(prototype.id)
      val exportSimplifiedId = simplifyId(prototype.id)
      val prototypeH = typeHammer.translatePrototype(hinputs, hamuts, prototype)
      hamuts.addFunctionExtern(prototypeH, exportSimplifiedId, exportName)
    })

    // We generate the names here first, so that externs get the first chance at having
    // ID 0 for each name, which means we dont need to add _1 _2 etc to the end of them,
    // and they'll match up with the actual outside names.
    //          externNameToExtern.map({ case (externName, prototype2) =>
    //            val fullNameH = NameHammer.translateFullName(hinputs, hamuts, prototype2.fullName)
    //            val humanName =
    //              prototype2.fullName.last match {
    //                case ExternFunctionName2(humanName, _) => humanName
    //                case _ => vfail("Only human-named functions can be extern")
    //              }
    //            if (fullNameH.readableName != humanName) {
    //              vfail("Name conflict, two externs with the same name!")
    //            }
    //            val prototypeH = typeHammer.translatePrototype(hinputs, hamuts, prototype2)
    //            (packageCoordinate -> (externName, prototypeH))
    //          })
    //      })
    //    val externPrototypesH =
    //      packageToExternNameToPrototypeH.values.flatMap(_.values.toVector)

    structHammer.translateInterfaces(hinputs, hamuts);
    structHammer.translateStructs(hinputs, hamuts)
    val userFunctions = functions.filter(f => f.header.isUserFunction).toVector
    val nonUserFunctions = functions.filter(f => !f.header.isUserFunction).toVector
    functionHammer.translateFunctions(hinputs, hamuts, userFunctions)
    functionHammer.translateFunctions(hinputs, hamuts, nonUserFunctions)

//    val immDestructorPrototypesH =
//      kindToDestructor.map({ case (kind, destructor) =>
//        val kindH = typeHammer.translateKind(hinputs, hamuts, kind)
//        val immDestructorPrototypeH = typeHammer.translatePrototype(hinputs, hamuts, destructor)
//        (kindH -> immDestructorPrototypeH)
//      }).toMap
//
//    immDestructorPrototypesH.foreach({ case (kindH, immDestructorPrototypeH) =>
//      vassert(immDestructorPrototypeH.params.head.kind == kindH)
//    })

    val packageToInterfaceDefs = hamuts.interfaceTToInterfaceDefH.groupBy(_._1.id.packageCoord)
    val packageToStructDefs = hamuts.structDefs.groupBy(_.id.packageCoordinate)
    val packageToFunctionDefs = hamuts.functionDefs.groupBy(_._1.id.packageCoord).mapValues(_.values.toVector)
    val packageToStaticSizedArrays = hamuts.staticSizedArrays.values.toVector.groupBy(_.name.packageCoordinate)
    val packageToRuntimeSizedArrays = hamuts.runtimeSizedArrays.values.toVector.groupBy(_.name.packageCoordinate)
//    val packageToImmDestructorPrototypes = immDestructorPrototypesH.groupBy(_._1.packageCoord(interner, keywords))
    val packageToExportNameToKind = hamuts.packageCoordToExportNameToKind
    val packageToExportNameToFunction = hamuts.packageCoordToExportNameToFunction
    val packageToExternNameToKind = hamuts.packageCoordToExternNameToKind
    val packageToExternNameToFunction = hamuts.packageCoordToExternNameToFunction

    val allPackageCoords =
      packageToInterfaceDefs.keySet ++
        packageToStructDefs.keySet ++
        packageToFunctionDefs.keySet ++
        packageToStaticSizedArrays.keySet ++
        packageToRuntimeSizedArrays.keySet ++
//        packageToImmDestructorPrototypes.keySet ++
        packageToExportNameToFunction.keySet ++
        packageToExportNameToKind.keySet ++
        packageToExternNameToFunction.keySet ++
        packageToExternNameToKind.keySet

    val packages = new PackageCoordinateMap[PackageH]()
    allPackageCoords.toVector.foreach(packageCoord => {
      packages.put(
        packageCoord,
        PackageH(
          packageToInterfaceDefs.getOrElse(packageCoord, Map()).values.toVector,
          packageToStructDefs.getOrElse(packageCoord, Vector.empty),
          packageToFunctionDefs.getOrElse(packageCoord, Vector.empty),
          packageToStaticSizedArrays.getOrElse(packageCoord, Vector.empty),
          packageToRuntimeSizedArrays.getOrElse(packageCoord, Vector.empty),
//          packageToImmDestructorPrototypes.getOrElse(packageCoord, Map()),
          packageToExportNameToFunction.getOrElse(packageCoord, Map()),
          packageToExportNameToKind.getOrElse(packageCoord, Map()),
          packageToExternNameToFunction.getOrElse(packageCoord, Map()),
          packageToExternNameToKind.getOrElse(packageCoord, Map())))
    })

    finalast.ProgramH(packages)
  }
}

object Hammer {

  private def flattenAndFilterVoids(unfilteredExprsHE: Vector[ExpressionH[KindHT]]): Vector[ExpressionH[KindHT]] = {
    val flattenedExprsHE =
      unfilteredExprsHE.flatMap({
        case ConsecutorH(innersHE) => innersHE
        case other => Vector(other)
      })
    flattenedExprsHE.init.foreach(exprHE => {
      exprHE.resultType.kind match {
        case NeverHT(_) => vwat()
        case _ =>
      }
    })

    // Filter out any Void that arent the last.
    val filteredFlattenedExprsHE =
      if (flattenedExprsHE.size <= 1) {
        flattenedExprsHE
      } else {
        flattenedExprsHE.init
          .filter({ case ConstantVoidH() => false case _ => true }) :+
          flattenedExprsHE.last
      }
    vassert(filteredFlattenedExprsHE.nonEmpty)
    filteredFlattenedExprsHE
  }

  def consecutive(unfilteredExprsHE: Vector[ExpressionH[KindHT]]): ExpressionH[KindHT] = {
    val filteredFlattenedExprsHE = flattenAndFilterVoids(unfilteredExprsHE)

    filteredFlattenedExprsHE match {
      case Vector() => vwat("Cant have empty consecutive")
      case Vector(only) => only
      case multiple => ConsecutorH(multiple)
    }
  }

  // Like consecutive() but for expressions that were meant to go somewhere
  // but then the last one crashes.
  // We store them into locals really just so ConsecutorH doesn't complain
  // about some pre-last statements not producing voids.
  // See BRCOBS.
  def consecrash(
    locals: LocalsBox,
    unfilteredExprsHE: Vector[ExpressionH[KindHT]]):
  ExpressionH[KindHT] = {
    unfilteredExprsHE.last.resultType.kind match {
      case NeverHT(_) =>
      case _ => vwat()
    }

    val exprsHE = flattenAndFilterVoids(unfilteredExprsHE)

    // Make temporaries for all the previous things if we end in a never
    val exprsWithStackifiedInitHE =
      exprsHE.init
        .map(expr => {
          if (expr.resultType.kind == VoidHT()) {
            // Dont need a temporary if it's void, we can just drop it.
            expr
          } else {
            val local = locals.addHammerLocal(expr.resultType, Final)
            StackifyH(expr, local, None)
          }
        }) :+
        exprsHE.last
    // We'll never need to unstackify them because we're about to crash.

    exprsWithStackifiedInitHE match {
      case Vector() => vwat("Cant have empty consecutive")
      case Vector(only) => return only
      case multiple => ConsecutorH(multiple)
    }
  }
}

