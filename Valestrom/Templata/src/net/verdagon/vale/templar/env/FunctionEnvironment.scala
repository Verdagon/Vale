package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{BlockSE, INameS, LocalS}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata.{ITemplata}
import net.verdagon.vale.templar.types.{CoordT, StructTT, VariabilityT}
import net.verdagon.vale.{IProfiler, vassert, vcurious, vfail, vimpl, vwat}

import scala.collection.immutable.{List, Map, Set}

case class BuildingFunctionEnvironmentWithClosureds(
  parentEnv: IEnvironment,
  fullName: FullNameT[BuildingFunctionNameWithClosuredsT],
  function: FunctionA,
  variables: Vector[IVariableT],
  templatas: TemplatasStore
) extends IEnvironment {

  val hash = runtime.ScalaRunTime._hashCode(fullName); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IEnvironment]) {
      return false
    }
    return fullName.equals(obj.asInstanceOf[IEnvironment].fullName)
  }

  override def getParentEnv(): Option[IEnvironment] = Some(parentEnv)
  override def globalEnv: PackageEnvironment[INameT] = parentEnv.globalEnv

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

}

case class BuildingFunctionEnvironmentWithClosuredsAndTemplateArgs(
  parentEnv: IEnvironment,
  fullName: FullNameT[BuildingFunctionNameWithClosuredsAndTemplateArgsT],
  function: FunctionA,
  variables: Vector[IVariableT],
  templatas: TemplatasStore
) extends IEnvironment {

  val hash = runtime.ScalaRunTime._hashCode(fullName); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IEnvironment]) {
      return false
    }
    return fullName.equals(obj.asInstanceOf[IEnvironment].fullName)
  }

  override def getParentEnv(): Option[IEnvironment] = Some(parentEnv)
  override def globalEnv: PackageEnvironment[INameT] = parentEnv.globalEnv
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
}

case class FunctionEnvironment(
  // These things are the "environment"; they are the same for every line in a function.
  parentEnv: IEnvironment,
  fullName: FullNameT[IFunctionNameT], // Includes the name of the function
  function: FunctionA,
  templatas: TemplatasStore,
  maybeReturnType: Option[CoordT],

  // This is stuff that's not state, but could be different line-to-line.
  // The containing Block, useful for looking up LocalS for any locals we're making.
  containingBlockS: Option[BlockSE],

  // The things below are the "state"; they can be different for any given line in a function.
  liveLocals: Vector[IVariableT],
  // This can refer to vars in parent environments, see UCRTVPE.
  unstackifieds: Set[FullNameT[IVarNameT]]

  // We just happen to combine these two things into one FunctionEnvironment.
  // It might even prove useful one day... since the StructDef for a lambda remembers
  // its original environment, a closure can know all the variable IDs and moveds for
  // its containing function at that time.
  // See AENS for some more thoughts on environment vs state.

) extends IEnvironment {

  val hash = runtime.ScalaRunTime._hashCode(fullName); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IEnvironment]) {
      return false
    }
    return fullName.equals(obj.asInstanceOf[IEnvironment].fullName)
  }

  vassert(fullName.steps.startsWith(parentEnv.fullName.steps))

  vassert(liveLocals == liveLocals.distinct)

  override def getParentEnv(): Option[IEnvironment] = Some(parentEnv)
  override def globalEnv: PackageEnvironment[INameT] = parentEnv.globalEnv

  def addVariables(newVars: Vector[IVariableT]): FunctionEnvironment = {
    FunctionEnvironment(parentEnv, fullName, function, templatas, maybeReturnType, containingBlockS, liveLocals ++ newVars, unstackifieds)
  }
  def addVariable(newVar: IVariableT): FunctionEnvironment = {
    FunctionEnvironment(parentEnv, fullName, function, templatas, maybeReturnType, containingBlockS, liveLocals :+ newVar, unstackifieds)
  }
  def markLocalUnstackified(newUnstackified: FullNameT[IVarNameT]): FunctionEnvironment = {
    vassert(!getAllUnstackifiedLocals(true).contains(newUnstackified))
    vassert(getAllLocals(true).exists(_.id == newUnstackified))
    // Even if the local belongs to a parent env, we still mark it unstackified here, see UCRTVPE.
    FunctionEnvironment(parentEnv, fullName, function, templatas, maybeReturnType, containingBlockS, liveLocals, unstackifieds + newUnstackified)
  }

  def addEntry(useOptimization: Boolean, name: INameT, entry: IEnvEntry): FunctionEnvironment = {
    FunctionEnvironment(
      parentEnv,
      fullName,
      function,
      templatas.addEntry(useOptimization, name, entry),
      maybeReturnType,
      containingBlockS,
      liveLocals,
      unstackifieds)
  }
  def addEntries(useOptimization: Boolean, newEntries: Map[INameT, Vector[IEnvEntry]]): FunctionEnvironment = {
    FunctionEnvironment(
      parentEnv,
      fullName,
      function,
      templatas.addEntries(useOptimization, newEntries),
      maybeReturnType,
      containingBlockS,
      liveLocals,
      unstackifieds)
  }

  override def lookupWithName(
    profiler: IProfiler,
    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    templatas.lookupWithName(profiler, this, name, lookupFilter, false)
  }
  override def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    templatas.lookupWithImpreciseName(profiler, this, nameS, lookupFilter, getOnlyNearest)
  }

  def getVariable(name: IVarNameT): Option[IVariableT] = {
    liveLocals.find(_.id.last == name) match {
      case Some(v) => Some(v)
      case None => {
        parentEnv match {
          case pfe @ FunctionEnvironment(_, _, _, _, _, _, _, _) => pfe.getVariable(name)
          case _ => None
        }
      }
    }
  }

  // Dont have a getAllUnstackifiedLocals or getAllLiveLocals here. We learned that the hard way.
  // See UCRTVPE, child environments would be the ones that know about their unstackifying of locals
  // from parent envs.

  def getAllLocals(includeAncestorEnvs: Boolean): Vector[ILocalVariableT] = {
    val parentLiveLocals =
      if (includeAncestorEnvs) {
        parentEnv match {
          case parentFuncEnv@FunctionEnvironment(_, _, _, _, _, _, _, _) => parentFuncEnv.getAllLocals(includeAncestorEnvs)
          case _ => Vector.empty
        }
      } else {
        Vector.empty
      }
    val hereLiveLocals = liveLocals.collect({ case i : ILocalVariableT => i })
    parentLiveLocals ++ hereLiveLocals
  }

  def getAllUnstackifiedLocals(includeAncestorEnvs: Boolean): Vector[FullNameT[IVarNameT]] = {
    val parentUnstackifiedLocals =
      if (includeAncestorEnvs) {
        parentEnv match {
          case parentFuncEnv@FunctionEnvironment(_, _, _, _, _, _, _, _) => parentFuncEnv.getAllUnstackifiedLocals(includeAncestorEnvs)
          case _ => Vector.empty
        }
      } else {
        Vector.empty
      }
    parentUnstackifiedLocals ++ unstackifieds
  }

  def makeChildEnvironment(newTemplataStore: () => TemplatasStore, newContainingBlockS: Option[BlockSE]) = {
    FunctionEnvironment(
      this,
      fullName,
      function,
      newTemplataStore(),
      maybeReturnType,
      newContainingBlockS,
      Vector.empty,
      Set())
  }

  // No particular reason we don't have an addFunction like PackageEnvironment does
}

case class FunctionEnvironmentBox(var functionEnvironment: FunctionEnvironment) extends IEnvironmentBox {
  override def hashCode(): Int = vfail() // Shouldnt hash, is mutable

  override def snapshot: FunctionEnvironment = functionEnvironment
  def parentEnv: IEnvironment = functionEnvironment.parentEnv
  def fullName: FullNameT[IFunctionNameT] = functionEnvironment.fullName
  def function: FunctionA = functionEnvironment.function
  def templatas: TemplatasStore = functionEnvironment.templatas
  def maybeReturnType: Option[CoordT] = functionEnvironment.maybeReturnType
  def containingBlockS: Option[BlockSE] = functionEnvironment.containingBlockS
  def liveLocals: Vector[IVariableT] = functionEnvironment.liveLocals
  def unstackifieds: Set[FullNameT[IVarNameT]] = functionEnvironment.unstackifieds
  override def globalEnv: PackageEnvironment[INameT] = parentEnv.globalEnv

  def setReturnType(returnType: Option[CoordT]): Unit = {
    functionEnvironment = functionEnvironment.copy(maybeReturnType = returnType)
  }

  def addVariable(newVar: IVariableT): Unit= {
    functionEnvironment = functionEnvironment.addVariable(newVar)
  }
  def markLocalUnstackified(newMoved: FullNameT[IVarNameT]): Unit= {
    functionEnvironment = functionEnvironment.markLocalUnstackified(newMoved)
  }

  def addEntry(useOptimization: Boolean, name: INameT, entry: IEnvEntry): Unit = {
    functionEnvironment = functionEnvironment.addEntry(useOptimization, name, entry)
  }
  def addEntries(useOptimization: Boolean, newEntries: Map[INameT, Vector[IEnvEntry]]): Unit= {
    functionEnvironment = functionEnvironment.addEntries(useOptimization, newEntries)
  }

  override def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    functionEnvironment.lookupWithImpreciseName(profiler, nameS, lookupFilter, getOnlyNearest)
  }

  override def lookupWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    functionEnvironment.lookupWithName(profiler, nameS, lookupFilter, getOnlyNearest)
  }

  override def lookupAllWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext]):
  Iterable[ITemplata] = {
    functionEnvironment.lookupAllWithImpreciseName(profiler, nameS, lookupFilter)
  }

  override def lookupAllWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Iterable[ITemplata] = {
    functionEnvironment.lookupAllWithName(profiler, nameS, lookupFilter)
  }

  override def lookupNearestWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata] = {
    functionEnvironment.lookupNearestWithImpreciseName(profiler, nameS, lookupFilter)
  }

  override def lookupNearestWithName(
    profiler: IProfiler,
    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata] = {
    functionEnvironment.lookupNearestWithName(profiler, nameS, lookupFilter)
  }

  def getVariable(name: IVarNameT): Option[IVariableT] = {
    functionEnvironment.getVariable(name)
  }

  def getAllLocals(includeAncestorEnvs: Boolean): Vector[ILocalVariableT] = {
    functionEnvironment.getAllLocals(includeAncestorEnvs)
  }

  def getAllUnstackifiedLocals(includeAncestorEnvs: Boolean): Vector[FullNameT[IVarNameT]] = {
    functionEnvironment.getAllUnstackifiedLocals(includeAncestorEnvs)
  }

  // Gets the effects that this environment had on the outside world (on its parent
  // environments).
  def getEffects(): Set[FullNameT[IVarNameT]] = {
    // We may have unstackified outside locals from inside the block, make sure
    // the parent environment knows about that.
    val unstackifiedAncestorLocals = unstackifieds -- liveLocals.map(_.id)
    unstackifiedAncestorLocals
  }

  def makeChildEnvironment(newTemplataStore: () => TemplatasStore, containingBlockS: Option[BlockSE]):
  FunctionEnvironmentBox = {
    FunctionEnvironmentBox(
      functionEnvironment
        .makeChildEnvironment(newTemplataStore, containingBlockS))
  }

  // No particular reason we don't have an addFunction like PackageEnvironment does
}

sealed trait IVariableT  {
  def id: FullNameT[IVarNameT]
  def variability: VariabilityT
  def reference: CoordT
}
sealed trait ILocalVariableT extends IVariableT {
  def reference: CoordT
  def id: FullNameT[IVarNameT]
}
// Why the difference between reference and addressible:
// If we mutate/move a variable from inside a closure, we need to put
// the local's address into the struct. But, if the closures don't
// mutate/move, then we could just put a regular reference in the struct.
// Lucky for us, the parser figured out if any of our child closures did
// any mutates/moves/borrows.
case class AddressibleLocalVariableT(
  id: FullNameT[IVarNameT],
  variability: VariabilityT,
  reference: CoordT
) extends ILocalVariableT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class ReferenceLocalVariableT(
  id: FullNameT[IVarNameT],
  variability: VariabilityT,
  reference: CoordT
) extends ILocalVariableT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
case class AddressibleClosureVariableT(
  id: FullNameT[IVarNameT],
  closuredVarsStructType: StructTT,
  variability: VariabilityT,
  reference: CoordT
) extends IVariableT {

}
case class ReferenceClosureVariableT(
  id: FullNameT[IVarNameT],
  closuredVarsStructType: StructTT,
  variability: VariabilityT,
  reference: CoordT
) extends IVariableT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash;

}
