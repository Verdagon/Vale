package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{BlockSE, INameS, LocalS}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.names.{BuildingFunctionNameWithClosuredsAndTemplateArgsT, BuildingFunctionNameWithClosuredsT, FullNameT, IFunctionNameT, INameT, IVarNameT}
import net.verdagon.vale.templar.templata.ITemplata
import net.verdagon.vale.templar.types.{CoordT, StructTT, VariabilityT}
import net.verdagon.vale.{IProfiler, vassert, vcurious, vfail, vimpl, vwat}

import scala.collection.immutable.{List, Map, Set}

case class BuildingFunctionEnvironmentWithClosureds(
  globalEnv: GlobalEnvironment,
  fullName: FullNameT[BuildingFunctionNameWithClosuredsT],
  function: FunctionA,
  variables: Vector[IVariableT],
  globalNamespaces: Vector[TemplatasStore],
  localNamespaces: List[TemplatasStore]
) extends IEnvironment {

  val hash = runtime.ScalaRunTime._hashCode(fullName); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IEnvironment]) {
      return false
    }
    return fullName.equals(obj.asInstanceOf[IEnvironment].fullName)
  }

  override def lookupWithName(
    profiler: IProfiler,
    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    TemplatasStore.lookupWithName(
      globalEnv, localNamespaces, globalNamespaces, profiler, name, lookupFilter, getOnlyNearest)
  }

  override def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    TemplatasStore.lookupWithImpreciseName(
      globalEnv, localNamespaces, globalNamespaces, profiler, nameS, lookupFilter, getOnlyNearest)
  }
}

case class BuildingFunctionEnvironmentWithClosuredsAndTemplateArgs(
  globalEnv: GlobalEnvironment,
  fullName: FullNameT[BuildingFunctionNameWithClosuredsAndTemplateArgsT],
  function: FunctionA,
  variables: Vector[IVariableT],
  globalNamespaces: Vector[TemplatasStore],
  localNamespaces: List[TemplatasStore]
) extends IEnvironment {

  val hash = runtime.ScalaRunTime._hashCode(fullName); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IEnvironment]) {
      return false
    }
    return fullName.equals(obj.asInstanceOf[IEnvironment].fullName)
  }


  override def lookupWithName(
    profiler: IProfiler,
    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    TemplatasStore.lookupWithName(
      globalEnv, localNamespaces, globalNamespaces, profiler, name, lookupFilter, getOnlyNearest)
  }

  override def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    TemplatasStore.lookupWithImpreciseName(
      globalEnv, localNamespaces, globalNamespaces, profiler, nameS, lookupFilter, getOnlyNearest)
  }
}

case class FunctionEnvironment(
  // These things are the "environment"; they are the same for every line in a function.
  globalEnv: GlobalEnvironment,
  fullName: FullNameT[IFunctionNameT], // Includes the name of the function
  function: FunctionA,
  globalNamespaces: Vector[TemplatasStore],
  localNamespaces: List[TemplatasStore],
  maybeReturnType: Option[CoordT],

  // Eventually we might have a list of imported environments here, pointing at the
  // environments in the global environment.

  // This is stuff that's not state, but could be different line-to-line.
  // The containing Block, useful for looking up LocalS for any locals we're making.
  containingBlockS: Option[BlockSE],

  // The things below are the "state"; they can be different for any given line in a function.
  liveLocals: Vector[IVariableT],
  // This can refer to vars in parent environments, see UCRTVPE.
  unstackifieds: Set[FullNameT[IVarNameT]]
) extends IEnvironment {
  val hash = runtime.ScalaRunTime._hashCode(fullName); override def hashCode(): Int = hash;

  vassert(liveLocals == liveLocals.distinct)

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IEnvironment]) {
      return false
    }
    return fullName.equals(obj.asInstanceOf[IEnvironment].fullName)
  }

  def addVariables(newVars: Vector[IVariableT]): FunctionEnvironment = {
    FunctionEnvironment(globalEnv, fullName, function, globalNamespaces, localNamespaces, maybeReturnType, containingBlockS, liveLocals ++ newVars, unstackifieds)
  }
  def addVariable(newVar: IVariableT): FunctionEnvironment = {
    FunctionEnvironment(globalEnv, fullName, function, globalNamespaces, localNamespaces, maybeReturnType, containingBlockS, liveLocals :+ newVar, unstackifieds)
  }
  def markLocalUnstackified(newUnstackified: FullNameT[IVarNameT]): FunctionEnvironment = {
    vassert(!getAllUnstackifiedLocals().contains(newUnstackified))
    vassert(getAllLocals().exists(_.id == newUnstackified))
    // Even if the local belongs to a parent env, we still mark it unstackified here, see UCRTVPE.
    FunctionEnvironment(globalEnv, fullName, function, globalNamespaces, localNamespaces, maybeReturnType, containingBlockS, liveLocals, unstackifieds + newUnstackified)
  }

  def addEntry(useOptimization: Boolean, name: INameT, entry: IEnvEntry): FunctionEnvironment = {
    FunctionEnvironment(
      globalEnv,
      fullName,
      function,
      globalNamespaces,
      localNamespaces.head.addEntry(useOptimization, name, entry) :: localNamespaces.tail,
      maybeReturnType,
      containingBlockS,
      liveLocals,
      unstackifieds)
  }
  def addEntries(useOptimization: Boolean, newEntries: Map[INameT, Vector[IEnvEntry]]): FunctionEnvironment = {
    FunctionEnvironment(
      globalEnv,
      fullName,
      function,
      globalNamespaces,
      localNamespaces.head.addEntries(newEntries) :: localNamespaces.tail,
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
    TemplatasStore.lookupWithName(
      globalEnv, localNamespaces, globalNamespaces, profiler, name, lookupFilter, getOnlyNearest)
  }

  override def lookupWithImpreciseName(
    profiler: IProfiler,
    nameS: INameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata] = {
    TemplatasStore.lookupWithImpreciseName(
      globalEnv, localNamespaces, globalNamespaces, profiler, nameS, lookupFilter, getOnlyNearest)
  }

  def getVariable(name: IVarNameT): Option[IVariableT] = {
    liveLocals.find(_.id.last == name)
  }

  // Dont have a getAllUnstackifiedLocals or getAllLiveLocals here. We learned that the hard way.
  // See UCRTVPE, child environments would be the ones that know about their unstackifying of locals
  // from parent envs.

  def getAllLocals(): Vector[ILocalVariableT] = {
    liveLocals.collect({ case i : ILocalVariableT => i })
  }

  def getAllUnstackifiedLocals(): Vector[FullNameT[IVarNameT]] = {
    unstackifieds.toVector
  }

  def makeChildEnvironment( newContainingBlockS: Option[BlockSE]) = {
    FunctionEnvironment(
      globalEnv,
      fullName,
      function,
      globalNamespaces,
      TemplatasStore(fullName, Map(), Map()) :: localNamespaces,
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
  def fullName: FullNameT[IFunctionNameT] = functionEnvironment.fullName
  def function: FunctionA = functionEnvironment.function
  def maybeReturnType: Option[CoordT] = functionEnvironment.maybeReturnType
  def containingBlockS: Option[BlockSE] = functionEnvironment.containingBlockS
  def liveLocals: Vector[IVariableT] = functionEnvironment.liveLocals
  def unstackifieds: Set[FullNameT[IVarNameT]] = functionEnvironment.unstackifieds
  override def globalEnv: GlobalEnvironment = functionEnvironment.globalEnv
  override def globalNamespaces: Vector[TemplatasStore] = functionEnvironment.globalNamespaces
  override def localNamespaces: List[TemplatasStore] = functionEnvironment.localNamespaces

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

  def getAllLocals(): Vector[ILocalVariableT] = {
    functionEnvironment.getAllLocals()
  }

  def getAllUnstackifiedLocals(): Vector[FullNameT[IVarNameT]] = {
    functionEnvironment.getAllUnstackifiedLocals()
  }

  // Gets the effects that this environment had on the outside world (on its parent
  // environments).
  def getEffects(): Set[FullNameT[IVarNameT]] = {
    // We may have unstackified outside locals from inside the block, make sure
    // the parent environment knows about that.
    val unstackifiedAncestorLocals = unstackifieds -- liveLocals.map(_.id)
    unstackifiedAncestorLocals
  }

  def makeChildEnvironment( containingBlockS: Option[BlockSE]):
  FunctionEnvironmentBox = {
    FunctionEnvironmentBox(
      functionEnvironment
        .makeChildEnvironment(containingBlockS))
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
