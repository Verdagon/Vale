package dev.vale.typing.env

import dev.vale.highertyping.FunctionA
import dev.vale.{Interner, vassert, vcurious, vfail, vpass}
import dev.vale.postparsing._
import dev.vale.typing.ast.{LocationInFunctionEnvironment, ParameterT}
import dev.vale.typing.names.{BuildingFunctionNameWithClosuredsT, IFunctionNameT, IFunctionTemplateNameT, INameT, IRegionNameT, ITemplateNameT, IVarNameT, IdT}
import dev.vale.typing.templata.{FunctionTemplata, ITemplata}
import dev.vale.typing.types._
import dev.vale.highertyping._
import dev.vale.postparsing.IImpreciseNameS
import dev.vale.typing._
import dev.vale.typing.types.StructTT
import dev.vale.{Interner, Profiler, vassert, vcurious, vfail, vimpl, vpass, vwat}

import scala.collection.immutable.{List, Map, Set}

case class BuildingFunctionEnvironmentWithClosureds(
  globalEnv: GlobalEnvironment,
  parentEnv: IEnvironment,
  id: IdT[IFunctionTemplateNameT],
  defaultRegion: ITemplata[RegionTemplataType],
  templatas: TemplatasStore,
  function: FunctionA,
  variables: Vector[IVariableT],
  isRootCompilingDenizen: Boolean
) extends IInDenizenEnvironment {

  val hash = runtime.ScalaRunTime._hashCode(id); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IInDenizenEnvironment]) {
      return false
    }
    return id.equals(obj.asInstanceOf[IInDenizenEnvironment].id)
  }

  override def rootCompilingDenizenEnv: IInDenizenEnvironment = {
    if (isRootCompilingDenizen) {
      this
    } else {
      parentEnv match {
        case PackageEnvironment(_, _, _) => vwat()
        case _ => {
          parentEnv match {
            case parentInDenizenEnv : IInDenizenEnvironment => {
              parentInDenizenEnv.rootCompilingDenizenEnv
            }
            case _ => vwat()
          }
        }
      }
    }
  }

  private[env] override def lookupWithNameInner(

    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithNameInner(
      this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
  }

  private[env] override def lookupWithImpreciseNameInner(

    name: IImpreciseNameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithImpreciseNameInner(
      this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
  }
}

case class BuildingFunctionEnvironmentWithClosuredsAndTemplateArgs(
  globalEnv: GlobalEnvironment,
  parentEnv: IEnvironment,
  id: IdT[IFunctionTemplateNameT],
  templateArgs: Vector[ITemplata[ITemplataType]],
  templatas: TemplatasStore,
  function: FunctionA,
  variables: Vector[IVariableT],
  isRootCompilingDenizen: Boolean,
  defaultRegion: ITemplata[RegionTemplataType],
) extends IInDenizenEnvironment {

  val hash = runtime.ScalaRunTime._hashCode(id); override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IInDenizenEnvironment]) {
      return false
    }
    return id.equals(obj.asInstanceOf[IInDenizenEnvironment].id)
  }

  override def rootCompilingDenizenEnv: IInDenizenEnvironment = {
    if (isRootCompilingDenizen) {
      this
    } else {
      parentEnv match {
        case PackageEnvironment(_, _, _) => vwat()
        case _ => {
          parentEnv match {
            case parentInDenizenEnv : IInDenizenEnvironment => {
              parentInDenizenEnv.rootCompilingDenizenEnv
            }
            case _ => vwat()
          }
        }
      }
    }
  }

  private[env] override def lookupWithNameInner(

    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithNameInner(
      this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
  }

  private[env] override def lookupWithImpreciseNameInner(

    name: IImpreciseNameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithImpreciseNameInner(
      this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
  }

}

case class NodeEnvironment(
  parentFunctionEnv: FunctionEnvironment,
  parentNodeEnv: Option[NodeEnvironment],
  node: IExpressionSE,
  life: LocationInFunctionEnvironment,

  // The things below are the "state"; they can be different for any given line in a function.
  templatas: TemplatasStore,
  // This contains locals from parent blocks, see WTHPFE.
  declaredLocals: Vector[IVariableT],
  // This can refer to vars in parent blocks, see UCRTVPE.
  unstackifiedLocals: Set[IdT[IVarNameT]],

  defaultRegion: ITemplata[RegionTemplataType]
) extends IInDenizenEnvironment {
  vassert(declaredLocals.map(_.id) == declaredLocals.map(_.id).distinct)

  val hash = id.hashCode() ^ life.hashCode();
  override def hashCode(): Int = hash;
  override def equals(obj: Any): Boolean = {
    obj match {
      case that @ NodeEnvironment(_, _, _, _, _, _, _, _) => {
        id == that.id && life == that.life
      }
    }
  }

  override def rootCompilingDenizenEnv: IInDenizenEnvironment = {
//    parentEnv match {
//      case PackageEnvironment(_, _, _) => this
//      case _ => parentEnv.rootCompilingDenizenEnv
//    }
    parentEnv.rootCompilingDenizenEnv
  }

  override def id: IdT[IFunctionNameT] = parentFunctionEnv.id
  def function = parentFunctionEnv.function

  private[env] override def lookupWithNameInner(

    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithNameInner(
      this, templatas, parentNodeEnv.getOrElse(parentFunctionEnv), name, lookupFilter, getOnlyNearest)
  }

  private[env] override def lookupWithImpreciseNameInner(

    name: IImpreciseNameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithImpreciseNameInner(
      this, templatas, parentNodeEnv.getOrElse(parentFunctionEnv), name, lookupFilter, getOnlyNearest)
  }

  def globalEnv: GlobalEnvironment = parentFunctionEnv.globalEnv

  def parentEnv: IInDenizenEnvironment = {
    parentNodeEnv.getOrElse(parentFunctionEnv)
  }

  def getVariable(name: IVarNameT): Option[IVariableT] = {
    declaredLocals.find(_.id.localName == name) match {
      case Some(v) => Some(v)
      case None => {
        parentNodeEnv match {
          case Some(p) => p.getVariable(name)
          case None => {
            parentFunctionEnv.closuredLocals.find(_.id.localName == name)
          }
        }
      }
    }
  }

  // Dont have a getAllUnstackifiedLocals or getAllLiveLocals here. We learned that the hard way.
  // See UCRTVPE, child environments would be the ones that know about their unstackifying of locals
  // from parent envs.

  def getAllLocals(): Vector[ILocalVariableT] = {
    declaredLocals.collect({ case i : ILocalVariableT => i })
  }

  def getAllUnstackifiedLocals(): Vector[IdT[IVarNameT]] = {
    unstackifiedLocals.toVector
  }

  def addVariables(newVars: Vector[IVariableT]): NodeEnvironment = {
    NodeEnvironment(
      parentFunctionEnv, parentNodeEnv, node, life, templatas, declaredLocals ++ newVars, unstackifiedLocals, defaultRegion)
  }
  def addVariable(newVar: IVariableT): NodeEnvironment = {
    NodeEnvironment(parentFunctionEnv, parentNodeEnv, node, life, templatas, declaredLocals :+ newVar, unstackifiedLocals, defaultRegion)
  }
  def markLocalUnstackified(newUnstackified: IdT[IVarNameT]): NodeEnvironment = {
    vassert(!getAllUnstackifiedLocals().contains(newUnstackified))
    vassert(getAllLocals().exists(_.id == newUnstackified))
    // Even if the local belongs to a parent env, we still mark it unstackified here, see UCRTVPE.
    NodeEnvironment(parentFunctionEnv, parentNodeEnv, node, life, templatas, declaredLocals, unstackifiedLocals + newUnstackified, defaultRegion)
  }

  // Gets the effects that this environment had on the outside world (on its parent
  // environments). In other words, parent locals that were unstackified.
  def getEffectsSince(earlierNodeEnv: NodeEnvironment): Set[IdT[IVarNameT]] = {
    vassert(parentFunctionEnv == earlierNodeEnv.parentFunctionEnv)

    // We may have unstackified outside locals from inside the block, make sure
    // the parent environment knows about that.

    // declaredLocals contains things from parent environment, which is why we need to receive
    // an earlier environment to compare to, see WTHPFE.
    val earlierNodeEnvDeclaredLocals = earlierNodeEnv.declaredLocals.map(_.id).toSet
    val earlierNodeEnvLiveLocals = earlierNodeEnvDeclaredLocals -- earlierNodeEnv.unstackifiedLocals
    val liveLocalsIntroducedSinceEarlier =
      declaredLocals.map(_.id).filter(x => !earlierNodeEnvLiveLocals.contains(x))

    val unstackifiedAncestorLocals = unstackifiedLocals -- liveLocalsIntroducedSinceEarlier
    unstackifiedAncestorLocals
  }

  def getLiveVariablesIntroducedSince(
    sinceNenv: NodeEnvironment):
  Vector[ILocalVariableT] = {
    val localsAsOfThen =
      sinceNenv.declaredLocals.collect({
        case x @ ReferenceLocalVariableT(_, _, _) => x
        case x @ AddressibleLocalVariableT(_, _, _) => x
      })
    val localsAsOfNow =
      declaredLocals.collect({
        case x @ ReferenceLocalVariableT(_, _, _) => x
        case x @ AddressibleLocalVariableT(_, _, _) => x
      })

    vassert(localsAsOfNow.startsWith(localsAsOfThen))
    val localsDeclaredSinceThen = localsAsOfNow.slice(localsAsOfThen.size, localsAsOfNow.size)
    vassert(localsDeclaredSinceThen.size == localsAsOfNow.size - localsAsOfThen.size)

    val unmovedLocalsDeclaredSinceThen =
      localsDeclaredSinceThen.filter(x => !unstackifiedLocals.contains(x.id))

    unmovedLocalsDeclaredSinceThen
  }

  def makeChild(node: IExpressionSE): NodeEnvironment = {
    NodeEnvironment(
      parentFunctionEnv,
      Some(this),
      node,
      life,
      TemplatasStore(id, Map(), Map()),
      declaredLocals, // See WTHPFE.
      unstackifiedLocals, // See WTHPFE.
      defaultRegion)
  }

  def addEntry(interner: Interner, name: INameT, entry: IEnvEntry): NodeEnvironment = {
    NodeEnvironment(
      parentFunctionEnv,
      parentNodeEnv,
      node,
      life,
      templatas.addEntry(interner, name, entry),
      declaredLocals,
      unstackifiedLocals,
      defaultRegion)
  }
  def addEntries(interner: Interner, newEntries: Vector[(INameT, IEnvEntry)]): NodeEnvironment = {
    NodeEnvironment(
      parentFunctionEnv,
      parentNodeEnv,
      node,
      life,
      templatas.addEntries(interner, newEntries),
      declaredLocals,
      unstackifiedLocals,
      defaultRegion)
  }

  def nearestBlockEnv(): Option[(NodeEnvironment, BlockSE)] = {
    node match {
      case b @ BlockSE(_, _, _) => Some((this, b))
      case _ => parentNodeEnv.flatMap(_.nearestBlockEnv())
    }
  }
  def nearestLoopEnv(): Option[(NodeEnvironment, IExpressionSE)] = {
    node match {
      case w @ WhileSE(_, _) => Some((this, w))
      case w @ MapSE(_, _) => Some((this, w))
      case _ => parentNodeEnv.flatMap(_.nearestLoopEnv())
    }
  }
}

case class NodeEnvironmentBox(var nodeEnvironment: NodeEnvironment) {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vfail() // Shouldnt hash, is mutable

  def snapshot: NodeEnvironment = nodeEnvironment
  def defaultRegion: ITemplata[RegionTemplataType] = nodeEnvironment.defaultRegion
  def id: IdT[IFunctionNameT] = nodeEnvironment.parentFunctionEnv.id
  def node: IExpressionSE = nodeEnvironment.node
  def maybeReturnType: Option[CoordT] = nodeEnvironment.parentFunctionEnv.maybeReturnType
  def globalEnv: GlobalEnvironment = nodeEnvironment.globalEnv
  def declaredLocals: Vector[IVariableT] = nodeEnvironment.declaredLocals
  def unstackifieds: Set[IdT[IVarNameT]] = nodeEnvironment.unstackifiedLocals
  def function = nodeEnvironment.function
  def functionEnvironment = nodeEnvironment.parentFunctionEnv

  def addVariable(newVar: IVariableT): Unit= {
    nodeEnvironment = nodeEnvironment.addVariable(newVar)
  }
  def markLocalUnstackified(newMoved: IdT[IVarNameT]): Unit= {
    nodeEnvironment = nodeEnvironment.markLocalUnstackified(newMoved)
  }

  def getVariable(name: IVarNameT): Option[IVariableT] = {
    nodeEnvironment.getVariable(name)
  }

  def getAllLocals(): Vector[ILocalVariableT] = {
    nodeEnvironment.getAllLocals()
  }

  def getAllUnstackifiedLocals(): Vector[IdT[IVarNameT]] = {
    nodeEnvironment.getAllUnstackifiedLocals()
  }

  def lookupNearestWithImpreciseName(

    nameS: IImpreciseNameS,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata[ITemplataType]] = {
    nodeEnvironment.lookupNearestWithImpreciseName(nameS, lookupFilter)
  }

  def lookupNearestWithName(

    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata[ITemplataType]] = {
    nodeEnvironment.lookupNearestWithName(nameS, lookupFilter)
  }

  def lookupAllWithImpreciseName( nameS: IImpreciseNameS, lookupFilter: Set[ILookupContext]): Iterable[ITemplata[ITemplataType]] = {
    nodeEnvironment.lookupAllWithImpreciseName(nameS, lookupFilter)
  }

  def lookupAllWithName( nameS: INameT, lookupFilter: Set[ILookupContext]): Iterable[ITemplata[ITemplataType]] = {
    nodeEnvironment.lookupAllWithName(nameS, lookupFilter)
  }

  private[env] def lookupWithImpreciseNameInner( nameS: IImpreciseNameS, lookupFilter: Set[ILookupContext], getOnlyNearest: Boolean) = {
    nodeEnvironment.lookupWithImpreciseNameInner(nameS, lookupFilter, getOnlyNearest)
  }

  private[env] def lookupWithNameInner( nameS: INameT, lookupFilter: Set[ILookupContext], getOnlyNearest: Boolean) = {
    nodeEnvironment.lookupWithNameInner(nameS, lookupFilter, getOnlyNearest)
  }

  def makeChild(node: IExpressionSE): NodeEnvironment = {
    nodeEnvironment.makeChild(node)
  }

  def addEntry(interner: Interner, name: INameT, entry: IEnvEntry): Unit = {
    nodeEnvironment = nodeEnvironment.addEntry(interner, name, entry)
  }
  def addEntries(interner: Interner, newEntries: Vector[(INameT, IEnvEntry)]): Unit= {
    nodeEnvironment = nodeEnvironment.addEntries(interner, newEntries)
  }

  def nearestBlockEnv(): Option[(NodeEnvironment, BlockSE)] = {
    nodeEnvironment.nearestBlockEnv()
  }
  def nearestLoopEnv(): Option[(NodeEnvironment, IExpressionSE)] = {
    nodeEnvironment.nearestLoopEnv()
  }
}

case class FunctionEnvironment(
  // These things are the "environment"; they are the same for every line in a function.
  globalEnv: GlobalEnvironment,
  // This points to the environment containing the function, not parent blocks, see WTHPFE.
  parentEnv: IEnvironment,
  templateId: IdT[IFunctionTemplateNameT],
  id: IdT[IFunctionNameT], // Includes the name of the function

  templatas: TemplatasStore,

  function: FunctionA,
  maybeReturnType: Option[CoordT],

  closuredLocals: Vector[IVariableT],

  isRootCompilingDenizen: Boolean,

  defaultRegion: ITemplata[RegionTemplataType]

  // Eventually we might have a list of imported environments here, pointing at the
  // environments in the global environment.
) extends IInDenizenEnvironment {
  val hash = runtime.ScalaRunTime._hashCode(id); override def hashCode(): Int = hash;

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[IInDenizenEnvironment]) {
      return false
    }
    return id.equals(obj.asInstanceOf[IInDenizenEnvironment].id)
  }

  override def rootCompilingDenizenEnv: IInDenizenEnvironment = {
    if (isRootCompilingDenizen) {
      this
    } else {
      parentEnv match {
        case PackageEnvironment(_, _, _) => vwat()
        case _ => {
          parentEnv match {
            case parentInDenizenEnv : IInDenizenEnvironment => {
              parentInDenizenEnv.rootCompilingDenizenEnv
            }
            case _ => vwat()
          }
        }
      }
    }
  }

  def templata = FunctionTemplata(parentEnv, function)

  def addEntry(interner: Interner, name: INameT, entry: IEnvEntry): FunctionEnvironment = {
    FunctionEnvironment(
      globalEnv,
      parentEnv,
      templateId,
      id,
      templatas.addEntry(interner, name, entry),
      function,
      maybeReturnType,
      closuredLocals,
      isRootCompilingDenizen,
      defaultRegion)
  }
  def addEntries(interner: Interner, newEntries: Vector[(INameT, IEnvEntry)]): FunctionEnvironment = {
    FunctionEnvironment(
      globalEnv,
      parentEnv,
      templateId,
      id,
      templatas.addEntries(interner, newEntries),
      function,
      maybeReturnType,
      closuredLocals,
      isRootCompilingDenizen,
      defaultRegion)
  }

  private[env] override def lookupWithNameInner(

    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithNameInner(
      this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
  }

  private[env] override def lookupWithImpreciseNameInner(

    name: IImpreciseNameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    EnvironmentHelper.lookupWithImpreciseNameInner(
      this, templatas, parentEnv, name, lookupFilter, getOnlyNearest)
  }

  def makeChildNodeEnvironment(node: IExpressionSE, life: LocationInFunctionEnvironment): NodeEnvironment = {
    // See WTHPFE, if this is a lambda, we let our blocks start with
    // locals from the parent function.
    val (declaredLocals, unstackifiedLocals) =
      parentEnv match {
        case NodeEnvironment(_, _, _, _, _, declaredLocals, unstackifiedLocals, _) => {
          (declaredLocals, unstackifiedLocals)
        }
        case _ => (Vector(), Set[IdT[IVarNameT]]())
      }

    NodeEnvironment(
      this,
      None,
      node,
      life,
      TemplatasStore(id, Map(), Map()),
      declaredLocals, // See WTHPFE.
      unstackifiedLocals, // See WTHPFE.
      defaultRegion)
  }

  def getClosuredDeclaredLocals(): Vector[IVariableT] = {
    parentEnv match {
      case n @ NodeEnvironment(_, _, _, _, _, _, _, _) => n.declaredLocals
      case f @ FunctionEnvironment(_, _, _, _, _, _, _, _, _, _) => f.getClosuredDeclaredLocals()
      case _ => Vector()
    }
  }

//  def getClosuredUnstackifiedLocals(): Vector[IVariableT] = {
//    parentEnv match {
//      case n @ NodeEnvironment(_, _, _, _, _, _, _) => n.unstackifiedLocals
//      case f @ FunctionEnvironment(_, _, _, _, _, _) => f.getClosuredDeclaredLocals()
//      case _ => Vector()
//    }
//  }

  // No particular reason we don't have an addFunction like PackageEnvironment does
}

case class FunctionEnvironmentBox(var functionEnvironment: FunctionEnvironment) extends IDenizenEnvironmentBox {
  override def equals(obj: Any): Boolean = vcurious(); override def hashCode(): Int = vfail() // Shouldnt hash, is mutable

  override def snapshot: FunctionEnvironment = functionEnvironment
  def id: IdT[IFunctionNameT] = functionEnvironment.id
  def function: FunctionA = functionEnvironment.function
  def maybeReturnType: Option[CoordT] = functionEnvironment.maybeReturnType

  override def defaultRegion: ITemplata[RegionTemplataType] = functionEnvironment.defaultRegion
  override def globalEnv: GlobalEnvironment = functionEnvironment.globalEnv
  override def templatas: TemplatasStore = functionEnvironment.templatas
  override def rootCompilingDenizenEnv: IInDenizenEnvironment = functionEnvironment.rootCompilingDenizenEnv

  def setReturnType(returnType: Option[CoordT]): Unit = {
    functionEnvironment = functionEnvironment.copy(maybeReturnType = returnType)
  }

  def addEntry(interner: Interner, name: INameT, entry: IEnvEntry): Unit = {
    functionEnvironment = functionEnvironment.addEntry(interner, name, entry)
  }
  def addEntries(interner: Interner, newEntries: Vector[(INameT, IEnvEntry)]): Unit= {
    functionEnvironment = functionEnvironment.addEntries(interner, newEntries)
  }

  override def lookupNearestWithImpreciseName(

    nameS: IImpreciseNameS,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata[ITemplataType]] = {
    functionEnvironment.lookupNearestWithImpreciseName(nameS, lookupFilter)
  }

  override def lookupNearestWithName(

    nameS: INameT,
    lookupFilter: Set[ILookupContext]):
  Option[ITemplata[ITemplataType]] = {
    functionEnvironment.lookupNearestWithName(nameS, lookupFilter)
  }

  override def lookupAllWithImpreciseName( nameS: IImpreciseNameS, lookupFilter: Set[ILookupContext]): Iterable[ITemplata[ITemplataType]] = {
    functionEnvironment.lookupAllWithImpreciseName(nameS, lookupFilter)
  }

  override def lookupAllWithName( nameS: INameT, lookupFilter: Set[ILookupContext]): Iterable[ITemplata[ITemplataType]] = {
    functionEnvironment.lookupAllWithName(nameS, lookupFilter)
  }

  override private[env] def lookupWithImpreciseNameInner( nameS: IImpreciseNameS, lookupFilter: Set[ILookupContext], getOnlyNearest: Boolean) = {
    functionEnvironment.lookupWithImpreciseNameInner(nameS, lookupFilter, getOnlyNearest)
  }

  override private[env] def lookupWithNameInner( nameS: INameT, lookupFilter: Set[ILookupContext], getOnlyNearest: Boolean) = {
    functionEnvironment.lookupWithNameInner(nameS, lookupFilter, getOnlyNearest)
  }

  def makeChildNodeEnvironment(node: IExpressionSE, life: LocationInFunctionEnvironment): NodeEnvironment = {
    functionEnvironment.makeChildNodeEnvironment(node, life)
  }

  // No particular reason we don't have an addFunction like PackageEnvironment does
}

sealed trait IVariableT  {
  def id: IdT[IVarNameT]
  def variability: VariabilityT
  def coord: CoordT
}
sealed trait ILocalVariableT extends IVariableT {
  def coord: CoordT
  def id: IdT[IVarNameT]
}
// Why the difference between reference and addressible:
// If we mutate/move a variable from inside a closure, we need to put
// the local's address into the struct. But, if the closures don't
// mutate/move, then we could just put a regular reference in the struct.
// Lucky for us, the parser figured out if any of our child closures did
// any mutates/moves/borrows.
case class AddressibleLocalVariableT(
  id: IdT[IVarNameT],
  variability: VariabilityT,
  coord: CoordT
) extends ILocalVariableT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();

}
case class ReferenceLocalVariableT(
  id: IdT[IVarNameT],
  variability: VariabilityT,
  coord: CoordT
) extends ILocalVariableT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();
  vpass()
}
case class AddressibleClosureVariableT(
  id: IdT[IVarNameT],
  closuredVarsStructType: StructTT,
  variability: VariabilityT,
  coord: CoordT
) extends IVariableT {
  vpass()
}
case class ReferenceClosureVariableT(
  id: IdT[IVarNameT],
  closuredVarsStructType: StructTT,
  variability: VariabilityT,
  coord: CoordT
) extends IVariableT {
  val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious();

}

object EnvironmentHelper {
  def lookupWithNameInner(
    requestingEnv: IEnvironment,
    templatas: TemplatasStore,
    parent: IEnvironment,

    name: INameT,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    val result = templatas.lookupWithNameInner(requestingEnv, name, lookupFilter)
    if (result.nonEmpty && getOnlyNearest) {
      result
    } else {
      result ++ parent.lookupWithNameInner(name, lookupFilter, getOnlyNearest)
    }
  }

  def lookupWithImpreciseNameInner(
    requestingEnv: IEnvironment,
    templatas: TemplatasStore,
    parent: IEnvironment,

    name: IImpreciseNameS,
    lookupFilter: Set[ILookupContext],
    getOnlyNearest: Boolean):
  Iterable[ITemplata[ITemplataType]] = {
    val result = templatas.lookupWithImpreciseNameInner(requestingEnv, name, lookupFilter)
    if (result.nonEmpty && getOnlyNearest) {
      result
    } else {
      result ++ parent.lookupWithImpreciseNameInner(name, lookupFilter, getOnlyNearest)
    }
  }
}