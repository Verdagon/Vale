package dev.vale.typing

//import dev.vale.astronomer.{GlobalFunctionFamilyNameS, INameS, INameA, ImmConcreteDestructorImpreciseNameA, ImmConcreteDestructorNameA, ImmInterfaceDestructorImpreciseNameS}
//import dev.vale.astronomer.VirtualFreeImpreciseNameS
import dev.vale.{Err, Interner, Keywords, Ok, RangeS, U, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing.{GlobalFunctionFamilyNameS, IImpreciseNameS, ITemplataType, ReachablePrototypeRuneS, RuneNameS}
import dev.vale.typing.ast.{InterfaceEdgeBlueprint, PrototypeT}
import dev.vale.typing.env.{GeneralEnvironment, IEnvironment, TemplataEnvEntry, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.types._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.function.FunctionCompiler.{EvaluateFunctionFailure, EvaluateFunctionSuccess}
import dev.vale.typing.names.{FullNameT, ICitizenNameT, ICitizenTemplateNameT, IFunctionTemplateNameT, IImplTemplateNameT, IInterfaceNameT, IInterfaceTemplateNameT, ITemplateNameT, ImplOverrideCaseNameT, ImplOverrideNameT, InterfaceTemplateNameT, PlaceholderNameT, PlaceholderTemplateNameT, ReachablePrototypeNameT, RuneNameT, StructTemplateNameT}
import dev.vale.typing.templata.ITemplata.expectKindTemplata
import dev.vale.typing.templata.{CoordTemplata, FunctionTemplata, ITemplata, KindTemplata, PlaceholderTemplata}
import dev.vale.typing.types._

import scala.collection.mutable

sealed trait IMethod
case class NeededOverride(
  name: IImpreciseNameS,
  paramFilters: Vector[CoordT]
) extends IMethod { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }
case class FoundFunction(prototype: PrototypeT) extends IMethod { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

case class PartialEdgeT(
  struct: StructTT,
  interface: InterfaceTT,
  methods: Vector[IMethod]) { val hash = runtime.ScalaRunTime._hashCode(this); override def hashCode(): Int = hash; override def equals(obj: Any): Boolean = vcurious(); }

class EdgeCompiler(
    interner: Interner,
    keywords: Keywords,
    functionCompiler: FunctionCompiler,
    overloadCompiler: OverloadResolver,
    implCompiler: ImplCompiler) {
  def compileITables(coutputs: CompilerOutputs):
  (
    Vector[InterfaceEdgeBlueprint],
    Map[
      FullNameT[IInterfaceNameT],
      Map[
        FullNameT[ICitizenNameT],
        EdgeT]]) = {
    val interfaceEdgeBlueprints =
      makeInterfaceEdgeBlueprints(coutputs)

    val itables =
      interfaceEdgeBlueprints.map(interfaceEdgeBlueprint => {
        val interfacePlaceholderedFullName = interfaceEdgeBlueprint.interface
        val interfaceTemplateFullName = TemplataCompiler.getInterfaceTemplate(interfacePlaceholderedFullName)
        val interfaceFullName =
          coutputs.lookupInterface(interfaceTemplateFullName).instantiatedInterface.fullName
        val interfaceDefinition = coutputs.lookupInterface(interfaceTemplateFullName)
//        val interfacePlaceholderedCitizen = interfaceDefinition.placeholderedInterface
        val overridingImpls = coutputs.getChildImplsForSuperInterfaceTemplate(interfaceTemplateFullName)
        val overridingCitizenToFoundFunction =
          overridingImpls.map(overridingImpl => {
            val overridingCitizenTemplateFullName = overridingImpl.subCitizenTemplateFullName
            val superInterfaceWithSubCitizenPlaceholders = overridingImpl.superInterface

            // Our goal now is to figure out the override functions.
            // Imagine we have these interfaces and impls:
            //
            //   interface ISpaceship<E, F, G> { ... }
            //
            //   struct Serenity<A, B, C> { ... }
            //   impl<H, I, J> ISpaceship<H, I, J> for Serenity<H, I, J>;
            //
            //   struct Firefly<A, B, C> { ... }
            //   impl<H, I, J> ISpaceship<J, I, H> for Firefly<H, I, J>;
            //   // Note the weird order here ^
            //
            //   struct Raza<B, C> { ... }
            //   impl<I, J> ISpaceship<int, I, J> for Raza<I, J>;
            //   // Note this int here ^
            //
            //   struct Milano<A, B, C, D> { ... }
            //   impl<I, J, K, L> ISpaceship<I, J, K> for Milano<I, J, K, L>;
            //   // Note that Milano has more params than ISpaceship.
            //   // This is like a MyStruct<T> implementing a IIntObserver.
            //
            //   struct Enterprise<A> { ... }
            //   impl<H> ISpaceship<H, H, H> for Enterprise<H>;
            //   // Note they're all the same type
            //
            // If we have an abstract function:
            //
            //   abstract func moo<X, Y, Z>(self &ISpaceship<X, Y, Z>, bork X) where exists drop(Y)void;
            //
            // and these overrides:
            //
            //   func moo<X, Y, Z>(self &Serenity<X, Y, Z>, bork X) where exists drop(Y)void { ... }
            //   func moo<X, Y, Z>(self &Firefly<X, Y, Z>, bork X) where exists drop(Y)void { ... }
            //   func moo<Y, Z>(self &Raza<Y, Z>, bork int) where exists drop(Y)void { ... }
            //   func moo<X, Y, Z, ZZ>(self &Milano<X, Y, Z, ZZ>, bork X) where exists drop(Y)void { ... }
            //   func moo<X>(self &Enterprise<X>, bork X) where exists drop(X)void { ... }
            //
            // We need to find those overrides.
            //
            // To do it, we need to *conceptually* lower these abstract functions to match-dispatching
            // functions. We're not actually doing this, just thinking this way. One might be:
            //
            //   func moo<X, Y, Z>(virtual self &ISpaceship<X, Y, Z>, bork X)
            //   where exists drop(Y)void {
            //     self match {
            //       serenity &Serenity<X, Y, Z> => moo(serenity, bork)
            //       firefly &Firefly<Z, Y, X> => moo(firefly, bork)
            //       <ZZ> milano &Milano<X, Y, Z, ZZ> => moo(milano, bork) // See the end for wtf this is
            //       // Read on for why the other cases aren't here
            //     }
            //   }
            //
            // Raza and Enterprise have some assumptions about their generic args, so we'll need different
            // conceptual functions for them.
            //
            //   func moo<Y, Z>(virtual self &ISpaceship<int, Y, Z>, bork int)
            //   where exists drop(Y)void {
            //     self match {
            //       raza &Raza<Y, Z> => moo(raza, bork)
            //       // other cases unimportant for our purposes
            //     }
            //   }
            //
            //   func moo<X>(virtual self &ISpaceship<X, X, X>, bork X)
            //   where exists drop(Y)void {
            //     self match {
            //       enterprise &Enterprise<X> => moo(enterprise, bork)
            //       // other cases unimportant for our purposes
            //     }
            //   }
            //
            // The reason we do all this is so we can do those resolves:
            //
            //    * moo(serenity) is resolving moo(&Serenity<X, Y, Z>, X)
            //    * moo(firefly) is resolving moo(&Firefly<Z, Y, X>, X)
            //    * moo(raza) is resolving moo(&Raza<Y, Z>, int)
            //    * moo(enterprise) is resolving moo(&Enterprise<H>, X)
            //
            // So, the below code does the important parts of the above conceptual functions.
            //
            // Now, how do we handle Milano's case?
            //
            //   func moo<X, Y, Z>(virtual self &ISpaceship<X, Y, Z>, bork X)
            //   where exists drop(Y)void {
            //     self match {
            //       serenity &Serenity<X, Y, Z> => moo(serenity, bork)
            //       firefly &Firefly<Z, Y, X> => moo(firefly, bork)
            //       <ZZ> milano &Milano<X, Y, Z, ZZ> => moo(milano, bork)
            //     }
            //   }
            //
            // As you can see, it doesn't really fit into this whole match/enum paradigm.
            // There could be any number of Milano variants in there... ZZ could be int, or str, or bool,
            // or whatever. Luckily there is a solution, described further below.


            val foundFunctions =
              interfaceEdgeBlueprint.superFamilyRootHeaders.map({ case (abstractFunctionPrototype, abstractIndex) =>
                val abstractFuncTemplateFullName =
                  TemplataCompiler.getFunctionTemplate(abstractFunctionPrototype.fullName)
                val abstractFunctionParamUnsubstitutedTypes = abstractFunctionPrototype.paramTypes
                vassert(abstractIndex >= 0)
                val abstractParamUnsubstitutedType = abstractFunctionParamUnsubstitutedTypes(abstractIndex)

                val maybeOriginFunctionTemplata =
                  coutputs.lookupFunction(abstractFunctionPrototype.toSignature)
                    .flatMap(_.header.maybeOriginFunctionTemplata)

                val range =
                  maybeOriginFunctionTemplata.map(_.function.range)
                    .getOrElse(RangeS.internal(interner, -2976395))

                val originFunctionTemplata = vassertSome(maybeOriginFunctionTemplata)

                // Recall:
                //
                //   interface ISpaceship<E, F, G> { ... }
                //   abstract func moo<X, Y, Z>(self &ISpaceship<X, Y, Z>, bork X) where exists drop(Y)void;
                //
                //   struct Raza<A, B, C> { ... }
                //   impl<I, J> ISpaceship<int, I, J> for Raza<I, J>;
                //
                //   func moo<Y, Z>(self &Raza<Y, Z>, bork int) where exists drop(Y)void { ... }
                //
                // Right now we have ISpaceship, moo, and the impl ("ri").
                //
                // We want to locate that moo/Raza override, similarly to the above conceptual
                // match-dispatching function:
                //
                //   func moo<Y, Z>(virtual self &ISpaceship<int, Y, Z>, bork int)
                //   where exists drop(Y)void {
                //     self match {
                //       raza &Raza<Y, Z> => moo(raza, bork)
                //     }
                //   }
                //
                // The first step is figuring out this function's inner environment, so we can
                // later use it to resolve our moo override.
                //
                // Start by compiling the impl, supplying any placeholders for it.
                //
                //   impl<I, J> ISpaceship<int, I, J> for Raza<I, J>;
                //
                // becomes:
                //
                //   impl<ri$0, ri$1> ISpaceship<int, ri$0, ri$1> for Raza<ri$0, ri$1>
                //
                // Now, take the original abstract function:
                //
                //   abstract func moo<X, Y, Z>(self &ISpaceship<X, Y, Z>, bork X) where exists drop(Y)void;
                //
                // and try to compile it given the impl's interface (ISpaceship<int, ri$0, ri$1>)
                // as the first parameter. However, rephrase the placeholders to be in terms of the abstract
                // function itself (so instead of ri$0, think of it as moo$0)
                //
                //   abstract func moo(self ISpaceship<int, moo$0, moo$1>, bork int) where exists drop(ri$0)void;
                //
                // We rephrased like that because:
                //
                //  1. We're conceptually compiling this function, not resolving it.
                //  2. There's an extremely valuable sanity check (OWPFRD) that will trip if we don't.
                //
                // In a way, we compiled it as if X = int, Y = moo$0, Z = moo$1.
                //
                // And now we have our inner environment from which we can resolve some overloads.

                // This is an array of substitutions, for example:
                // - Replace ri$0 with moo$0
                // - Replace ri$1 with moo$1
                // So that instead of ISpaceship<int, ri$0, ri$1> we'll have ISpaceship<int, moo$0, moo$1>.

                val abstractFuncOuterEnv =
                  coutputs.getOuterEnvForFunction(abstractFuncTemplateFullName)

                val implOverrideName =
                  interner.intern(ImplOverrideNameT(overridingImpl.templateFullName))
                val implOverrideFullName =
                  abstractFuncTemplateFullName.addStep(implOverrideName)
                val implOverrideEnv =
                  GeneralEnvironment.childOf(
                    interner,
                    abstractFuncOuterEnv,
                    implOverrideFullName)

                // There's a slight chance it's not ISomeInterface<ri$0, ri$1> but instead something
                // like ISomeInterface<ri$3, ri$3>. The indexes won't necessarily line up, so this is a map
                // instead of a vector.
                val placeholderToSubstitution = mutable.HashMap[FullNameT[PlaceholderNameT], ITemplata[ITemplataType]]()
                val substitutions =
                  U.map[ITemplata[ITemplataType], (FullNameT[PlaceholderNameT], ITemplata[ITemplataType])](
                    overridingImpl.instantiatedFullName.last.templateArgs.toArray,
                    {
                      case PlaceholderTemplata(implPlaceholderFullName @ FullNameT(packageCoord, initSteps, PlaceholderNameT(PlaceholderTemplateNameT(index))), tyype) => {
                        // Sanity check we're in an impl template, we're about to replace it with a function template
                        initSteps.last match { case _ : IImplTemplateNameT => case _ => vwat() }

                        implPlaceholderFullName ->
                          (placeholderToSubstitution.get(implPlaceholderFullName) match {
                            case Some(existing) => existing
                            case None => {
                              val abstractFuncPlaceholderFullName =
                                createOverridePlaceholder(coutputs, implOverrideEnv, placeholderToSubstitution.size)
                              val abstractFuncPlaceholder = PlaceholderTemplata(abstractFuncPlaceholderFullName, tyype)
                              placeholderToSubstitution.put(implPlaceholderFullName, abstractFuncPlaceholder)
                              abstractFuncPlaceholder
                            }
                          })
                      }
                      case KindTemplata(PlaceholderT(implPlaceholderFullName @ FullNameT(packageCoord, initSteps, PlaceholderNameT(PlaceholderTemplateNameT(index))))) => {
                        // Sanity check we're in an impl template, we're about to replace it with a function template
                        initSteps.last match { case _ : IImplTemplateNameT => case _ => vwat() }

                        implPlaceholderFullName ->
                        (placeholderToSubstitution.get(implPlaceholderFullName) match {
                          case Some(existing) => existing
                          case None => {
                            val abstractFuncPlaceholderFullName =
                              createOverridePlaceholder(coutputs, implOverrideEnv, placeholderToSubstitution.size)
                            val abstractFuncPlaceholder = KindTemplata(PlaceholderT(abstractFuncPlaceholderFullName))
                            placeholderToSubstitution.put(implPlaceholderFullName, abstractFuncPlaceholder)
                            abstractFuncPlaceholder
                          }
                        })
                      }
                      case CoordTemplata(CoordT(ownership, PlaceholderT(implPlaceholderFullName @ FullNameT(packageCoord, initSteps, PlaceholderNameT(PlaceholderTemplateNameT(index)))))) => {
                        // Sanity check we're in an impl template, we're about to replace it with a function template
                        initSteps.last match { case _ : IImplTemplateNameT => case _ => vwat() }

                        implPlaceholderFullName ->
                          (placeholderToSubstitution.get(implPlaceholderFullName) match {
                            case Some(existing) => existing
                            case None => {
                              val abstractFuncPlaceholderFullName =
                                createOverridePlaceholder(coutputs, implOverrideEnv, placeholderToSubstitution.size)
                              val abstractFuncPlaceholder = CoordTemplata(CoordT(ownership, PlaceholderT(abstractFuncPlaceholderFullName)))
                              placeholderToSubstitution.put(implPlaceholderFullName, abstractFuncPlaceholder)
                              abstractFuncPlaceholder
                            }
                          })
                      }
                      case other => vwat(other)
                    })
                val abstractFuncPlaceholderedInterface =
                  expectKindTemplata(
                    TemplataCompiler.substituteTemplatasInTemplata(
                      coutputs,
                      interner,
                      keywords,
                      KindTemplata(overridingImpl.superInterface),
                      substitutions.toArray)).kind.expectInterface()
                val abstractParamType =
                  abstractParamUnsubstitutedType.copy(kind = abstractFuncPlaceholderedInterface)
                // Now we have a ISpaceship<int, moo$0, moo$1> that we can use to compile the abstract
                // function header. (Using the Raza example)
                // In the Milano case, we have an ISpaceship<moo$0, moo$1, moo$2> and also another
                // substitution for a moo$3 that doesnt actually correspond to any template parameter
                // of the abstract function.

                val EvaluateFunctionSuccess(dispatchingAbstractFuncPrototype, abstractFuncInferences) =
                  functionCompiler.evaluateGenericLightFunctionParentForPrototype(
                    coutputs,
                    List(range, overridingImpl.templata.impl.range),
                    abstractFuncOuterEnv,
                    originFunctionTemplata,
                    abstractFunctionPrototype.paramTypes.indices.map(_ => None)
                      .updated(abstractIndex, Some(abstractParamType))
                      .toVector) match {
                    case EvaluateFunctionFailure(x) => {
                      throw CompileErrorExceptionT(CouldntEvaluateFunction(List(range), x))
                    }
                    case efs @ EvaluateFunctionSuccess(_, _) => efs
                  }
                // Recall the match-dispatching function:
                //
                //   func moo<Y, Z>(virtual self &ISpaceship<int, Y, Z>, bork int)
                //   where exists drop(Y)void {
                //     self match {
                //       raza &Raza<Y, Z> => moo(raza, bork)
                //       // other cases unimportant for our purposes
                //     }
                //   }
                //
                // Now we have it's inner environment's inferences! We'll construct an IEnvironment below
                // containing these. Then we can use this to resolve some overrides.

                // For the Milano case, we'll add to the inner environment the extra placeholder
                // that doesn't correspond to any template argument in the abstract function.
                // Conceptually, we're in a compile-time pack expansion. We're basically expanding this:
                //
                //       milano &Milano<X, Y, Z, ...> => moo(milano, bork)
                //
                // into a bunch of:
                //
                //


                // This is so we fill the other params' types, like that bork int.
                //
                // Now, try to resolve an overload with the impl's struct there instead, look for:
                //   moo(Raza<ri$0, ri$1>, int)
                // and sure enough, we find the override func:
                //   func moo<P, Q>(self Raza<P, Q>, bork int) where exists drop(P)void;
                //
                // Instantiating it is the next challenge, we'll do that below.
                //
                // All this is done from the impl's perspective, the impl is the original calling
                // env for all these solves and resolves, and all these placeholders are phrased in
                // terms of impl placeholders (eg ri$0).

                // Evaluate the function as if we're defining it, even using the definition site rules.
                // We'll even take the inferences and add em to an environment later.
                // The only difference from real defining is that we're handing in an actual parameter,
                // namely the impl's super interface.
                //


//                // If we have a:
//                //   impl<A, B> ISpaceship<int, A, B> for Serenity<A, B>;
//                // This will be the ISpaceship<int, A, B>.
//                // We'll use it below to make an abstract function.
//                val interfaceTypeForThisImpl =
//                  abstractFuncPrototype.prototype.paramTypes(
//                    vassertSome(abstractFunctionHeader.getVirtualIndex)).kind.expectInterface()
//                strt here
//                // this is phrased in terms of the impl. makes sense, because we fed in abstractFunctionType
//                // which was phrased in terms of the original impl.
//                // now we need to combine this with the abstract function as if it's taking one of these in...

//
//                val superFunctionParamTypes = abstractFuncPrototype.prototype.paramTypes
//
//                val implPlaceholderedOverridingCitizen = overridingImpl.placeholderedSubCitizen
//                val overridingParamCoord = abstractParamType.copy(kind = implPlaceholderedOverridingCitizen)
//                val overrideFunctionParamTypes =
//                  superFunctionParamTypes.updated(abstractIndex, overridingParamCoord)
//
                val overrideImpreciseName =
                  vassertSome(
                    TemplatasStore.getImpreciseName(
                      interner, abstractFunctionPrototype.fullName.last))
//
//                // See ONBIFS and NBIFPR for why we need these bounds in our below env.
//                val overridingKindReachableBounds =
//                  TemplataCompiler.getReachableBounds(
//                    interner, keywords, coutputs, KindTemplata(implPlaceholderedOverridingCitizen))

//                val abstractFuncInnerEnv =
//                  coutputs.getInnerEnvForFunction(abstractFunctionHeader.fullName)

                // We'll solve the impl given the placeholdered super interface.
                // So if we have an abstract function:
                //   func moo<T>(virtual a ISpaceship<int, T>);
                // Imagine this body:
                //   func moo<T, Z>(virtual self &ISpaceship<int, T, Z>) {
                //     self match {
                //       myShip Serenity<int, T, Z> => moo(myShip)
                //       myBike Raza<int, Z, T> => moo(myBike)
                //     }
                //   }
                // Imagine we're actually compiling it, which means we have placeholders:
                //   func moo<$0, $1>(virtual self &ISpaceship<int, $0, $1>) {
                //     self match {
                //       myShip &Serenity<int, $0, $1> => moo(myShip)
                //       myBike &Raza<int, $1, $0> => moo(myBike)
                //     }
                //   }
                // Right here, we're trying to resolve an override function, eg `moo(myBike)`.
                // First, we need to figure out `Raza<$1, $0>`. That's what this impl solve is
                // doing. It's feeding the ISpaceship<$0, $1> into the impl:
                //   impl<T, Z> ISpaceship<T, Z> for Raza<Z, T>;
                // roughly solving to:
                //   impl<moo$0, moo$1> ISpaceship<moo$0, moo$1> for Raza<moo$1, moo$0>;
                // to get the `Raza<moo$1, moo$0>`.
                //strt here
                // At test:test.vale:7:3:
                //   func go(virtual this &MyIFunction1<P1, R>, param P1) R;
                // Couldn't evaluate impl statement:
                // Conflict, thought rune _31311 was i32 but now concluding it's Kind$_0
                // impl MyIFunction1<int, int> for MyFunc;
                //                                 ^^^^^^ _2111: (unknown)
                //                        ^^^ _31411: Kind$_1
                //                   ^^^ _31311: Kind$_0
                //      ^^^^^^^^^^^^^^^^^^^^^^ _311: MyIFunction1<Kind$_0, Kind$_1>
                //      ^^^^^^^^^^^^ _31211: MyIFunction1
                //
                // Forgot about these dang specializing impls.
                // Perhaps we should first solve the impl given its own placeholders, and then
                // somehow enter into the abstract function with that as the interface.
                val abstractFuncPlaceholderedSubCitizen =
                  expectKindTemplata(
                    TemplataCompiler.substituteTemplatasInTemplata(
                      coutputs,
                      interner,
                      keywords,
                      KindTemplata(overridingImpl.subCitizen),
                      substitutions.toArray))
                    .kind.expectCitizen()

                // We need this to pull in some bounds knowledge from the override struct.
                //
                // For example, this is a parent interface that has no knowledge or assumptions of
                // being droppable:
                //
                //   #!DeriveInterfaceDrop
                //   sealed interface ILaunchable {
                //     func launch(virtual self &ILaunchable) int;
                //   }
                //
                //   #!DeriveStructDrop
                //   struct Ship<T>
                //   where func drop(Lam)void, func __call(&Lam)int {
                //     lam Lam;
                //   }
                //
                //   impl<T> ILaunchable for Ship<T>;
                //
                //   func launch<T>(self &Ship<T>) int {
                //     return (self.lam)();
                //   }
                //
                // When resolving overrides for it, this is the conceptual case:
                //
                //   func launch(virtual self &ILaunchable) {
                //     self match {
                //       <ZZ> borky &Ship<ZZ> => bork(fwd)
                //     }
                //   }
                //
                // However, there's something subtle that's needed. The bork(fwd) call is trying to resolve
                // this function:
                //
                //   func launch<T>(self &Ship<T>) int
                //
                // However, the `Ship<T>` invocation requires that Lam has a `drop`... which nobody
                // can guarantee.
                //
                // But wait! We're taking an *existing* Ship<T> there. So we can know that the T already
                // supports a drop.
                //
                // We do this for NBIFPR for parameters and returns and one day for cases inside matches.
                // Let's do it here for this conceptual case too.
                val boundsForCase =
                  TemplataCompiler.getReachableBounds(interner, keywords, coutputs, KindTemplata(abstractFuncPlaceholderedSubCitizen))
                    .zipWithIndex
                    .map({ case (templata, num) => ReachablePrototypeRuneS(num) -> templata })

                // We don't do this here:
                //   coutputs.getInnerEnvForFunction(abstractFunctionPrototype.fullName)
                // because that will get the original declaration's inner env.
                // We want an environment with the above inferences instead.
                val implOverrideCaseEnv =
                  GeneralEnvironment.childOf(
                    interner,
                    implOverrideEnv,
                    implOverrideEnv.fullName.addStep(interner.intern(ImplOverrideCaseNameT())),
                    (abstractFuncInferences ++ boundsForCase)

                      .map({ case (nameS, templata) =>
                        interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
                      }).toVector)
//
//                  implCompiler.getImplDescendantGivenParent(
//                    coutputs,
//                    List(range),
//                    abstractFuncInnerEnv,
//                    overridingImpl.templata,
//                    abstractFuncPlaceholderedInterface,
//                    true,
//                    false) match {
//                    case Ok(x) => x
//                    case Err(x) => {
//                      throw CompileErrorExceptionT(CouldntEvaluatImpl(List(range), x))
//                    }
//                  }
                // Now we have the `Raza<moo$1, moo$0>`, so we can try to resolve that `moo(myBike)`,
                // in other words look for a `moo(&Raza<moo$1, moo$0>)`.
                // This is also important for getting the instantiation bounds for that particular invocation,
                // so that the monomorphizer can later know how to properly convey the abstract function's
                // bounds (such as a drop(T)void) down to the override's bounds.
//                val abstractFuncEnvWithImplBounds =
//                  GeneralEnvironment.childOf(
//                    interner,
//                    abstractFuncInnerEnv,
//                    abstractFuncInnerEnv.fullName,
//                    Vector())
//                    abstractFuncInferences.map({ case (nameS, templata) =>
//                      interner.intern(RuneNameT((nameS))) -> TemplataEnvEntry(templata)
//                    }).toVector ++
//                      overridingKindReachableBounds.zipWithIndex.map({ case (reachableBound, index) =>
//                        interner.intern(ReachablePrototypeNameT(index)) -> TemplataEnvEntry(reachableBound)
//                      }))

//                val implPlaceholderedOverridingCitizen = overridingImpl.placeholderedSubCitizen
                val overridingParamCoord = abstractParamType.copy(kind = abstractFuncPlaceholderedSubCitizen)
                val overrideFunctionParamTypes =
                  dispatchingAbstractFuncPrototype.prototype.paramTypes
                    .updated(abstractIndex, overridingParamCoord)

                // We need the abstract function's conclusions because it contains knowledge of the
                // existence of certain things like concept functions, see NFIEFRO.
                val foundFunction =
                  resolveOverride(
                    coutputs,
                    List(range, overridingImpl.templata.impl.range),
                    implOverrideCaseEnv,
                    interfaceTemplateFullName,
                    overridingCitizenTemplateFullName,
                    overrideImpreciseName,
                    overrideFunctionParamTypes)
                vassert(coutputs.getInstantiationBounds(foundFunction.function.prototype.fullName).nonEmpty)

                abstractFunctionPrototype.fullName -> foundFunction.function.prototype
              })
            val overridingCitizenFullName = overridingImpl.subCitizen.fullName
            vassert(coutputs.getInstantiationBounds(overridingCitizenFullName).nonEmpty)
            val superInterfaceFullName = overridingImpl.superInterface.fullName
            vassert(coutputs.getInstantiationBounds(superInterfaceFullName).nonEmpty)
            val edge =
              EdgeT(
                overridingImpl.instantiatedFullName,
                overridingCitizenFullName,
                overridingImpl.superInterface.fullName,
                overridingImpl.runeToFuncBound,
                foundFunctions.toMap)
            val overridingCitizenDef = coutputs.lookupCitizen(overridingCitizenTemplateFullName)
            overridingCitizenDef.instantiatedCitizen.fullName -> edge
          }).toMap
        interfaceFullName -> overridingCitizenToFoundFunction
      }).toMap
    (interfaceEdgeBlueprints, itables)
  }

  private def resolveOverride(
      coutputs: CompilerOutputs,
      range: List[RangeS],
      abstractFuncEnv: IEnvironment,
      interface: FullNameT[IInterfaceTemplateNameT],
      overridingCitizen: FullNameT[ICitizenTemplateNameT],
      impreciseName: IImpreciseNameS,
      paramTypes: Vector[CoordT]):
  EvaluateFunctionSuccess = {
    overloadCompiler.findFunction(
      // It's like the abstract function is the one calling the override.
      // This is important so the override can see existing concept functions, see NAFEWRO.
      abstractFuncEnv,
      coutputs,
      range,
      impreciseName,
      Vector.empty,
      Array.empty,
      paramTypes,
      Vector(
        coutputs.getOuterEnvForType(interface),
        coutputs.getOuterEnvForType(overridingCitizen)),
      true,
      true) match {
      case Err(e) => throw CompileErrorExceptionT(CouldntFindOverrideT(range, e))
      case Ok(x) => x
    }
  }

  private def makeInterfaceEdgeBlueprints(coutputs: CompilerOutputs): Vector[InterfaceEdgeBlueprint] = {
    val x1 =
      coutputs.getAllFunctions().flatMap({ case function =>
        function.header.getAbstractInterface match {
          case None => Vector.empty
          case Some(abstractInterface) => {
            val abstractInterfaceTemplate =
              TemplataCompiler.getInterfaceTemplate(abstractInterface.fullName)
            Vector(abstractInterfaceTemplate -> function)
          }
        }
      })
    val x2 = x1.groupBy(_._1)
    val x3 = x2.mapValues(_.map(_._2))
    val x4 =
      x3.map({ case (interfaceTemplateFullName, functions) =>
        // Sort so that the interface's internal methods are first and in the same order
        // they were declared in. It feels right, and vivem also depends on it
        // when it calls array generators/consumers' first method.
        val interfaceDef = coutputs.getAllInterfaces().find(_.templateName == interfaceTemplateFullName).get
        // Make sure `functions` has everything that the interface def wanted.
        vassert(
          (interfaceDef.internalMethods.toSet --
            functions.map(func => func.header.toPrototype -> vassertSome(func.header.getVirtualIndex)).toSet)
            .isEmpty)
        // Move all the internal methods to the front.
        val orderedMethods =
          interfaceDef.internalMethods ++
            functions.map(_.header)
              .filter(x => !interfaceDef.internalMethods.exists(y => y._1.toSignature == x.toSignature))
              .map(header => header.toPrototype -> vassertSome(header.getVirtualIndex))
        (interfaceTemplateFullName -> orderedMethods)
      })
    val abstractFunctionHeadersByInterfaceTemplateFullNameWithoutEmpties = x4
    // Some interfaces would be empty and they wouldn't be in
    // abstractFunctionsByInterfaceWithoutEmpties, so we add them here.
    val abstractFunctionHeadersByInterfaceTemplateFullName =
      abstractFunctionHeadersByInterfaceTemplateFullNameWithoutEmpties ++
        coutputs.getAllInterfaces().map({ case i =>
          (i.templateName -> abstractFunctionHeadersByInterfaceTemplateFullNameWithoutEmpties.getOrElse(i.templateName, Set()))
        })

    val interfaceEdgeBlueprints =
      abstractFunctionHeadersByInterfaceTemplateFullName
        .map({ case (interfaceTemplateFullName, functionHeaders2) =>
          InterfaceEdgeBlueprint(
            coutputs.lookupInterface(interfaceTemplateFullName).instantiatedInterface.fullName,
            // This is where they're given order and get an implied index
            functionHeaders2.toVector)
        })
    interfaceEdgeBlueprints.toVector
  }

  def createOverridePlaceholder(
    coutputs: CompilerOutputs,
    implOverrideEnv: IEnvironment,
    index: Int):
  FullNameT[PlaceholderNameT] = {
    // Need New Special Placeholders for Abstract Function Override Case (NNSPAFOC)
    //
    // One would think that we could just conjure up some placeholders under the abstract
    // function's name, like:
    //
    //   val placeholderName =
    //     PlaceholderNameT(PlaceholderTemplateNameT(placeholderToSubstitution.size))
    //   val placeholderFullName =
    //     FullNameT(packageCoord, abstractFuncTemplateFullName.steps, placeholderName)
    //
    // It would even mostly work, because the abstract function was already compiled, already
    // made a bunch of placeholders, and registered them and their envs with the coutputs so
    // we can just reuse them.
    //
    // Alas, not so simple, because of the Milano case. Those god damned Milanos.
    //
    // Recall this line:
    //
    //   <ZZ> milano &Milano<X, Y, Z, ZZ> => moo(milano, bork)
    //
    // We're actually introducing a fourth placeholder, one that doesn't really refer to a
    // generic arg of the abstract function. This is a moo$3, and moo only had generic args
    // moo$0-moo$2.
    //
    // So, we need to conjure an entirely new placeholder.
    //
    // And of course, since this might happen multiple times (for multiple impls), and we
    // don't want to double-register anything with the coutputs. To get around that, we're
    // just going to make entirely new placeholders every time.
    //
    // For that, we need a unique name, so we'll put the impl's name inside the placeholder's
    // name. The placeholder's full name will be the abstract function's name, then a step
    // containing the impl's name, and then the placeholder name.
    //
    // To be consistent, we'll do this for every placeholder, not just the extra one like ZZ.

    val placeholderName =
      interner.intern(PlaceholderNameT(
        interner.intern(PlaceholderTemplateNameT(index))))
    val placeholderFullName = implOverrideEnv.fullName.addStep(placeholderName)
    // And, because it's new, we need to declare it and its environment.
    val placeholderTemplateFullName =
      TemplataCompiler.getPlaceholderTemplate(placeholderFullName)
    coutputs.declareType(placeholderTemplateFullName)
    coutputs.declareTypeOuterEnv(
      placeholderTemplateFullName,
      GeneralEnvironment.childOf(interner, implOverrideEnv, placeholderTemplateFullName))
    placeholderFullName
  }
}
