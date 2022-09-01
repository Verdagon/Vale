package dev.vale.typing

//import dev.vale.astronomer.{GlobalFunctionFamilyNameS, INameS, INameA, ImmConcreteDestructorImpreciseNameA, ImmConcreteDestructorNameA, ImmInterfaceDestructorImpreciseNameS}
//import dev.vale.astronomer.VirtualFreeImpreciseNameS
import dev.vale.{Err, Interner, Keywords, Ok, RangeS, vassert, vassertSome, vcurious, vfail, vimpl, vwat}
import dev.vale.postparsing.{GlobalFunctionFamilyNameS, IImpreciseNameS, RuneNameS}
import dev.vale.typing.ast.{InterfaceEdgeBlueprint, PrototypeT}
import dev.vale.typing.env.{GeneralEnvironment, IEnvironment, TemplataEnvEntry, TemplataLookupContext, TemplatasStore}
import dev.vale.typing.types._
import dev.vale.typing.ast._
import dev.vale.typing.citizen.ImplCompiler
import dev.vale.typing.function.FunctionCompiler
import dev.vale.typing.function.FunctionCompiler.{EvaluateFunctionFailure, EvaluateFunctionSuccess}
import dev.vale.typing.names.{FullNameT, ICitizenNameT, ICitizenTemplateNameT, IInterfaceNameT, IInterfaceTemplateNameT, InterfaceTemplateNameT, ReachablePrototypeNameT, RuneNameT, StructTemplateNameT}
import dev.vale.typing.templata.{FunctionTemplata, KindTemplata}
import dev.vale.typing.types._

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
            val overridingCitizenTemplateFullName = overridingImpl.subCitizenTemplateName
            val superInterfaceWithSubCitizenPlaceholders = overridingImpl.parentInterfaceFromPlaceholderedSubCitizen

            // Given:
            //   impl<T> IObserver<T, int> for MyThing<T>
            // Start by compiling the impl, supplying any placeholders.
            //   impl<impl$T> IObserver<impl$T, int> for MyThing<impl$T>
            // Now, take the abstract function:
            //   abstract func myFunc<X, Y>(self IObserver<X, Y>, bork Y);
            // and try to evaluate it given the impl's interface (IObserver<impl$T, int>)
            // as the first parameter:
            //   abstract func myFunc(self IObserver<impl$T, int>, bork int);
            // This is so we fill the other params' types, like that bork int.
            // Now, try to resolve an overload with the impl's struct there instead:
            //   func myFunc(self MyThing<impl$T>, bork int)
            // and sure enough, we find the override func:
            //   func myFunc<T>(self MyThing<T>, bork int)
            // (and also conclude that its T = impl$T).
            // Now we can add that to the impl's edge.
            //
            // All this is done from the impl's perspective, the impl is the original calling
            // env for all these solves and resolves.

            val foundFunctions =
              interfaceEdgeBlueprint.superFamilyRootHeaders.map(abstractFunctionHeader => {
                val abstractFunctionParamUnsubstitutedTypes = abstractFunctionHeader.paramTypes
                val abstractIndex = abstractFunctionHeader.params.indexWhere(_.virtuality.nonEmpty)
                vassert(abstractIndex >= 0)
                val abstractParamUnsubstitutedType = abstractFunctionParamUnsubstitutedTypes(abstractIndex)
                val abstractParamType =
                  abstractParamUnsubstitutedType.copy(kind = superInterfaceWithSubCitizenPlaceholders)

                val range = abstractFunctionHeader.maybeOriginFunctionTemplata.map(_.function.range).getOrElse(RangeS.internal(interner, -2976395))

                val originFunctionTemplata = vassertSome(abstractFunctionHeader.maybeOriginFunctionTemplata)

                // Evaluate the function as if we're defining it, even using the definition site rules.
                // We'll even take the inferences and add em to an environment later.
                // The only difference from real defining is that we're handing in an actual parameter,
                // namely the impl's super interface.
                val EvaluateFunctionSuccess(abstractFuncPrototype, abstractFuncInferences) =
                  functionCompiler.evaluateGenericLightFunctionParentForPrototype(
                      coutputs,
                      List(range, overridingImpl.templata.impl.range),
                      overridingImpl.implOuterEnv,
                      originFunctionTemplata,
                      abstractFunctionHeader.paramTypes.indices.map(_ => None)
                        .updated(abstractIndex, Some(abstractParamType))
                        .toVector) match {
                    case EvaluateFunctionFailure(x) => {
                      throw CompileErrorExceptionT(CouldntEvaluateFunction(List(range), x))
                    }
                    case efs @ EvaluateFunctionSuccess(_, _) => efs
                  }

                // If we have a:
                //   impl<T, Z> MyShipOrBike<int, T, Z> for MyShip<T, Z>;
                // This will be the MyShipOrBike<int, T, Z>.
                // We'll use it below to make an abstract function.
                val interfaceTypeForThisImpl =
                  abstractFuncPrototype.prototype.paramTypes(
                    vassertSome(abstractFunctionHeader.getVirtualIndex)).kind.expectInterface()
                strt here
                // this is phrased in terms of the impl. makes sense, because we fed in abstractFunctionType
                // which was phrased in terms of the original impl.
                // now we need to combine this with the abstract function as if it's taking one of these in...

//
//                val superFunctionParamTypes = abstractFuncPrototype.prototype.paramTypes
//
//                val implPlaceholderedOverridingCitizen = overridingImpl.placeholderedSubCitizen
//                val overridingParamCoord = abstractParamType.copy(kind = implPlaceholderedOverridingCitizen)
//                val overrideFunctionParamTypes =
//                  superFunctionParamTypes.updated(abstractIndex, overridingParamCoord)
//
                val impreciseName =
                  vassertSome(
                    TemplatasStore.getImpreciseName(
                      interner, abstractFunctionHeader.fullName.last))
//
//                // See ONBIFS and NBIFPR for why we need these bounds in our below env.
//                val overridingKindReachableBounds =
//                  TemplataCompiler.getReachableBounds(
//                    interner, keywords, coutputs, KindTemplata(implPlaceholderedOverridingCitizen))

                val abstractFuncInnerEnv =
                  coutputs.getInnerEnvForFunction(abstractFunctionHeader.fullName)

                // We'll solve the impl given the placeholdered super interface.
                // So if we have an abstract function:
                //   func moo<int, T>(virtual a MyShipOrBike<int, T>);
                // Imagine this body:
                //   func moo<int, T, Z>(virtual self &MyShipOrBike<int, T, Z>) {
                //     self match {
                //       myShip MyShip<int, T, Z> => moo(myShip)
                //       myBike MyBike<int, Z, T> => moo(myBike)
                //     }
                //   }
                // Imagine we're actually compiling it, which means we have placeholders:
                //   func moo<$0, $1>(virtual self &MyShipOrBike<$0, $1>) {
                //     self match {
                //       myShip &MyShip<$0, $1> => moo(myShip)
                //       myBike &MyBike<$1, $0> => moo(myBike)
                //     }
                //   }
                // Right here, we're trying to resolve an override function, eg `moo(myBike)`.
                // First, we need to figure out `MyBike<$1, $0>`. That's what this impl solve is
                // doing. It's feeding the MyShipOrBike<$0, $1> into the impl:
                //   impl<T, Z> MyShipOrBike<T, Z> for MyBike<Z, T>;
                // roughly solving to:
                //   impl<moo$0, moo$1> MyShipOrBike<moo$0, moo$1> for MyBike<moo$1, moo$0>;
                // to get the `MyBike<moo$1, moo$0>`.
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
                val subCitizenFromAbstractFuncPerspective =
                  implCompiler.getImplDescendantGivenParent(
                    coutputs,
                    List(range),
                    abstractFuncInnerEnv,
                    overridingImpl.templata,
                    interfaceTypeForThisImpl,
                    true,
                    false) match {
                    case Ok(x) => x
                    case Err(x) => {
                      throw CompileErrorExceptionT(CouldntEvaluatImpl(List(range), x))
                    }
                  }
                // Now we have the `MyBike<moo$1, moo$0>`, so we can try to resolve that `moo(myBike)`,
                // in other words look for a `moo(&MyBike<moo$1, moo$0>)`.
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
                val overridingParamCoord = abstractParamType.copy(kind = subCitizenFromAbstractFuncPerspective)
                val overrideFunctionParamTypes =
                  abstractFuncPrototype.prototype.paramTypes
                    .updated(abstractIndex, overridingParamCoord)

                // We need the abstract function's conclusions because it contains knowledge of the
                // existence of certain things like concept functions, see NFIEFRO.
                val foundFunction =
                  resolveOverride(
                    coutputs,
                    List(range, overridingImpl.templata.impl.range),
                    abstractFuncInnerEnv,
                    interfaceTemplateFullName,
                    overridingCitizenTemplateFullName,
                    impreciseName,
                    overrideFunctionParamTypes)
                vassert(coutputs.getInstantiationBounds(foundFunction.function.prototype.fullName).nonEmpty)

                val abstractFuncTemplateFullName =
                  TemplataCompiler.getFunctionTemplate(abstractFuncPrototype.prototype.fullName)
                abstractFuncTemplateFullName -> foundFunction.function.prototype
              })
            val overridingCitizenFullName = overridingImpl.placeholderedSubCitizen.fullName
            vassert(coutputs.getInstantiationBounds(overridingCitizenFullName).nonEmpty)
            val superInterfaceFullName = overridingImpl.parentInterfaceFromPlaceholderedSubCitizen.fullName
            vassert(coutputs.getInstantiationBounds(superInterfaceFullName).nonEmpty)
            val edge =
              EdgeT(
                overridingImpl.instantiatedFullName,
                overridingCitizenFullName,
                overridingImpl.parentInterfaceFromPlaceholderedSubCitizen.fullName,
                overridingImpl.runeToFuncBound,
                foundFunctions.toMap)
            overridingCitizenFullName -> edge
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
        vassert((interfaceDef.internalMethods.toSet -- functions.map(_.header).toSet).isEmpty)
        // Move all the internal methods to the front.
        val orderedMethods =
          interfaceDef.internalMethods ++
            functions.map(_.header).filter(x => {
              !interfaceDef.internalMethods.contains(x)//exists(y => y.toSignature == x.toSignature)
            })
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
}
