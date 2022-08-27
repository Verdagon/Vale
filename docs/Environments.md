
## Interfaces Must Remember Functions Declared Inside (IMRFDI)

For the like rule, we need to know, at rule-time, all of the functions
for a given interface. This is difficult because we also want to be able
to declare abstract functions outside the interface, such as:

```
def lookup(citizenRef: abstract CitizenRef2): CitizenDefinition2 = {
  if :StructRef2 => lookupStruct(citizenRef)
  if :InterfaceRef2 => lookupInterface(citizenRef)
}
```

which actually makes two overrides. (Let's also keep in mind that this
kind of external abstract function can only be used for sealed
interfaces.)

Option A: Make two kinds of interfaces: traits and concepts. A
concept's functions must all be inside its declaration.

```
concept IRulexTR {
  fn resultType() ITemplataType;
}
```

This has a high cost of introducing a new keyword and idea to the user.

Option B: When used like a concept, only the functions inside the
interface's namespace are considered part of the contract. This will
inflict spooky-action-at-a-distance on us; an abstract function way over
there will affect users way over here.

Option C: When used like a concept, only the functions inside its
declaration are considered part of the contract.

```
interface IRulexTR {

fn resultType() ITemplataType;

}

fn eval(state: &!State, rule: abstract &IRulexTR)
IEvalResult<ITemplata> {
  if :EqualsTR => evaluateEqualsRule(state, r)
  if :ConformsTR => evaluateConformsRule(state, r)
  if :OrTR => evaluateOrRule(state, r)
  if :ComponentsTR => evaluateComponentsRule(state, r)
  if :TemplexTR => evaluateTemplex(state, templex)
  if :CallTR => evaluateRuleCall(state, r)
}
```

We're going with C.

Note from later: we might need different entire mechanisms. There's no
way to have an interface express that we want this:

```
concept Printer {
  fn __call(x: #X) Str;
}
```

Another note from later: We might need these internal methods anyway.
When we're conforming a closure to an interface (or if we want to feed
multiple closures into an anonymous subclass) we need a defined ordering
to the methods of the thing we're creating. For that, we need internal
methods.

Also, we cant just put FunctionA's in there, we need to somehow see
them from the global environment too to enable UFCS.



# Compilation Order of Denizens, Macros, Environments (CODME)

We need a way to add new names to an environment.

Those names need to be hierarchical in some way, to avoid name collisions. For example, we might have an interface Ship, and have a macro generate a ShipAnonSubstruct, and have some macros generate free(Ship) and free(ShipAnonSubstruct).

Three ways:
 * Generate children. Like, once we have the Ship, generate a child Ship.ShipAnonSubstruct, and then Ship.free and Ship.ShipAnonSubstruct.Free.
 * Generate siblings. To disambiguate, the last part of the name will have to use wrappers, like Ship(), ShipAnonSubstruct(Ship()), free(Ship()), free(ShipAnonSubstruct(Ship())).
 * A blend of the two.

First seems to make more sense. Also, we already do it plenty; when we make a closure, we add `__call` and `drop` functions to it.

Keep in mind, we need to support a macro adding a virtual free() function for interfaces, because of IMRFDI.

A challenge: If we're adding them to the environment late, how do they get discovered when we try to resolve? Like, how do we find the free() function for ShipAnonSubstruct?

That's not a problem because when we do overload resolution, we look in the environments of all args, so not a problem.

Another challenge: How do we make sure those things get compiled? They aren't really visible from the top level, where we might easily dispatch workers to compile them.

For that, let's have a way to lazily dispatch newcomers and any children to the compilation stage. We kind of already do this, as plenty of folks call FunctionCompiler's compileXYZ methods.

When we get to an environment, let's:

 * Call all contained denizens' macros, they'll generate more denizens, call all their denizens' macros, and so on until we're done.
    * We want to delay compilation of any contained interface until all the macros have a chance to add any virtual methods for it.
 * After all macros are done generating, compile each denizen.
    * Someday, if we want to generate more entries, thats fine, but other denizens probably won't be able to see them. That's kind of what happens with closures too.
 * When we're compiling an interface, look for any virtual functions in the environment. Include them in the internal methods.
 * Someday, if any macros generated any sub-environments, recurse and do all of these steps on that sub-environment.

With this system, something generally won't be able to see the children of siblings. That's not terrible, that's how it generally works in other languages.
