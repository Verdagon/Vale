
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

Another note from later: We might need these internal methods anyway. When we're conforming a closure to an interface (or if we want to feed multiple closures into an anonymous subclass) we need a defined ordering to the methods of the thing we're creating. For that, we need internal methods.

Also, we cant just put FunctionA's in there, we need to somehow see them from the global environment too to enable UFCS.

There are also macros that want to add interface methods (InterfaceFreeMacro adds a virtual free function). We collect those to be part of the internal methods during compileInterface. It knows how to find them because their FullNames are prefixed by the interface, which fits well. (See CODME also)


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

 1. Call all contained denizens' macros, they'll generate more denizens, call all their denizens' macros, and so on until we're done.
    * We want to delay compilation of any contained interface until all the macros have a chance to add any virtual methods for it.
    * We also want to delay compilation of any siblings until then too, because they might want to other things in the same environment.
 2. After all macros are done generating, assemble the IEnvironment. **This is the "outer env".**
 3. Compile each denizen, giving it the outer env.
    * Someday, if we want to generate more entries, thats fine, but other denizens probably won't be able to see them. That's kind of what happens with closures too.
 3. When we're compiling an interface, look for any virtual functions in the environment. Include them in the internal methods.
 4. Someday, if any macros generated any sub-environments, recurse and do all of these steps on that sub-environment.

We'll want to call the macros on all public global-scoped denizens before compiling anything, because when we compile things, they'll want to access them. In other words, do step 1 on *all* global scope environments first.

With this system, something generally won't be able to see the children of siblings. That's not terrible, that's how it generally works in other languages.

There's still one flaw here: we'll be compiling some functions before some structs. For example, MyList contains a MyOption. MyList's child function drop(MyList) function wants to call drop(MyOption). If we compile MyList (and all children, including drop(MyList)) before MyOption, then it won't be able to see it. This is the original reason we compiled all structs and interfaces before all functions.

But that seems to conflict with a requirement of interfaces, which is to have headers for all methods before it's finished compiling.

We can resolve by delaying compilation of any function *bodies.* Interfaces only need the headers, not the bodies.


# Resolving Things In Templated Namespaces (RTITN)

DO NOT SUBMIT

```
struct Vec<T> {
  func new() Vec<T> { }
}
exported func main() {
  v = Vec<int>.new();
}
```

new is actually secretly `new<T>` because inner functions like these pull the generic parameters from their parents.

A: Patch the environment so that it doesn't have placeholders.

Could even make it a new child environment.

This should be done at the initial solve place, when we have the values for *all* templatas.

Pros:

 * It conceptually fits with what an environment really is.

Cons:

 * It means placeholders can be seen if there's a bug.

need to do an assembleKnownTemplatas call or something like it, when we make the globalfunctiongroup.



B: Create new specific environment

We'd remember the inferences from when we resolved the struct. We'd make a new environment with those and all the functions from the struct definition's environment.

It's like A but an entirely new environment, no risk of placeholders in the parent envs getting out.

Cons:

 * Seems a bit complicated and expensive to make entirely new environments.


C: Send an Id which has all the identifying runes filled out.

Pros:

 * Makes a nice separation between call-site and definition environment

Cons:

 * We'll have to re-solve for all the other runes.
    * Or will we? Won't we be just doing a new solve for the function?
 * Have to pipe that through everywhere




If we have this:

```
extern struct Vec<T> imm {
  extern func with_capacity(c i64) Vec<T>;
  extern func capacity(v Vec<T>) i64;
}
```

We generally don't want these:

`Vec.capacity<int>(v Vec<int>)`

because they confuse the instantiator, because all struct internal methods now have names like this:

`Vec<T>.capacity<T>(v Vec<T>)`

But that leads to an interesting conundrum.

When we're resolving the function, the function resolving code sees the function in the struct's outer environment (the one that doesn't have any placeholders or anything).

But the struct's outer environment simply has the name `Vec`, not `Vec<T>`, so when we instantiate the resolved function's name, it comes out like:

`Vec.capacity<int>(v Vec<int>)`

which confuses the instantiator.

In other words, the function resolving logic needs to output the same shape of name as the function definition, so it doesn't confuse the instantiator.

Our options are limited. We need the name to end up like this:

`Vec<T>.capacity<T>(v Vec<T>)`

so that we can successfully interoperate with Rust, which can put methods inside a struct's inner namespace.

So the only option seems to be to make the function resolving code output this:

`Vec<int>.capacity<int>(v Vec<int>)`

But how do we do that?

There doesn't seem to be anything at the resolving site that could help us here.

Perhaps we can say that if a parameter is named `self`, then the function will be liftable and that will be the parameter to bring into the final name.

So this:

`Vec.capacity<int>(self Vec<int>)`

will be adjusted to this:

`Vec<int>.capacity<int>(self Vec<int>)`

This is also nice because it removes some of the complex logic for determining whether a function can be lifted (previously we looked to see if the parent struct could be determined via parameters).


