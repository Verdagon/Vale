
# Basic Concepts

Back when we did templates, every time we used (resolved) a struct, we would lazily compile it for that set of template args, unless we've seen that combination before.

Now, we **compile** them all ahead of time, outside of any particular use.

However, sometimes during the compilation, we'll be resolving _other_ templates. Those may or may not have been compiled yet.

...we may need to do a declare phase so we can populate the overload index one day.


# Some Rules Only Apply to Call Site or Definition (SROACSD)

This snippet is using a function bound:

```
func launch<X>(x X)
where func foo(int)void {
  ...
}
```

When we do a `func foo(int)void` rule, that's actually making three rules under the hood.

The first one is a `DefinitionFunc` rule, and it just generates a `Prototype` for its result rune, and puts it in the env. That way, the body can call that function. **This rule is used when compiling the function itself, it's not used at the call site.**

In the call site, instead we have a **Resolve** rule which looks in the current environment and grabs the actual `Prototype` that matches, and we have a **CallSiteFunc** rule which checks that its params and returns match what we expect. **These rules are used at the call site, but not used when compiling the function itself.**

In other words, some rules only apply to the call site, and some rules only apply when compiling the definition. There's some filtering that makes this happen.


# Some Rules are Hoisted Out of Default Param (SRHODP)

This snippet is using a **function bound generic parameter**:

```
func launch<X, func foo(int)void>(x X)
where  {
  ...
}
```

It's particularly nice because the call-site can hand in something that's not called `foo`.

This `func foo(int)void` generates three rules (see SROACSD):

 * DefinitionFunc, used when compiling definition, creates a prototype that can later be called.
 * Resolve, which looks in the current environment for a matching function.
 * CallSiteFunc, used when compiling call site, which makes sure the given prototype has the right parameters and returns.

If the call-site wants to pass in their own prototype, then they *don't* want that Resolve rule in there. So, we need Resolve only be a *default* expression, only run when the user doesn't specify something.

But we don't want the other two rules (DefinitionFunc, CallSiteFunc) to be defaults, we want those to always be there. So, we'll hoist them out of the generic parameter's "default rules" and into the function's main rules.



# Call Site Solving Needs Caller Env (CSSNCE)

We have functions like:

```
func xoo<X>(x X) void
where func drop(X)void
{
  zoo(x);
}

func zoo<Z>(z Z) void
where func drop(Z)void
{
  ...
}
```

We're trying to solve `zoo(x)`. `zoo` requires a `func drop(Z)void` but that only exists in `xoo`'s environment.

So, the callee needs access to the caller's environment.



# Default Parameters Can Only Depend on Other Default Parameters (DPCODODP)

We had:

```
struct Functor1<F Prot = func(P1)R> imm
where P1 Ref, R Ref { }
```

But when defining it it had no idea what to do. It should have generated a DefinitionCallSR which would produce the right prototype, but it didn't know what coords to use for its param and return.

I believe this means that we should have had some placeholders for the P1 and R.

That then means that P1 and R should have been generic params themselves.

So, it should be like this:

```
struct Functor1<P1 Ref, R Ref, F Prot = func(P1)R> imm { }
```

And then we should make placeholders for P1 and R, and let the 3rd param's DefinitionCallSR create a prototype using those two. Then things would work.


# Only Work with Placeholders From the Root Denizen (OWPFRD)

Let's say we're in this function:

```
struct SomeStruct<X> {
  x X;
}
func genFunc<T>() {
  thing = SomeStruct<int>(7);
  z = thing.x;
  println(z);
}
```

The type of `z` is `int`, of course.

`SomeStruct.x` (of the template) is of type SomeStruct$0. We did a substitution to go from SomeStruct$0 to int.

If we don't do that substitution, then a SomeStruct$0 creeps into our `genFunc` and wreaks absolute havoc, because then we're likely confusing it with genFunc's own placeholder, `genFunc$0`.

This is the reason we prefix placeholders with the name of their container (both in FullNameT and here in the docs when talking about them).

We also have a sanity check in the solver to make sure that we're never dealing with foreign placeholders, search for OWPFRD.


# Struct Can Impl Interface Multiple Times (SCIIMT)

A struct might implement an interface in multiple ways.

For example `MyController<T>` might implement `IObserver<SignalA>` and `IObserver<SignalB>`, so there would be two ImplT's for it:

 * `ImplT(MyController, MyController<Placeholder(0)>, IObserver, IObserver<SignalA>)`
 * `ImplT(MyController, MyController<Placeholder(0)>, IObserver, IObserver<SignalB>)`


## Must Look Up Impls By Template Name (MLUIBTN)

The above (SCIIMT) also means that one cannot just look for `ImplT(_, _, _, IObserver<Placeholder(0)>`, as there are none in that table.

They should instead search by the template name, like `ImplT(_, _, IObserver, _)`.


# Structs Member Rules Are Skipped During Resolving (SMRASDR)

When we have a recursive type like this:

```
struct ListNode<T> {
  val T;
  next Opt<ListNode<T>>;
}
```

when we try to resolve the `Opt<ListNode<T>>` it will try to resolve the `ListNode<T>` which runs all these rules _again_, and goes into an infinite loop.

The answer is to only run the innards when compiling the definition, not when we're resolving.



# Compile Impl From Both Directions (CIFBD)

NOTE: Not sure this is true anymore, ever since generics.

We previously compiled all impls for a given struct. We did this for each struct.

However, this would miss some things. For example, if we had this interface:

```
sealed interface MySerenityOrRazaUnion { }
impl Serenity for MySerenityOrRazaUnion { }
impl Raza for MySerenityOrRazaUnion { }
```

and it was in some hidden leaf dependency, not seen by Serenity or Raza, then it would be missed.

For this reason, we also compile all impls for a given interface.

This *could* result in collisions. For example:

```
interface MyInterface { }
struct MyStruct { }
impl MyStruct for MyInterface { }
```

Compiling this from both directions will result in the same impl.

That's fine, the CompilerOutputs class will deduplicate them.


# Require Explicit Multiple Upcasting to Indirect Descendants and Ancestors (REMUIDDA)

If we have a Serenity which impls IFirefly which impls IShip:

```
interface IShip { }

interface IFirefly { }
impl IFirefly for IShip { }

struct Serenity { }
impl Serenity for IFirefly { }
```

Then we can't directly upcast a Serenity to an IShip, like:

```
ship IShip = Serenity();
```

We'll have to upcast it to an IFirefly first:

```
ship IShip = Serenity.as<IFirefly>();
```

This is just to save some compile speed. This way, we can just index the direct parents and children of structs and interfaces, and don't have to do any transitive calculations, for example to find out if a struct indirectly implements an interface.

This will likely also save us some space and complexity in the vtables; each vtable won't need *all* the descendants and ancestors.



# Don't Use Default Expression When Compiling Denizen (DUDEWCD)

When compiling definitions, we need to always populate placeholders for every argument, and never use default expressions.

Let's say we have:

```
struct Thing<N Int = 5> {
  vec Vec<N, Float>;
}
```

When we compile the innards of that, we don't want to assume that N is 5, because it could be anything.

So, we need to *not* execute that LiteralSR(N, 5) rule.


# Using Instantiated Names in Templar (UINIT)


This one little name field can illuminate much of how the compiler works.

For ordinary functions, each ordinary FunctionA becomes one ordinary FunctionT/FunctionHeaderT.

 * This IFunctionNameT's parameters have the types you'd expect
 * This IFunctionNameT will have no templateArgs.

For generic functions, each generic FunctionA becomes one ordinary FunctionT/FunctionHeaderT.

 * This IFunctionNameT's parameters will have some PlaceholderTs or PlaceholderTemplatas in them.
 * The templateArgs will all PlaceholderTemplatas.

Lambdas are where it gets interesting. One lambda can manifest multiple FunctionTs/FunctionHeaderTs. For example:

```
lam = x => println(x);
lam(3);
lam(true);
```

  This will actually manifest *two* FunctionTs/FunctionHeaderTs:

 * One for line 2, with one parameter (int) and one template arg (int).
 * One for line 3, with one parameter (bool) and one template arg (bool).

We also use this same scheme for the CompilerOutputs, to map names to environments.

# Need Abstract Function's Environment When Resolving Overload (NAFEWRO)

Notice how both the abstract function and the override function have a bound:

```
abstract func drop<T>(virtual opt Opt<T>)
where func drop(T)void;

func drop<T>(opt Some<T>)
where func drop(T)void
{
  [x] = opt;
}
```

We eventually get to the late stages of the compiler, looking for overrides for that abstract `drop`. When we try to find one via OverloadCompiler, we have trouble evaluating the candidate (the second `drop` there) because there's no way for it to know if there actually exists a drop function.

We need some way to convey the promise from the abstract function down to the override function.

So, when we resolve that override, we include the environment from the abstract function, so the override can find the guarantee that there is a drop for T.


# Concept Functions With Generics (CFWG)

Our current concept functions don't really work with default generic parameters that well.

Here's how it should work in a post-generics world.



## Prototype-Based Concept Functions

```
struct Functor1<P1 Ref, R Ref, F Prot = func moo(P1)R> {
}
```

No need of placeholders. We just go in and start figuring shit out.

We start solving. We never know P1, R, or F, but we at least know that we can call a function named moo on things of those types.

func moo(P1)R becomes:

 * ResolveSR, which looks through the overload indexes for a single `moo` that fits the name and param requirements, even though we dont fully know the requirements yet.
 * CallSiteFuncSR, which grabs its return type and equates it
 * DefinitionSiteFuncSR, which just puts into the env the knowledge that this is a call that can be made.



## Placeholder-Based Concept Functions

```
struct Functor1<P1 Ref, R Ref, F Ref> {
  functor F = func moo(P1)R;
}
```

```
func Functor1<P1, R, F>(functor F = func moo(P1)R) Functor1<P1, R, F> {
}
```

func moo(P1)R becomes:

 * ResolveSR(X Ref, moo, P1, R);
   runs when P1 and R are defined.
   it will create an OverloadSet pointing at moo and put it in X.
   the templex produces X ref as the result rune, so it ends up equal to F.
 * DefinitionFuncSR(Z Prot, X, P1, R);
   runs when X, P1, and R are all defined.
   X is fed in as a placeholder, remember.
   which puts the prototype Z into the environment to establish that X is callable.
 * CallsiteFuncSR(Z Prot, X Ref, P1, R)
   runs when X, P1, and R are all defined.
   X will be an overload set from resolve-func, or a `__call`able type from user.
   it makes sure that there is indeed a `__call` prototype taking X and P1 and returns R.
   it assigns it into Z. it doesnt _really_ have to, but it means Z is always defined which is simpler.


sucks that we need a functor. but honestly, that comes from this weird placeholder crap. if we didnt need placeholders, we could do this easier.



# Require Rune for Function Bound?

In this example:

```
func moo(i int, b bool) str { return "hello"; }

exported func main() str
where func moo(int, bool)str
{
  return moo(5, true);
}
```

It's ambiguous which moo we're referring to. Which one should we use?

We could say that since it's ambiguous, they should stuff it into a rune and then call the rune directly... but this feels like it would be fragile.








Variable SSA vs Final SSA

It might be difficult to have parameterized variability.

when we make SSAs, they need to have a variability, right there in the kind.

we could have a PlaceholderVariability? and then not allow sets on it.


we'll likely run into the same problem with the size. StaticSizedArrayT holds an integer, not really a placeholder.


we could have an ITemplata[+ITemplataType] perhaps?
nope. theres more than just 9 templata types, theyre infinite because pack and template.
cant constrain something like that.
well, we can at least constrain on those top 9 types. should be good enough.


but wait, we have a problem now. coords need to be able to have placeholder kinds in them.
 * could we make locals contain itemplatas?
 * struct fields too maybe?
 * function parameters?
anything.
at this point, theres not much difference between KindT and a Placeholder[KindTemplataType].

the basic problem is that basically templatas can be (and contain) coords and kinds, and kinds can contain templatas now.

well they arent really templatas. theyre Variability|Placeholder, Ownership|Placeholder, Coord|Placeholder. also, structs have a list of templatas already.

we need to associate envs with certain templatas then. wtf?



# A: PlaceholderKind for kind and coord, PlaceholderOr[ITemplata] for all else

Locals, members, and parameters would need to be this.

perhaps this could be a good stepping stone to full rules?

yes, and coord and kind happen to already kind of do this on their own, which is nice.

this is a good stepping stone to full rules. after this, we:

 * replace all of these with IRuneT, and get rid of PlaceholderKind.
 * have a table of values, basically ITemplata. look up that IRuneT in that table. if its not present, its a placeholder. if its there, its a value


# B: ITemplata[+T <: ITemplataType]

Locals, members, parameters would need to change to this. everything would.

it could be a stepping stone to full rules. we'd do the same thing as A.

this is just more intensive right now.


# C: PlaceholderKind for kind and coord, ITemplata[+T <: ITemplataType] for all else

This is honestly equivalent to A.

Not a fan because there's kind of an overlap, `ITemplata[CoordTemplataType]` and `ITemplata[KindTemplataType]` don't make sense.


# All-in on Runes

we could have everything be a rune, and all constants would be in the env.
in fact, that would make things closer to the rule system.
thats an interesting thought.

oddly enough, we're already kind of there. post-scout, everything is already runes. presumably iexpression would become a bunch of rules? i suppose it doesnt really need to.

pre-solving would basically just try and figure enough out to resolve all overloads and... what else


but itd be nice to not have to embark on this particular journey yet. perhaps there's an easy first step that will unblock regions?



steps:
 * ??
 * Make structref itemplatas, interfaceref itemplatas, locals, members, parameters, returns all be runes. the current ITemplata will live on in hammer definitely


an itemplata should be like a rune, that may or may not have a value assigned.
so, every itemplata should be either a placeholder or an ITemplataValue or something.

could ITemplataValues have runes? yes.


what does an env follow? it seems they can follow placeholders. no, it shouldnt be associated with a particular usage of the type, should be associated with the type itself. in fact, not really the type, but instead the template.

we dont really need the env to follow the placeholder, because those functions will be in scope anyway.

in the future, we would like to resolve overloads with only partial data. so there should be some notion of a coord whose type isnt really known.

sounds like instead of coords flying around we want ITemplata[CoordTemplataType]s flying around.

but when we figure out a rune, wed have to go in and update all the rune itemplatas to be value templatas. that would be annoying. the reason this works for placeholders is that we know they wont be resolved. so we need a table eventually. but perhaps not while we're just doing placeholders.


# Hardcode Zero, empty string, Final, mutable?

hardcoding mutable might be weird.

it feels like this will head straight into trouble.


# IntTemplata contains Placeholder|int, BoolTemplata contains Placeholder|bool etc.

Just like we're doing with kind and coord and stuff.

perhaps this could be a good stepping stone to full rules?

after this, an ITemplata would basically become an IRuneS. it might point to a rule that has a constant or not. after pre-solving, it might have an inference or not.

this would evolve into IntTemplata becoming IntRuneT, BoolTemplata becoming BoolRuneT, etc.


# PlaceholderInt, PlaceholderKind, etc. all subclasses of ITemplata

Locals, members, and parameters would need to be ITemplata


# IGenericData: VariableGenericData or LiteralGenericData

Locals, members, and parameters would need to be this.


# PlaceholderT kind, ITemplata/PlaceholderTemplata subclasses for everything else?

means locals, members, parameters wouldnt need to do anything different.

but StaticSizedArrayT would need to contain e.g. ITemplata[OwnershipTemplata]






















"Tests a templated linked list" test fails
I think it's because interface Opt<T> is trying to define a drop, even though
it has #!DeriveInterfaceDrop. Thats cuz this method is called directly, regardless
of whether the #! thing is there or not.
We need to make it conditionally run.
We cant have this as a child entry because then it will be under the defining environment
of the interface, with all sorts of placeholders and nonsense in it.
Perhaps we can:
 1. just run this from Compiler.scala, if the #! thing isnt there.
 2. make an umbrella environment that has siblings and interface in it, so the interface's
   placeholder runes dont get into it.
Why do we even put that declaring environment anywhere?
Ah, its for overload resolution, so we can see its siblings and parents.
Well then, we dont have to have the runes in there. Why do we put the runes into an environment?
Maybe we can just not do that.
Ok, thats option 3.
 3. Dont put runes in the environment! The solving is only useful for the definition anyway.
actually that might not work. the members (and any child functions one day) will need to see
those runes.
but you know what, lets run the macros and stuff *before* making that environment, and then
declare that env into the coutputs. then after that we can make an environment for the members.
Not sure what we'll do for the functions one day... probably best just syntactically lift them
out?









# Solve First With Predictions, Resolve Later (SFWPRL)

Sometimes, the `func drop(T)void` rule doesn't run soon enough for us. For example:

```
sealed interface Opt<T Ref>
where func drop(T)void
{ }

struct Some<T>
where func drop(T)void
{ x T; }

impl<T> Opt<T> for Some<T>
where func drop(T)void;
```

Here, when we're compiling that impl, two things need to happen:

 * The DefinitionFuncSR puts the `drop` function into the environment.
 * We resolve the `Some<T>`, which requires that a `drop` function exists in the environment.

There are two ways we might solve this:

 * Somehow force the drop rule to run sooner.
 * Force the resolve of the `Some<T>` to happen later.

The former might require some sort of priority mechanism in the solver, so we're doing the latter.

Basically, during a solve, if the solver wants to resolve an interface or struct, it doesn't actually call the StructCompiler's `resolveStruct`/`resolveInterface` which checks all the requirements are there.

Instead, it will call StructCompiler's `predictStruct`/`predictInterface` which do very little, they mostly just create a name, plus solve some default generic parameters.

Later, after the solve, we go back through and do the actual `resolveStruct`/`resolveInterface`.

We do this with functions too. ResolveSR will actually just create a prototype out of thin air. It's only afterward that we actually go and find it. (This is also why ResolveSR needs a return type rune)


## Only See Direct Caller's Environment (OSDCE)

Let's say we have these definitions:

```
interface MyInterface<T>
where func drop(T)void { }

struct MyStruct<T>
where func drop(T)void { ... }

impl MyInterface<T> for MyStruct<T>
where func drop(T)void;
```

Which expands to something like this:

```
#!DeriveInterfaceDrop
interface MyInterface<T>
where func drop(T)void { }

virtual func drop(self MyInterface<T>);

#!DeriveStructDrop
struct MyStruct<T>
where func drop(T)void { ... }

func drop(self MyInterface<T>);

impl MyInterface<T> for MyStruct<T>
where func drop(T)void;
```

This tree of steps happens when we compile the itables:

 * We're looking for all overrides for the abstract function `func drop(self MyInterface<T>)void`.
    * We use the environment from it, **which includes a conjured** `where func drop(T)void` bound.
    * We use a placeholder for `T` (named `drop(MyInterface<T>).$0`, but we'll keep calling it `T`).
    * We see that `MyStruct` implements it, so we try resolving a function `func drop(MyStruct<T>)void`.
       * During solving, we conjure a `MyStruct<T>` and postpone its resolving.
       * We conjure an environment with the conclusions from the solving, **including the conjured** `func drop(MyStruct<T>)void`.
       * Now after solving, we're actually resolving that `MyStruct<T>`.
          * During solving, we conjure a `func drop(T)void` and postpone its resolving.
          * Afer solving, we actually want to resolve that `func drop(T)void`.
             * Uh oh! We find **two** matching prototypes.

It seems that both of them are conjuring a prototype to use and requiring things.

So, we don't send anything downward.


# Solving Then Checking Must Be Different Phases (STCMBDP)

When compiling a definition, we declare that certain concept functions exist.

There's a cyclical dependency here though:

 1. To declare that a function exists, we need to know the actual types of the params and returns.
 2. To know the actual types we're resolving, we need to check their requirements.
 3. To check their requirements, we need to know that a function actualy exists in our scope.

One of those dependencies needs to be changed up.

 1. We might be able to change this if we could make our system operate on partial data.
 2. We can check their requirements later.
 3. This can't be changed.

#2 seems easiest for now.

So, when compiling a denizen, we do all the checking of calls _later_.





# Macro-Derived Sibling Functions Often Need All Rules From Original (MDSFONARFO)

Macros can take a denizen and generate new denizens right next to them.

For example, in:

```
sealed interface Opt<T Ref>
where func drop(T)void
{ }

struct Some<T>
where func drop(T)void
{ x T; }

impl<T> Opt<T> for Some<T>
where func drop(T)void;
```

The implicit InterfaceDropMacro defines a drop function for that interface.

It almost looks like this:

```
func drop(this Opt<T>) void {
  [x] = this;
}
```

But wait! That doesn't work! There needs to be a `<T>` parameter and there needs to exist a drop for that T, like:

```
func drop<T>(this Opt<T>) void
where func drop(T)void {
  [x] = this;
}
```

We can see now that this `drop` function actually takes a _lot_ from the original interface.

It needs:

 * Generic parameters
 * Concept functions
 * Rune types

Pretty much everything.


# Need Function Inner Env For Resolving Overrides (NFIEFRO)

Let's say we have these functions:

```
abstract func drop<T>(x Opt<T>)
where func drop(T)void;

func drop<T>(x Some<T>)
where func drop(T)void {
  [_] = x;
}
```

When we're figuring out the itables, we see the abstract one, and so we try to resolve for the one taking a `Some<T>`.

However, when we do that, it failed the second `where func drop(T)void` there. That's because when the abstract function is "calling" the override, it didn't have the abstract function's original environment which contained the knowledge that there exists a `func drop(T)void`.

So, we need to recall the abstract function's inner environment when we do that resolve. To do that, we need to track the inner env in the CompilerOutputs.



## Must Know Runes From Above

(MKRFA)

When we start evaluating a function or struct or something, we don\'t
know the values of its runes.

For example:

```
fn add<T>(list &List<T>, elem T) { ... }
```

we don't know the value of T, we're figuring it out now.

One would think that whenever we see a CodeRuneS("T"), it's an
unknown.

**Case 1: Manually Specifying a Rune**

If we have this function:

```
fn moo<T>(x T) Some<T> {
  Some<T>(x)
}
```

We need to look up that T from the environment.

For that reason, Scout's IEnvironment will track which runes are
currently known.

**Case 2: Runes from Parent**

If we have this `__call` function:

```
interface IFunction1<M, P1 Ref, R Ref> M {
  fn __call(virtual this &IFunction1<M, P1, R>, param P1) R;
}
```

If we want to evaluate it, we can actually infer the M, P1, and R from
the first argument. So they can be regular runes.

**Conclusion**

The PostParser will keep track of what runes are defined in parent environments. When it encounters one, it'll know it's a rune, and add a RuneParentEnvLookupSR rule for it. For Case 1, we'll just leave it in. For Case 2, we'll immediately strip it out.

When compiling an expression (like case 1) we'll preprocess the RuneParentEnvLookupSR rule out, to populate its value from the environment.


# Can't Get All Descendants Of Interface (CGADOI)

Let's say we have a `MyStruct` implementing interface `MyObserver<int>`:

If we want to know all children for `MyObserver`, would we count this? It's hard to say.

For now, we leave that question unanswered, and say that we can never know all children for a specific interface template.


# Need Bound Information From Parameters (NBIFP)

(previously NBIFPR)

Let's say we have this code:

```
#!DeriveStructDrop
struct BorkForwarder<LamT>
where func __call(&LamT)int {
  lam LamT;
}

func bork<LamT>(self &BorkForwarder<LamT>) int {
  return (self.lam)();
}

exported func main() {
  b = BorkForwarder({ 7 });
  b.bork();
  [_] = b;
}
```

This fails on `(self.lam)()` because `bork` itself doesn't know that there exists a `__call(&Lam)int`.

Two possible solutions:

 1. Require the user add bounds to `func bork` (and all other callers) too.
 2. Infer that there's a `__call(&Lam)int` from the existence of `BorkForwarder<Lam>` which requires it.

We can't always do 1 because sometimes the caller is an abstract function (see ONBIFS).

So, we'll need to do #2.

A few places we'll need to do this:

 * At the beginning of the current denizen, where we introduce the placeholders. We scour all of the requirements imposed by all of the parameters (like the `BorkForwarder<LamT>` that requires `__call(&LamT)int`) and create prototypes for them. (See also [Rust #2089](https://github.com/rust-lang/rfcs/pull/2089))
 * When an abstract function is "calling" an override, we'll need to incorporate the bounds for the overriding struct. (See ONBIFS)
 * In a match's case statement, when we mention a type, we need to incorporate the bounds from that type.


### ... but not return types.

Note that while we can incorporate bounds from parameters, we can't incorporate any from return types. We used to do this, and in this example:

```
func HashMap<K Ref imm, V, H, E>(hasher H, equator E) HashMap<K, V, H, E> {
  return HashMap<K, V, H, E>(hasher, equator, 0);
}
```

It failed because `main` wasn't passing any functions to satisfy the bounds which were expected by the `HashMap<K, V, H, E>(hasher, equator, 0)` invocation (it expected a `drop(H)`). At best, we can hoist the _requirements_ from the return type, but we can't use the return type as evidence that a type satisfies some bounds.


### Monomorphizer

The monomorphizer also needs to do this. This example (search NBIFP for test case) shows why:

```
struct IntHasher { }
func __call(this &IntHasher, x int) int { return x; }

#!DeriveStructDrop
struct HashMap<H> where func(&H, int)int {
  hasher H;
}

func moo<H>(self &HashMap<H>) {
  // Nothing needed in here to cause the bug
}

exported func main() int {
  m = HashMap(IntHasher());
  moo(&m);
  destruct m;
  return 9;
}
```

When we instantiate `moo`, we're given 


# Overrides Need Bound Information From Structs (ONBIFS)

This is a special case of NBIFPR, where an abstract function is trying to resolve an override which has some requirements.

```
sealed interface Bork {
  func bork(virtual self &Bork) int;
}

struct BorkForwarder<Lam>
where func drop(Lam)void, func __call(&Lam)int {
  lam Lam;
}

impl<Lam> Bork for BorkForwarder<Lam>
where func drop(Lam)void, func __call(&Lam)int;

func bork<Lam>(self &BorkForwarder<Lam>) int
where func drop(Lam)void, func __call(&Lam)int {
  return (&self.lam)();
}

exported func main() int {
  f = BorkForwarder({ 7 });
  return f.bork();
}
```

This failed while trying to assemble the itables.

When we were figuring out the vtable for `BorkForwarder<Lam1>`, we were trying to find its override for `bork`.

We looked for a `bork(BorkForwarder<Lam1>)`. (Aside: because of NAFEWRO we looked from the perspective of `bork(Bork)`, we used its environment.)

However, `func bork(BorkForwarder<Lam1>)` has a requirement that there's a `func __call(&Lam)int`, but the call site (the abstract function `bork(Bork)`) had no knowledge of such a function, so it failed.

This reinforces that we need to solve NBIFPR by gathering information from elsewhere (parameters).


# Functions Must Be Associated With Type to be Used in Function Bounds (FMBAWTUFB)

We can't just pass any old function in for a function bound. It must be associated with the type somehow. Otherwise, if we have a struct and function like this:

```
struct Bork<T>
where func zork(T)void {
  ...
}

func moo(bork Bork<T>)
where func zork(T)void {
  ...
}
```

and then we store that `Bork<T>` into a global (or some other object somewhere), and then later retrieve it, we'll have no idea what its `zork` is.









Are these two separate problems?

 - Here we want the call to be able to look into the environment of its parameters to get the bounds. Its the only callsite that doesnt really have them already. we kinda want to gather those protos before executing the callsitefuncSRs. kinda like definitionsiteSRs really.
 - Above, we want the definition to look into the environment of its parameters to get the bounds. for convenience really.


its kind of like we have some sort of switch case going on. the match statement is what figures out whether it's a certain type, which comes with some knowledge that certain bounds are met. so, we'll need to do this for match statements too, interesting. well maybe not; we'll check its a valid type in the match statement before dispatching.
hmm.

the case will check that its an actual valid combination.

this is like having a bound on the outer func, then doing a match, and calling the inner func.
but also calling the struct. hmm.



Perhaps instead, it should know this just from looking at `BorkForwarder<Lam>`'s definition and seeing that there must exist one for it somewhere.

(Interesting consequence: if the bound travels with the type, and not through all parent functions, then we can't customize it all the way down. The function needs to be associated with the type itself. At least... where interfaces are involved. There's nothing stopping us from making a custom substruct that does interesting things with the functions we give it. In fact, that's the entire basis of the anonymous substruct feature.)

So, when we are looking for a function involving something, we should look in the environment for every parameter to see if there are any function bounds for that sort of thing.

Don't we already do that?



could we perhaps grab that information from the structs, but still require it from the functions?

looks like thats what rust does too: https://github.com/rust-lang/rfcs/pull/2089

so, when we get into a generic function, lets grab the bounds from the structs.

when calling from an abstract, we... hmmm..........

we know it meets those bounds because we have an existing ImplT with that StructRef, so it has to work. we just need to bring that knowledge into the "case" so to speak.



for now, just say it must be associated with the type; must be in the type's module or rather, visible from the type's own environment (so can be in its dependency)

this works well with overrides not needing the override/impl keyword.

how does this work with interfaces? if the subclass has a bound, it must itself make sure its satisfied, cannot rely on the interface.

if the subclass func has a bound, it must be satisfied by the abstract fn or the struct itself or the caller.

if the base class has a bound, maybe we can use it from the subclass?




right now, when we translate an InterfaceTT, we try to instantiate the InterfaceDefinitionT. then we try to monomorphize the function. it doesnt know the function bounds.

when we call an interface function, or even call an interface template, we need to instantiate it. same as how functions work.

to do that, it would be good to pre-resolve any function bounds that the interface needs, such as func drop(T)void.

it might be good idea to put these somewhere else on the side, perhaps cached. seems like a lot of repeated work otherwise.

can do the same thing for function calls with certain arguments, thatd be nice.

when we call a sealed interface function (isEmpty) that has bounds (drop(T)), do we need to monomorphize the functions? yes, probably. or do the edges do that? like upcasting?



did you know that `Opt<Ship>` and `Opt<Boat>` might have different vtables? one has clone and the other doesnt!




https://doc.rust-lang.org/rust-by-example/generics/where.html
`where Option<T>: Debug` wow




# Tuples And Variadics With Generics (TAVWG)

Tuples have a variadic member:

```
struct Tup<T RefList> {
  ..T;
}
```

It would be easy to work with these if we knew the actual instantiation, like if we were currently dealing with a Tup<(int, bool)> we could reasonably determine what myTup.0 is, or how to destroy myTup.

However, we need to be able to write generic functions for tuples, for example drop. It would look something like:

```
func drop<T RefList>(tup Tup<T>)
where func drop(T...)void {
  (tup)...drop();
}
```

which means we need some sort of "move ellipsis" operator and something to check the bounds on each T.

Alas, that's likely too much work for now, we'll have to fall back to having a two-element tuple and come back to them.


# Lambdas Are Generic Templates (LAGT)

Lambdas are instantiated every time they're called. In this:

```
func genFunc<T>(a &T) &T {
  f = x => a; // Lambda struct at code loc 2:6
  f(true);
  f(a);
  f(7)
}
exported func main() int {
  genFunc(7)
}
```

There are actually three generic functions created:

 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{bool}`
 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{genFunc$0}`
 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{int}`

These are each generic functions, just like a normal `func moo<T>(a T) { ... }`/`moo<moo$0>` generic function. These just have an extra disambiguation, these are **not** instantiations.

They are disambiguated by the "generic template args" (eg `{bool}`). It's just a list of coords to disambiguate them from each other. These are not generic args, they are generic **template** args.

In this program, we're calling only `genFunc(7)` so after the monomorphizer pass these would be the three final instantiations in total:

 * `mvtest/genFunc<int>.lam:2:6.__call{bool}<bool>`
 * `mvtest/genFunc<int>.lam:2:6.__call{int}<int>`
 * `mvtest/genFunc<int>.lam:2:6.__call{int}<int>` (duplicate!)

The generic template args are usually redundant with the actual parameters, so we don't include a `(bool)` at the end of the name like we usually do for function names.

However, theyre not necessarily redundant with the template args. If we had a `(a int, b)` then the lambda has only one generic arg, the implicit one for b. That thing's name might be `__call{int, bool}<bool>`.

Serendipitously, this approach will result in the same ending instantiation name for those latter two, so we don't have to instantiate that lambda an extra time needlessly.


If we also called `genFunc("hello")` we'd have these 6 in total:

 * `mvtest/genFunc<int>.lam:2:6.__call{bool}<bool>`
 * `mvtest/genFunc<int>.lam:2:6.__call{int}<int>`
 * `mvtest/genFunc<int>.lam:2:6.__call{int}<int>`
 * `mvtest/genFunc<str>.lam:2:6.__call{bool}<bool>`
 * `mvtest/genFunc<str>.lam:2:6.__call{str}<str>`
 * `mvtest/genFunc<str>.lam:2:6.__call{int}<int>`

So in a way, a generic is a template that makes a generic function. That one `x => a` is a template which formed three generic functions. Each of those generic functions was instantiated twice, so we have six in total.


## Lambdas Have Placeholders from Containing Top Level Denizen (LHPCTLD)

Look at LAGT's example, and we see that lambdas never create placeholders for themselves; there are never any lambda parameter placeholders.

They do however sometimes use placeholders from their parent function, such as the `mvtest/genFunc<genFunc$0>.lam:2:6.__call{genFunc$0}`.

Another example, this program has a lambda inside a generic function:

```
func genFunc<T>(a &T) &T {
  return { a }();
}
exported func main() int {
  genFunc(7)
}
```

The function `genFunc.lam:2:10`, is loading `a` whose type is actually the containing genFunc's 0th placeholder.

In other words, lambda functions load placeholders from a different function (their parent function).



## Getting Lambda Instantiation's Original Generic's Name (GLIOGN)

Normally, if we have a PrototypeT's full name, it's pretty easy to get its original template's full name. Take a FullNameT[IFunctionNameT]'s local name (the IFunctionNameT) and just call .template on it.

However, that doesn't work for lambdas, which are templates rather than generics.

```
func genFunc<T>(a &T) &T {
  f = x => a; // Lambda struct at code loc 2:6
  f(true);
  f(a);
  f(7)
}
exported func main() int {
  genFunc(7)
}
```

Here, there are actually three generic functions created:

 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{bool}`
 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{genFunc$0}`
 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{int}`

In this program, we're calling only `genFunc(7)` so these would be the final three instantiations in total:

 * `mvtest/genFunc<int>.lam:2:6.__call{bool}<bool>`
 * `mvtest/genFunc<int>.lam:2:6.__call{genFunc$0}<int>`
 * `mvtest/genFunc<int>.lam:2:6.__call{int}<int>`

Note how `{genFunc$0}` is still there, even in an instantiation name. See DMPOGN for why.
  
These all might be completely different functions, depending on what happened inside the body, what kind of metaprogramming it does, etc.

Now the challenge: What is `mvtest/genFunc<int>.lam:2:6.__call{int}<int>`'s original generic's name? We have to be careful because these look very similar:

 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{genFunc$0}`
 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{int}`

It's actually the second one.

We can find that second one by comparing all their generic full names. The generic full name is where we remove any template args (`<...>`) and parameters (`(...)`) from every name in the full name.

`mvtest/genFunc<int>.lam:2:6.__call{int}<int>` becomes `mvtest/genFunc.lam:2:6.__call{int}`.

The two candidates become:
 * `mvtest/genFunc.lam:2:6.__call{genFunc$0}`
 * `mvtest/genFunclam:2:6.__call{int}`

The second one matches nicely!

And that's how we find the original generic function for a particular instantiation.

## Don't Monomorphize Parts Of Generic Names (DMPOGN)

Because of GLIOGN, we need a lambda function to remember its original generic.

There was an interesting oddity, where we had an instantiated name that contained a placeholder: `mvtest/genFunc<int>.lam:2:6.__call{genFunc$0}<int>`.

This section explains why that's important, by showing what happens if we don't.

Let's say we have a similar program, but calling `genFunc(str)`

```
func genFunc<T>(a &T) &T {
  f = x => a; // Lambda struct at code loc 2:6
  f(true);
  f(a);
  f(7)
}
exported func main() int {
  genFunc("hello")
}
```

We have those same three generic functions coming from the typing pass:

 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{bool}`
 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{genFunc$0}`
 * `mvtest/genFunc<genFunc$0>.lam:2:6.__call{int}`

But now the challenge is: What is `mvtest/genFunc<str>.lam:2:6.__call{str}<str>`'s original generic's name?

We apply the same rules to get the generic full name, and end up with `mvtest/genFunc.lam:2:6.__call{str}`. **However, that doesn't exist.** There is no generic by that name.

This happened because we were looking for something nonsensical. How did that `__call{str}` even happen, and why were we looking for something with that? It's because our translator in monomorphizer was blindly replacing *all* placeholders.

Moral of the story: It shouldn't replace placeholders in generic names, such as `__call{genFunc$0)`. It should leave that alone, so that we instead looked for `mvtest/genFunc<str>.lam:2:6.__call{$genFunc0}<str>`.


# Must Specify Array Element (MSAE)

We no longer support grabbing a prototype's return type, so we can no longer say:

`Array<mut>(5, x => x)`

It previously would look at the incoming lambda, send it an argument type, and grab the return value. We'll need to add that back in.

For now, we have to specify the type:

`Array<mut, int>(5, x => x)`


# Lambdas Can Call Parents' Generic Bounds (LCCPGB)

If we have a lambda in a generic function:

```
func genFunc<T>(a &T)
where func print(&T)void {
  { print(a); }()
}
exported func main() {
  genFunc("hello");
}
```

Then when we monomorphize the lambda, the instantiator needs to remember the supplied `print` from `genFunc`'s caller.


## Lambdas and Children Need Bound Arguments From Above (LCNBAFA)

When we're monomorphizing a lambda, it will be calling function bounds that came from the parent function. So, the monomorphizer needs to convey those downward when we stamp a lambda generic template.


For example, when we're inside

`add<int, int, ^IntHasher>(&HashMap<int, int, ^IntHasher)`

it will want to call 

`add:204<int, int, ^IntHasher>(&HashMap<int, int, ^IntHasher>).lam:281` which might call `HashMap.bound:__call<>(&add$2, @add$0)int`. But the lambda itself doesn't know any bounds... it really should contain the mapping `IntHasher.__call<>(&IntHasher, int)` ->
`HashMap.bound:__call<>(&add$2, @add$0)int` somehow.

So in the monomorphizer, when a function tries to instantiate something beginning with its own name, it will pass down its own bounds into it as well.


Additionally, we run into the same problem with child functions of the lambda (and likely will again with interfaces' child functions).

`add<int, int, ^IntHasher>(&HashMap<int, int, ^IntHasher)`

Will want to call this `drop` function:

```
add:204<int, int, ^IntHasher>(&HashMap<int, int, ^IntHasher>)
.lam:281
.drop<>(@add:204<int, int, ^IntHasher>(&HashMap<int, int, ^IntHasher>).lam:281)
```

so it tries to monomorphize that. It takes a `@add:204<int, int, ^IntHasher>(&HashMap<int, int, ^IntHasher>).lam:281` argument, which has a template arg of `HashMap<int, int, ^IntHasher>`, but when it sees that it doesn't see that we satisfied its bounds (similar to above) so it dies.

The solution is the same: in the monomorphizer, when a function tries to instantiate something beginning with its own name, it will pass down its own bounds into it as well.




# WTF Is Going On With Impls (WTFIGOWI)

Recall:

```
interface ISpaceship<E, F, G> { ... }
abstract func moo<X, Y, Z>(self &ISpaceship<X, Y, Z>, bork X) where exists drop(Y)void;

struct Raza<A, B, C> { ... }
impl<I, J> ISpaceship<int, I, J> for Raza<I, J>;

func moo<Y, Z>(self &Raza<Y, Z>, bork int) where exists drop(Y)void { ... }
```

Right now we have ISpaceship, moo, and the impl ("ri").

We want to locate that moo/Raza override, similarly to the above conceptual
match-dispatching function:

```
func moo<Y, Z>(virtual self &ISpaceship<int, Y, Z>, bork int)
where exists drop(Y)void {
  self match {
    raza &Raza<Y, Z> => moo(raza, bork)
  }
}
```

The first step is figuring out this function's inner environment, so we can
later use it to resolve our moo override.

Start by compiling the impl, supplying any placeholders for it.

```
impl<I, J> ISpaceship<int, I, J> for Raza<I, J>;
```

becomes:

```
impl<ri$0, ri$1> ISpaceship<int, ri$0, ri$1> for Raza<ri$0, ri$1>
```

Now, take the original abstract function:

```
abstract func moo<X, Y, Z>(self &ISpaceship<X, Y, Z>, bork X) where exists drop(Y)void;
```

and try to compile it given the impl's interface (ISpaceship<int, ri$0, ri$1>)
as the first parameter. However, rephrase the placeholders to be in terms of the dispatching
function's case (so instead of ri$0, think of it as case$0)

```
abstract func moo(self ISpaceship<int, case$0, case$1>, bork int) where exists drop(case$0)void;
```

We rephrased like that because we're conceptually compiling a match's case, from
which we'll resolve a function. Also, the abstract function has some bounds which are phrased in terms
of its own placeholders. 

In a way, we compiled it as if X = int, Y = case$0, Z = case$1.

And now we have our inner environment from which we can resolve some overloads.


## Resolving Overrides With Cases (ROWC)

Our goal now is to figure out the override functions.
Imagine we have these interfaces and impls:

```
interface ISpaceship<E, F, G> { ... }

struct Serenity<A, B, C> { ... }
impl<H, I, J> ISpaceship<H, I, J> for Serenity<H, I, J>;

struct Firefly<A, B, C> { ... }
impl<H, I, J> ISpaceship<J, I, H> for Firefly<H, I, J>;
// Note the weird order here ^

struct Raza<B, C> { ... }
impl<I, J> ISpaceship<int, I, J> for Raza<I, J>;
// Note this int here ^

struct Milano<A, B, C, D> { ... }
impl<I, J, K, L> ISpaceship<I, J, K> for Milano<I, J, K, L>;
// Note that Milano has more params than ISpaceship.
// This is like a MyStruct<T> implementing a IIntObserver.

struct Enterprise<A> { ... }
impl<H> ISpaceship<H, H, H> for Enterprise<H>;
// Note they're all the same type
```

If we have an abstract function:

```
abstract func launch<X, Y, Z>(self &ISpaceship<X, Y, Z>, bork X) where exists drop(X)void;
```

and these overrides:

```
func launch<X, Y, Z>(self &Serenity<X, Y, Z>, bork X) where exists drop(X)void { ... }
func launch<X, Y, Z>(self &Firefly<X, Y, Z>, bork X) where exists drop(X)void { ... }
func launch<Y, Z>(self &Raza<Y, Z>, bork int) { ... }
func launch<X, Y, Z, ZZ>(self &Milano<X, Y, Z, ZZ>, bork X) where exists drop(X)void { ... }
func launch<X>(self &Enterprise<X>, bork X) where exists drop(X)void { ... }
```

We need to find those overrides.

To do it, we need to *conceptually* lower these abstract functions to match-dispatching
functions. We're not actually doing this, just thinking this way. One might be:

```
func launch<X, Y, Z>(virtual self &ISpaceship<X, Y, Z>, bork X)
where exists drop(X)void {
  self match {
    serenity &Serenity<X, Y, Z> => launch(serenity, bork)
    firefly &Firefly<Z, Y, X> => launch(firefly, bork)
    <ZZ> milano &Milano<X, Y, Z, ZZ> => launch(milano, bork) // See the end for wtf this is
    // Read on for why the other cases aren't here
  }
}
```

Raza and Enterprise have some assumptions about their generic args, so we'll need different
conceptual functions for them.

```
func launch<Y, Z>(virtual self &ISpaceship<int, Y, Z>, bork int) {
  self match {
    raza &Raza<Y, Z> => launch(raza, bork)
    // other cases unimportant for our purposes
  }
}

func launch<X>(virtual self &ISpaceship<X, X, X>, bork X)
where exists drop(X)void {
  self match {
    enterprise &Enterprise<X> => launch(enterprise, bork)
    // other cases unimportant for our purposes
  }
}
```

The reason we do all this is so we can do those resolves:

   * launch(serenity) is resolving launch(&Serenity<X, Y, Z>, X)
   * launch(firefly) is resolving launch(&Firefly<Z, Y, X>, X)
   * launch(raza) is resolving launch(&Raza<Y, Z>, int)
   * launch(enterprise) is resolving launch(&Enterprise<H>, X)

So, the below code does the important parts of the above conceptual functions.


## Override Milano Case Needs Additional Generic Params (OMCNAGP)

Now, how do we handle Milano's case?

```
func launch<X, Y, Z>(virtual self &ISpaceship<X, Y, Z>, bork X)
where exists drop(X)void {
  self match {
    serenity &Serenity<X, Y, Z> => launch(serenity, bork)
    firefly &Firefly<Z, Y, X> => launch(firefly, bork)
    <ZZ> milano &Milano<X, Y, Z, ZZ> => launch(milano, bork)
  }
}
```

As you can see, it doesn't really fit into this whole match/enum paradigm.
There could be any number of Milano variants in there... ZZ could be int, or str, or bool,
or whatever. Luckily there is a solution, described further below. (TODO: inline that here)


The `<ZZ>` is an "independent" generic arg.

Try and think if there's any times we should generate only one placeholder for an impl that has a bunch of independent generic args.


## Abstract Function Calls The Dispatcher (AFCTD)

Let's say we have this abstract func:

```
abstract func send<T>(self &IObs<T>, e T)
where D Prot = func drop(T)void
```

And this impl:

```
impl<X, ZZ> IObs<Opt<X>> for MyStruct<X, ZZ>
```

Then call the abstract function with `&IObs<Opt<dis$X>>` which is the interface from the impl but with its placeholders phrased as "dis" placeholders which stands for "dispatcher". We end up with this:

```
func ...(self &IObs<Opt<dis$X>>, e Opt<dis$X>)
where D Prot = func drop(Opt<dis$X>)void
```

Now fill in the name; it's the "dispatcher" and it takes in generic parameters similar to the impl. Think of this as an alternate way of compiling a function; we end up with something like a compiled function, that's phrased in terms of its own placeholders.

```
func dis<dis$X>(self &IObs<Opt<dis$X>>, e Opt<dis$X>)
where func drop(Opt<dis$X>)void
```

(Note that there's no `dis$ZZ` in here, we don't include independent runes, see OMCNAGP.)

So it's like the abstract function `send<T>` magically decided that its own `T` = `Opt<dis$X>`, and calls `dis` with it.

In the end, `send<T>` is calling `dis<dis$X>(&IObs<Opt<dis$X>>, Opt<dis$X>)` and sends instantiation bounds `D` = `func drop(send$T)void`.

Of course, if this was an actual call AST, some asserts would definitely trigger, because we're sending an `&IObs<send$T>` into a parameter expecting a `&IObs<Opt<dis$X>>`, and the other argument doesn't match either.

This is an unusual call. It's manufacturing a `dis$X` out of thin air. The abstract function doesn't have that, and has no idea where it comes from, while it's in the typing pass. It's up to the monomorphizer to substitute `send$T` and `dis$X` correctly so that the arguments and parameters line up.


### Monomorphizing

Let's walk through some monomorphizing cases.

Someone calls `send<Opt<str>>(&IObs<Opt<str>>, Opt<str>)`. The call's instantiation bounds say that `D` = `func drop(Opt<str>)void`.

We look through the edges to find all the structs that could implement `IObs<Opt<bool>>`.

  * We consider an instantiation `OtherStruct<bool>` impls `IObs<bool>` from an `impl<E> OtherStruct<E> for IObs<bool>`.
     * Its interface doesn't match, abort.
  * We consider an instantiation `MyStruct<int, Opt<str>>` impls `IObs<Opt<str>>`, from our original impl.
     * Its interface matches, so proceed.

We look at that edge's full name and find its ZZ = `int` and X = `str`.

Now we look at the call from the typing pass: `send<send$T>` calls `dis<dis$X>(&IObs<Opt<dis$X>>, Opt<dis$X>)` with instantiation bounds `D` = `func drop(send$T)void`).

We have all the substitutions we need.
 
 * We know from the original call's name that `send$T` is an `Opt<str>`.
 * We know `dis$X` should be `str` from the edge.

The call becomes: `dis<str>(&IObs<Opt<str>>, Opt<str>)` with instantiation bounds `D` = `func drop(Opt<str>)void`).



## Something


We'll solve the impl given the placeholdered super interface.
So if we have an abstract function:

```
func launch<T>(virtual a ISpaceship<int, T>);
```

Imagine this body:

```
func launch<T, Z>(virtual self &ISpaceship<int, T, Z>) {
  self match {
    myShip Serenity<int, T, Z> => launch(myShip)
    myBike Raza<int, Z, T> => launch(myBike)
  }
}
```

Imagine we're actually compiling it, which means we have placeholders:

```
func launch<$0, $1>(virtual self &ISpaceship<int, $0, $1>) {
  self match {
    myShip &Serenity<int, $0, $1> => launch(myShip)
    myBike &Raza<int, $1, $0> => launch(myBike)
  }
}
```

Right here, we're trying to resolve an override function, eg `launch(myBike)`.
First, we need to figure out `Raza<$1, $0>`. That's what this impl solve is
doing. It's feeding the ISpaceship<$0, $1> into the impl:

```
impl<T, Z> ISpaceship<T, Z> for Raza<Z, T>;
```

roughly solving to:

```
impl<launch$0, launch$1> ISpaceship<launch$0, launch$1> for Raza<launch$1, launch$0>;
```

to get the `Raza<launch$1, launch$0>`.


## Next Step 3



Recall the match-dispatching function:

```
func launch<Y, Z>(virtual self &ISpaceship<int, Y, Z>, bork int)
where exists drop(Y)void {
  self match {
    raza &Raza<Y, Z> => launch(raza, bork)
    // other cases unimportant for our purposes
  }
}
```

Now we have it's inner environment's inferences! We'll construct an IEnvironment below
containing these. Then we can use this to resolve some overrides.

For the Milano case, we'll add to the inner environment the extra placeholder
that doesn't correspond to any template argument in the abstract function. It actually
comes from the impl; any placeholders that cant be figured out from just the incoming interface
will be added as generic parameters for the case.

```
<ZZ> milano &Milano<X, Y, Z, ZZ> => launch(milano, bork)
```

For all ZZ, we're having a Milano case. Later on in the instantiator, we'll loop over all impls and get the ZZ from there.






This is so we fill the other params' types, like that bork int.

Now, try to resolve an overload with the impl's struct there instead, look for:
```
launch(Raza<ri$0, ri$1>, int)
```
and sure enough, we find the override func:
```
func launch<P, Q>(self Raza<P, Q>, bork int) where exists drop(P)void;
```

Instantiating it is the next challenge, we'll do that below.

All this is done from the impl's perspective, the impl is the original calling
env for all these solves and resolves, and all these placeholders are phrased in
terms of impl placeholders (eg ri$0).



## WTFBBQ


We need this to pull in some bounds knowledge from the override struct.

For example, this is a parent interface that has no knowledge or assumptions of
being droppable:

```
#!DeriveInterfaceDrop
sealed interface ILaunchable {
  func launch(virtual self &ILaunchable) int;
}

#!DeriveStructDrop
struct Ship<T>
where func drop(Lam)void, func __call(&Lam)int {
  lam Lam;
}

impl<T> ILaunchable for Ship<T>;

func launch<T>(self &Ship<T>) int {
  return (self.lam)();
}
```

When resolving overrides for it, this is the conceptual case:

```
func launch(virtual self &ILaunchable) {
  self match {
    <ZZ> borky &Ship<ZZ> => bork(fwd)
  }
}
```

However, there's something subtle that's needed. The bork(fwd) call is trying to resolve
this function:

```
func launch<T>(self &Ship<T>) int
```

However, the `Ship<T>` invocation requires that Lam has a `drop`... which nobody
can guarantee.

But wait! We're taking an *existing* `Ship<T>` there. So we can know that the T already
supports a drop.

We do this for NBIFPR for parameters and returns and one day for cases inside matches.
Let's do it here for this conceptual case too.






