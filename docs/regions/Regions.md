

how do we do this?

# Basic Regions (BASREG)

## The Challenge

The challenge is that we can turn a region immutable at will. In all these below cases, `arr` is type `main' []Ship` and `main'` is mutable, but once we do anything pure (pure block, parallel foreach, pure call) that `main'` becomes immutable.

Case 1: A pure block

```
func main() {
  arr = [](10, i => Ship(i));
  pure {
    foreach ship in arr {
      println(ship.fuel);
    }
  }
}
```

Case 2: A parallel foreach

```
func main() {
  arr = [](10, i => Ship(i));
  parallel foreach ship in arr {
    println(ship.fuel);
  }
}
```

Case 3: Calling a pure function

```
pure func printAllShips<r' ro>(arr r' []Ship) {
  foreach ship in arr {
    println(ship.fuel);
  }
}
func main() {
  arr = [](10, i => Ship(i));
  printAllShips(&arr);
}
```

Case 1 and 2 are actually pretty similar. Case 2 can probably even use case 1 under the hood.

So how do we know that a region is immutable? It would be pretty difficult to track that in the coord, because as we cross that `pure {` boundary wed' have to change the coord of everything coming in via environment and stuff.


## A: Re-declare regions in a more local environment

We can have the region itself in the environment. It's name would be `main'` and its value would be something like `Region(mutable)`.

When we get to the `pure {` we'll redeclare those regions, so it would be `main'` -> `Region(immutable)` at that point.


## B: Compare "stack heights" to know whether somethings' immutable

Lets' say that `main'` is at height 0 and that `pure {` is at "height" 4, and inside it is height 5. While were' inside it, we know that `main'`s' height is below `pure {`s' height, and therefore anything in `main'` is immutable right now.


## Go with option A

Option A seems a little more precise. Option B indiscriminately turns everything immutable, which works well for these examples, but maybe one day we'll have a `xrw` mutex open and we want it to remain mutable even inside the pure block. Option A can do that, option B cant'.

So, every coord will have the name of a region that lives in the environment.


# Overload Resolution With Regions (ORWR)

Every coord has a region ID in it. During solving, we actually won't care about whether a region is mutable or immutable, we'll just figure out which caller regions send into which callee regions.

After the overloading is done, probably near the instantiation checking, we'll also check that the regions line up well.


# Do We Have Region in Coord? (DWHRC)

The user will think of the coord as having a region. They'll be using notation like `a'Ship` and `b'&Ship` which does seem to imply it's in the coord.


However, it doesn't necessarily have to be that way under the hood, it could just be a generic param on the ship, like `Ship<a'>` and `&Ship<a'>`. It would be pretty similar conceptually, really.


For now, we'll actually have it in both places: `a'Ship<a'>` and `b'&Ship<b'>`. The one in the coord will really just be derived from the generic parameter.


One day, we might decide which makes more sense. This section covers how both would work.


## Have Region Only In Coord (HeROIC)

(This is not the case yet, this is a hypothetical.)

If we do this, then this struct:

```
struct Ship {
  engine Engine;
}
```

would be this under the hood:

```
struct Ship self'{
  engine self'Engine;
}
```

The `self'` in `self'{` is just a way for us to designate that the self region is referred to by "`self'`". We could also just make `self'` a special reserved region keyword.


### Transforming Regions When Getting Members (TRWGM)

When we're in a function and we try to grab a member, like the `s.engine` here:

```
func main() {
  s = Ship(Engine(7));
  e = s.engine;
  println(e);
}
```

We actually need to translate that Coord(`own`, `Ship.self'`, `Engine`) to Coord(`borrow`, `main.self'`, `Engine`).


We do something exactly like this when we grab something of type `X` out of a `MyStruct<X>` like `MyStruct<int>`; we translate that `X` placeholder to `int` when we bring it into the user's world.


To do this, we can either:

 * Look up the name of the region from inside the struct.
 * Make it deterministic, so we can always determine a struct's default region id.

We go with the latter; the function TemplataCompiler.getDefaultRegionId will tell us the region id for any template.



## Have Region As Only Generic Parameter (HRAOGP)

(This is not the case yet, this is a hypothetical.)

Under the hood, our Coord can still just contain an ownership and a kind.

It would actually work well for things like `int`, `bool`, `float`, `void`, `never`, because they dont really have regions and they kind of transcend this whole region _thing_.

Under the hood, structs will have a region generic param. This struct:

```
struct Ship {
  engine Engine;
}
```

would really be this under the hood:

```
struct Ship<a' own> a'{
  engine Engine<a'>;
}
```

The `own` in `a' own` is needed... i'm not sure why. Maybe we don't need it. Maybe we do?

 * Maybe because we can't just own something in any ol' region. If we could create something in any ol' region, then if we have a `MyStruct<a', b'>` it's hard to know whether we can blast it away. Would we indirectly be blasting away something in `b'`?

The `a'{`. That is a way to give a name to the region that this struct is in.

The `a'` in `Engine<a'>` is (conceptually) needed because if we require a parameter, they'll have a parameter.


The advantage here is that it makes it a bit easier to do substitutions.


## Start Using Internal Onion Typing (SUIOT)

We don't really need a default region if instead of coords we have just... types. Like:

 * int
 * bool
 * MyStruct\<int\>
 * Heap(MyStruct\<int\>)
 * Borrow(MyStruct\<int\>)

Kind of like you would see in languages that allow any number of nested pointers/references.

In this approach, we wouldn't actually need to have a special region at all. This struct:

```
struct Ship {
  engine Engine;
}
```

has no place that we need to specify a region for anything.


# Implicit Default Region

Every denizen has a self region.

For example, this `main` has a default region:

```
func main(args []str) {
  list = List<int>(0);
  println(list.len());
}
```

Here's the same exact thing with all the implicit things added:

```
func main<m' rw>(args m'[]str) m'{
  list = m'List<int>(0);
  println(list.len());
}
```

Here's a struct:

```
struct HashMap<K, V> {
  size int;
  arr []HashMapNode<K, V>;
}
```

and here it is with the implicit region added in:

```
struct HashMap<K, V, x'> x'{
  size x'int;
  arr x'[]HashMapNode<K, V>;
}
```

It seems this serves two purposes:

 * To answer the question "what region is `arr` in?"
 * To answer the question "what region is the `HashMap` in?". though, thats not really a question the HashMap ever really needs to know...

it does also make the question awkward of... if we leave a region annotation off, what region is it? well, i guess its the default region. kind of makes sense.

does a struct really need to know its own region?


having it as a struct parameter also lets us have types that are region-less... ints, floats, etc. but that kind of leaves behind things like Vec3 which want to be inline values too.


heres a weird question... if it has a value, then what would we hand in as its initial one? like, what would the monomorphizer hand in for `main`s region?
well... hmm. "root mutable"?
i guess thats kind of the same thing as having a function define its own ambient region?


 * can the thing's region change?
    * yes, yes it can actually. the region varies, independently of the type. but we could do that with generic params too i think.
    * it cannot be moved or changed if it's part of of an immutable one.
    * its special because this particular region will be made immutable by a pure function, and everything indirectly owned, but not necessarily the other regions.
    * so it's special, we know that. but does that mean it should be a generic parameter?
    * are we able to turn an xrw region into an immutable? i think so. so any of these can vary.
 * its special because its the region of all the owning things inside it. is there a difference there?

a function can independently move things around. it has a different region than the things it contains. structs kind of are decoupled too, in the opposite direction.

It's not entirely clear whether this is needed. We'll see if it comes into play. I wonder if this is kind of like the region of the stack frame?


# Are Regions Actually Generic Parameters? (ARAGP)


Take this function:

```
pure func len<K Ref imm, V, H, E, r' imm>(
  self r' &RHashMap<K, V, H, E>)
int {
  return self.size;
}
```

`r'` is actually a generic parameter.


This is particularly beneficial because it will help us reuse the substitution logic. For example, if we have this struct (s' region added for clarity):

```
struct HashMap<K Ref imm, V, H, E, s'> where ... s' {
  hasher H;
  equator E;
  size s'int;
  arr s'[]HashMapNode<K, V>;
}
```

then the caller can just hand in the `s'` and it will affect the `size` and `arr` inside.




## Old Thoughts

(Not relevant any more)

Even though regions appear as generic parameters, they arent' really.

Take this function:

```
pure func len<r' imm, K Ref imm, V, H, E>(
  self r' &RHashMap<K, V, H, E>)
int {
  return self.size;
}
```

It's actually more like...

```
pure func len[r' imm]<K Ref imm, V, H, E>(
  self r' &RHashMap<K, V, H, E>)
int {
  return self.size;
}
```

Just dont' think of them as generic params. Think of them more like permissions (like c++ const) which are tied to the specific function. It's like the function has its own permissions.


# Returning Directly Into A Region (RDIAR)

Take this clone function:

```
func clone<E>(list &List<E>) List<E>
where func clone(&E)E {
  return List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

If were' to regionize it, its' unclear how wed' handle that returned `List<E>`.

We have a few options:

 * Require any returned value to be an iso, like `iso List<E>`.
 * Have it be part of `clone`s' own region, and then implicitly copy/transmigrate it at the end.
 * Take the "output region" in as a region parameter.

That third one is the most promising. An example:

```
pure func clone<'o, 'l, E>(list l' &List<E>) o' List<E>
where pure func clone<'e, o'>(e' &E) o' E c' {
  return o' List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

Were' taking the output region in as a parameter (`o'`). Now, the caller can specify where they want it to go. This means they can put it into a new region of their choice and then merge it in later.


# Inferring Regions For Pure Functions (IRFPF)

Take this clone function:

```
pure func clone<'o, 'l, E>(list l' &List<E>) o' List<E>
where pure func clone<'e, o'>(e' &E) o' E c' {
  return o' List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

A lot of functions kind of look like this, where they have a region for every parameter (minus perhaps the primitives) and another region for the return.

Lets' make that automatic for pure functions. It then becomes:

```
pure func clone<E>(list &List<E>) List<E>
where pure func clone(&E)E {
  return List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

We might need some annotations if we want to return a reference to one of the parameters, or something inside them that isnt' a generic parameter.


# Concept Functions Can Have Regions (CFCHR)

Take this struct:

```
#!DeriveStructDrop
struct RHashMap<K Ref imm, V, H, E>
where func(&H, &K)int,
    func(&E, &K, &K)bool
{
  hasher H;
  equator E;
  table! Array<mut, Opt<RHashMapNode<K, V>>>;
  size! int;
}
```

Note how it's just using the regions as known by its parent function. That's totally fine. And if we wanted to manually specify one of the parent function's regions in there, we could.

One day with full regions it could look like:

```
#!DeriveStructDrop
struct RHashMap<K Ref imm, V, H, E>
where pure func<x'>(x' &H, x' &K)int,
    pure func<x'>(x' &E, x' &K, x' &K)bool
{
  hasher H;
  equator E;
  table! Array<mut, Opt<RHashMapNode<K, V>>>;
  size! int;
}
```

But we don't wanna do that quite so soon, it's not really needed.

(When we change that, look for all occurrences of CFCHR.)


# Specifying Regions In Expressions (SRIE)

Lets' say we have this ship:

```
struct Ship { fuel int; }
```

Making a new region:

```
region x';
```

Putting something into that new region:

```
list = x'List<x'Ship>(&myArr);
```

(Let's say `myArr` is in `g'`)

Notes:

 * The `x'` in front won't affect any of the arguments for now. It's an arbitrary decision, we can change it later.
 * The `x'` in the `x'Ship` is needed for now. Well' see if we can infer it later.


The `x'` in front is actually specifying the output region. Without it, it would hand in the functions' default region. The compiler will figure out what the callee is calling their output region, and make sure we pass it in as that particular generic arg.

So if we have a callee:

```
pure func List<T, 'r, o'>(arr r'&[]T) o'List<T> { ... }
```

Then as we call it, we'll send in the callers' `x'` in and equate it with the callees' `o'`.

The tentative convention is to have that be the last generic parameter (note how the generic params has the `r'` before the `o'`) because we have this nice alternate way to specify it.

We could also specify it manually if we wanted to do that instead:

```
list = List<a'Ship, 'g, a'>(&myArr);
```

but it's rather tedious as we had to specify that second generic parameter (`g'`) along the way to get to that last one.


# Simplifying Regions in Monomorphizer

We do regions like any other template parameter. blah blah

we hand them in like template parameters, and the solver figures them out like template parameters

when they finally get to the monomorphizer, it does an interesting thing: it completely ignores the differences between any of the regions. all it does it pay attention to their mutabilities.

So these all lower to the same thing:

 * `MyThing<'a, 'b, bool>` if `a` is mut and `b` is mut
 * `MyThing<'p, 'q, bool>` if `p` is mut and `q` is mut
 * `MyThing<'x, 'y, bool>` if `x` is mut and `y` is mut

and all these are the same:

 * `MyThing<'a, 'b, bool>` if `a` is imm and `b` is mut
 * `MyThing<'p, 'q, bool>` if `p` is imm and `q` is mut
 * `MyThing<'x, 'y, bool>` if `x` is imm and `y` is mut

So in the monomorphizer, its' kind of like were' actually invoking with mutabilities, not with the actual regions' identities.






we cant just use a placeholder everywhere we currently use a coord
we need that coords' ownership

we'll also want the regions' mutability.. maybe we'll want the same arrangement?

no, should be a name that we can look up in the env i think, cuz we can turn a region immutable at will.





every time we do a pure call, would we make a new region instance? i dont think so.

when we do a pure call that merges multiple regions, we *could* keep track of what original regions it came from. and maybe we could even use those to verify some sort of universal reference thats coming in?

right now when we make a universal reference, it points at the latest merged region. later on when we try to use it, we get a seg fault. honestly maybe thats a good thing?




what happens if we do allow multiple regions in one allocation? is that bad?

perhaps if we disallow moving the container if the contained region is imm?

does the "contained"/covariant thing help  or something like it?

if we can nail it, then we can put an array in its own region and mutex lock it, instead of HGM basically.





nm start w pure calls.
just have a thing in the func env, vector of regions' mutabilities