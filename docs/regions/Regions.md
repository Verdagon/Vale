

how do we do this?

# Basic Regions (BASREG)

## The Challenge

The challenge is that we can turn a region immutable at will. In all these below cases, `arr` is type `'main []Ship` and `'main` is mutable, but once we do anything pure (pure block, parallel foreach, pure call) that `'main` becomes immutable.

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
pure func printAllShips<'r ro>(arr 'r []Ship) {
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

So how do we know that a region is immutable? It would be pretty difficult to track that in the coord, because as we cross that `pure {` boundary we'd have to change the coord of everything coming in via environment and stuff.


## A: Re-declare regions in a more local environment

We can have the region itself in the environment. It's name would be `'main` and its value would be something like `Region(mutable)`.

When we get to the `pure {` we'll redeclare those regions, so it would be `'main` -> `Region(immutable)` at that point.


## B: Compare "stack heights" to know whether something's immutable

Let's say that `'main` is at height 0 and that `pure {` is at "height" 4, and inside it is height 5. While we're inside it, we know that `'main`'s height is below `pure {`'s height, and therefore anything in `'main` is immutable right now.


## Go with option A

Option A seems a little more precise. Option B indiscriminately turns everything immutable, which works well for these examples, but maybe one day we'll have a `xrw` mutex open and we want it to remain mutable even inside the pure block. Option A can do that, option B can't.

So, every coord will have the name of a region that lives in the environment.



# Overload Resolution With Regions (ORWR)

Every coord has a region ID in it. During solving, we actually won't care about whether a region is mutable or immutable, we'll just figure out which caller regions send into which callee regions.

After the overloading is done, probably near the instantiation checking, we'll also check that the regions line up well.


# Regions Aren't Actually Generic Parameters (RAAGP)


Even though regions appear as generic parameters, they aren't really.

Take this function:

```
pure func len<'r imm, K Ref imm, V, H, E>(
  self 'r &RHashMap<K, V, H, E>)
int {
  return self.size;
}
```

It's actually more like...

```
pure func len['r imm]<K Ref imm, V, H, E>(
  self 'r &RHashMap<K, V, H, E>)
int {
  return self.size;
}
```

Just don't think of them as generic params. Think of them more like permissions (like c++ const) which are tied to the specific function. It's like the function has its own permissions.


# Returning Directly Into A Region (RDIAR)

Take this clone function:

```
func clone<E>(list &List<E>) List<E>
where func clone(&E)E {
  return List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

If we're to regionize it, it's unclear how we'd handle that returned `List<E>`.

We have a few options:

 * Require any returned value to be an iso, like `iso List<E>`.
 * Have it be part of `clone`'s own region, and then implicitly copy/transmigrate it at the end.
 * Take the "output region" in as a region parameter.

That third one is the most promising. An example:

```
pure func clone<'o, 'l, E>(list 'l &List<E>) 'o List<E>
where pure func clone<'e, 'o>('e &E) 'o E 'c {
  return 'o List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

We're taking the output region in as a parameter (`'o`). Now, the caller can specify where they want it to go. This means they can put it into a new region of their choice and then merge it in later.


# Inferring Regions For Pure Functions (IRFPF)

Take this clone function:

```
pure func clone<'o, 'l, E>(list 'l &List<E>) 'o List<E>
where pure func clone<'e, 'o>('e &E) 'o E 'c {
  return 'o List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

A lot of functions kind of look like this, where they have a region for every parameter (minus perhaps the primitives) and another region for the return.

Let's make that automatic for pure functions. It then becomes:

```
pure func clone<E>(list &List<E>) List<E>
where pure func clone(&E)E {
  return List<E>(Array<mut, E>(list.len(), { list.get(_).clone() }));
}
```

We might need some annotations if we want to return a reference to one of the parameters, or something inside them that isn't a generic parameter.


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

With full regions it would look like:

```
#!DeriveStructDrop
struct RHashMap<K Ref imm, V, H, E>
where pure func<'x>('x &H, 'x &K)int,
    pure func<'x>('x &E, 'x &K, 'x &K)bool
{
  hasher H;
  equator E;
  table! Array<mut, Opt<RHashMapNode<K, V>>>;
  size! int;
}
```

One would *think* that this means we need generic concept functions. However, we don't, because of RAAGP.


# Specifying Regions In Expressions (SRIE)

Let's say we have this ship:

```
struct Ship { fuel int; }
```

Making a new region:

```
region 'a;
```

Putting something into that new region:

```
list = 'a List<'a Ship>(0);
```

The `'a` in front is actually specifying the output region. Without it, it would assume the function's default region.

The `'a` in front won't affect any of the arguments. It's an arbitrary decision, we can change it later.

The `'a` in the `'a Ship` is needed for now. We'll see if we can infer it later.











we cant just use a placeholder everywhere we currently use a coord
we need that coord's ownership

we'll also want the region's mutability.. maybe we'll want the same arrangement?

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