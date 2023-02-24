
# How Regions Are Lowered In Instantiator (HRALII)

Note from later: we dont change the coord's ownership anymore, they stay borrow and share. To know the mutability we need to look at the current pure stack height and compare it to the coord's region's pure stack height.

Instantiator lowers region placeholders to RegionTemplata(mutable). However, this is the mutability of the region _at the time the thing was created_. A `List<mut&Ship>` is not a list of mutable references to ships, it's a List of references that it regards as mutable, but might not be mutable right now. Rather, it tracks what's mutable right now by putting that information in the coord's ownership, which can be owning, mutable borrow, or immutable borrow.

The instantiator is in a constant quest to simplify coords to just two things:

 * Ownership: owning, mutable borrow, immutable borrow.
 * Kind. Any regions in the template coords are reduced to booleans.

```
func main() { // main'
  ship Ship = ...;
  // Typing phase: own main'Ship<main'>
  // Instantiator: own Ship<mut'>

  list List<&Ship> = ...;
  // Typing phase: own main'List<&main'Ship<main'>, main'>
  // Instantiator: own List<mut&Ship<mut'>, mut'>

  pure { // p1'

    z = &list;
    // Typing phase: &main'List<&main'Ship<main'>, main'>
    // Instantiator: imm&List<mut&Ship<mut'>, mut'>

    listOfImms List<&main'List<&main'Ship>> = ...;
    // Typing phase: own p1'List<&main'List<&main'Ship<main'>, main'>, p1'>
    // Instantiator: own List<imm&List<mut&Ship<mut'>, mut'>, mut'>
  }
}
```

When the instantiator enters a pure block, **the coord's ownership is different.** The:

When you ignore all those irrelevant default region `mut'` things out, a:

`&main'List<&main'Ship<main'>, main'>` becomes:

`imm&List<mut&Ship<mut'>, mut'>`.

Note how the ownership is `imm&`.

However, **it doesn't actually see the surrounding kinds as different. The kinds are the same.** Think of the kind's template args as a snapshot of how the kind was made.

When you ignore all those irrelevant default region `mut'` things out, a:

`p1'List<&main'List<&main'Ship>>` becomes:

`own List<imm&List<mut&Ship>>`.

Note how the `mut&ship` is still `mut`. Rule of thumb: think of a kind's template args as how it sees _itself_. It doesn't mean it's mutable to us; we'd have to traverse through an immutable ref to get to it, and viewpoint adaptation keeps it immutable to us.


Also note how the **coord has no region information anymore**. They still appear in template args, but only as a mutable/immutable boolean. 


Other notes:

 * Yes, it's weird that every struct has a mut' at the end. It makes sense, you can only create a struct in a mutable region, and a struct's template args are a snapshot of when it was mutable.
 * In the above snippet, we say a simple `Ship` is an `own main'Ship<main'>`. That's because we still haven't decided whether the region should be in the kind only or the coord too. It seems orthogonal to this topic, luckily.
 * We do in fact need a mutability boolean in RegionTemplata. "What if we instead relied on the ownership part of the coord to distinguish between various instantiations with different region mutabilities?" Because if we have a `struct Moo<z', ShipKind> { ship z'ShipKind; }`, where ShipKind is a kind, we can't distinguish between a `Moo<imm', ShipKind>` and `Moo<mut', ShipKind>`. There are likely other hidden differences mutability can cause too.
 * (We probably shouldn't reuse the coord's region to store mutability, as that's used to describe what region a string is in. Let's tackle that problem separately.)


As you'd expect, a `LocalLoadHE` inside a `pure` block that loads from an outside local `mut'List<&mut'Ship>` will produce an `imm&mut'List<&mut'Ship>`.

Similarly, if we hand in a `mut'List<&mut'Ship>` to a pure function, it should receive it as a `imm&mut'List<&mut'Ship>`.


Alternative: Instantiator makes RegionTemplata(initialMutability, stackHeight). This isn't great because then we'd need to cast when we're calling a pure function.


### Rejected Alternative: Templar calculates mutability for LocalLookup

In the templar, we use the stack height to determine whether a given LocalLookup is loading from a now-immutable region.

Instantiator will then read it, and that's the only time it ever produces an immutable borrow for a coord ownership. If we hand an immutable borrow in to a regular borrow, we produce another immutable borrow, kind of virally.

The downside of this is that the instantiator has to inspect the incoming coords to an instruction to see if something is immutable.


### Rejected Alternative: Templar makes immutable borrowed coords

We can make the Templar populate coords with "immutable borrow" ownership. This is mostly when inside a pure block we read a pre-pure local, or when we're calling a pure function.

This would make the typing phase a little more complex. We now have immutability information in three places: ownership, region in coord, region in kind.

The advantage is that the instantiator doesn't have to calculate the immutable ownerships itself. Except it kind of does... when it sees an immutable borrow being handed into a readonly borrow, it'll have to produce another immutable borrow virally. Since it would have to do that anyway, this is no better than approach B.


### Rejected Alternative: RegionTemplatas contain originalMutability and stackHeight, like PlaceholderTemplatas

This doesn't quite work.

`main` might call it with a RegionTemplata(8, false), `bork` might call it with a RegionTemplata(6, false), and all sorts of other functions will be handing in different stack heights, different templatas. This means the instantiator will create way more versions of each function than we want. Additionally, those incoming stack heights make no sense for `printShip`. The instantiator will see it and be confused because they don't line up with the current function.

Instead, we could make it so **callers hand in 0 for the stack height** into function calls. In a way, we're creating a new region in a new world. It's kind of like we're creating a virtual region.

This would be done in the instantiator so the typing phase doesn't have to worry about any of this. It just checks mutabilities line up with what the callee expects and is done. The instantiator however has to do some special logic.

When the instantiator sees that a caller is doing a `pure` call, it changes its substitutions. RegionTemplata(8, true) becomes RegionTemplata(0, false).

The downside is that, we'll need to do some casting so the backend understands that we can hand in a `Ship<RegionTemplata(8, true)>` to a function that expects a `Ship<RegionTemplata(0, false)>`. The accepted solution doesn't have this weakness.


### Rejected Alternative: Update Template Args According to Current Mutability (UTAACM)

For example, a pure block wouldn't see a previous `&main'List<&main'Ship>` as `imm&List<mut&Ship>`, it would see it as `imm&List<imm&Ship>`. After all, that ship is immutable to us right now.

This seems to always end up needing some casting somewhere, especially in the backend. The accepted solution needs no casting in the backend.


### Rejected Alternative: Change Substitutions, Erase Regions from Types

(build on UTAACM)

Perhaps we could have something (a `pure` block perhaps) to signal the instantiator to change its substitutions. Instead of main' -> RegionTemplata(true), it needs to have main' -> RegionTemplata(false) for a certain scope. Then, it can call the receiving function that expects a RegionTemplata(false).

However, then a variable might have a different type inside the block than outside:

```
struct Ship { hp int; }
func main() {
  ship = Ship(42);
  // ship is a coord (mut main)'Ship<(mut main)'>
  pure {
    // ship is a coord (imm main)'Ship<(imm main)'>
    println(ship.hp);
  }
}
```

...which would confuse the backend when we try to read from ship. It would expect one type, and see another. So, we really shouldnt have the mutability appear in the templata.

Instead, let's erase all regions from the types, probably in instantiator. All RegionTemplatas for types are rewritten to true, as if they're mutable. Instantiator can say at an expression level whether to skip gen checks or not.

Three remaining minor downsides:

 * The typing phase itself will have trouble comparing the two. Let's say we return a reference from the pure block, we'd have to do some tricky logic to compare the result `(imm main)'Ship<(imm main)'>` can be received into a `(mut main)'Ship<(mut main)'>`.
 * The instantiator doesn't have enough information to sanity check the typing phase's outputs. That'll be nice to have when we distribute .vast files with the typing phase outputs.
 * We have to loop over the entire environment. Not too expensive, but the other approach is a bit faster.


# Can't Translate Outside Things From Inside Pure Blocks (CTOTFIPB)

(Note from later: we found a way to resolve this, see TTTDRM)

In this snippet:

```
struct Engine { fuel int; }
struct Spaceship { engine Engine; }

exported func main(s &Spaceship) int {
  pure block { s.engine.fuel }
}
```

Before the pure block, `s` is a `&main'Spaceship<main'>`, instantiated it's `mut&Spaceship<mut>`.

From inside the pure block, `s` is still a `&main'Spaceship<main'>`, but instantiated it's a `imm&Spaceship<mut>`. Note how only that first `mut` turned into an `imm`, because of HRALII.

However, the implementation can be trippy here. In the pure, when we do a LocalLookupTE, it contains a `ReferenceLocalVariableTE` which contains the type of the local, in this case `&main'Spaceship<main'>`. Of course, when we do the instantiation, we see `main'` is immutable, so it erroneously produces `imm&Spaceship<imm>`.

The answer is for the instantiator to keep track of the current type of every variable in scope, and not re-translate it inside the pure block like that.


The same applies to structs. ReferenceMemberLookupTE.memberCoord might be translated as `imm&Engine<imm>` when it needs to be `imm&Engine<mut>`.


The same applies to when we're translating anything from outside the pure block. In LocalVariableT, we have the full name of the variable, which includes the full name of the function. When we try to translate it, suddenly its mutable parameters are immutable, and the variable suddenly thinks it's in a different function.

So generally speaking, we shouldn't translate anything outside the pure block from within the pure block.

(Note from later: we found a way to resolve this, see TTTDRM)


# Time Travel To Determining Region Mutabilities (TTTDRM)

Recall HRALII, where a template argument that's a region becomes the mutability at the time the template was created.

Of course, that's difficult to do. If we mess it up we run into the CTOTFIPB problem.

Let's take this example:

```
struct Spaceship { fuel int; }

exported func main(s &main'List<&main'Spaceship>) int {
  pure block {
    z = s;
    z.engine.fuel
  }
}
```

`s` is a `mut&List<mut&Spaceship>`, but `z` needs to be an `imm&List<mut&Spaceship>` However, if inside the pure block we just use the current mutabilities of the `main'` region we erroneously end up with `imm&List<imm&Spaceship>`.

What we really need is to go back and calculate the mutabilities at the time the `List` was made.

Luckily, there's an easy way to do that. We just calculate List's template args' mutabilities **from the perspective of List's region**.

In other words:

 1. We know that `List` is in the `main'` region which is currently immutable, so it's an `imm&List<something>`.
 2. Now we start processing `List`'s template arguments there. But we do it **from the perspective** of `main'`. This is the "**perspective region**".
 3. We encounter the `&main'Spaceship`, and ask, "what is the mutability of `main'` from the perspective of the containing List?" in other words "what is the mutability of `main'` from `main'`?" And the answer is, of course, mutable.

So how do we calculate the mutability of one region from the perspective of another?

Conceptually, we need to figure out if there was a pure block introduced between those two regions. Instead of searching through the environment, let's just have RegionPlaceholderNameT **remember the location of the latest pure block** from its perspective. That way, we can just check if the latest pure block is before or after the other region.



# RegionTemplata Has Pure Stack Height (RTHPSH)

RegionTemplata(boolean) didnt quite work, because we couldn't determine (from just regions' booleans) the relationship between all the regions. We need to know what region is pure from what region's perspective, not just whether a region is mutable or not. When a function can only receive one boolean per region, it can't tell if one immutable region is immutable to another immutable region. (DO NOT SUBMIT TODO explain this more)

So instead, we'll have a RegionTemplata(int) where the int is how many pure blocks between here and there. Zero means mutable. One means there's one pure block between us and this region, and two means there are two pure blocks between us and this region.

If x' is 1 and y' is 2, then both are immutable, and y is immutable to x.

If x' is 1 and y' is 1, then both are immutable, and y is mutable to x.



# After Instantiator There Are No Regions (AITANR)

Under the hood, the instantiator is really just trying to figure out what references are currently immutable. The RegionTemplata(int) is mostly just an abstraction that helps humans reason about it better.

Well, "mostly" because there is one thing regions help us do that coding with `imm&` and `&` can't do: it helps the compiler know when it's safe to cast from `imm&` back to `&`, such as when we come back from a pure block. The compiler knows that if a pure block is returning something created just before itself, it can safely cast that to a mutable reference... but if a pure block is returning something created three pure blocks up, then it shouldn't cast that to a mutable reference.



the instantiator will still instantiate RegionTemplata(int) and hand them in as function signatures

when we do a function call, we'll map the local int to a receiver int. likely just an array. start at zero for the most immutable region. that will determine the template arguments

however, the CoordI will have no region, it will just have an immutability in its ownership.

we could mimic this by just always having RegionTemplata(-1) in the coords _produced_ by the instantiator.



we cant do that rewrite in instantiator because we dont have the right bound arguments registered for the thing we translate.


regiontemplata *must* be integers because otherwise the instantiator has no idea what the *actual* heights are. 1 2 3 or 1 2 2 or 1 1 1 etc. templar might say theyre all -1 -1 -1 in the definition site, needs actual heights.

inside the function we'll still have those RegionTemplata(1) RegionTemplata(2) RegionTemplata(3) etc.


# Region Generic Params Pure Heights Are Some Zero (RGPPHASZ)

(Some Zero means `Some(0)`)

A few places in the typing stage will need to know whether a region is mutable or not.

We could have put a `initiallyMutable` boolean in `RegionPlaceholderNameT` but it seemed nice to just use `Some(0)` for the pure height instead. It does however mean that not all region generic params are None.



