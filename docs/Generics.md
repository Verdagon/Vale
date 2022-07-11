
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



# Hardcode Zero, empty string, Final, mutable?

hardcoding mutable might be weird.

also, we cant just make that kind of assumption, mutability isnt covariant.

a mutable SSA might operate different than an immutable SSA, right? cant really think it through


# IntTemplata contains Placeholder|int, BoolTemplata contains Placeholder|bool etc.

Just like we're doing with kind and coord and stuff.

perhaps this could be a good stepping stone to full rules?


# PlaceholderInt, PlaceholderKind, etc. all subclasses of ITemplata

Locals, members, and parameters would need to be ITemplata


# PlaceholderOr[ITemplata]

Locals, members, and parameters would need to be this.

perhaps this could be a good stepping stone to full rules?

# IGenericData: VariableGenericData or LiteralGenericData

Locals, members, and parameters would need to be this.


# PlaceholderT kind, ITemplata/PlaceholderTemplata subclasses for everything else?

means locals, members, parameters wouldnt need to do anything different.

but StaticSizedArrayT would need to contain e.g. ITemplata[OwnershipTemplata]


# All-in on Runes

we could have everything be a rune, and all constants would be in the env.
in fact, that would make things closer to the rule system.
thats an interesting thought.

oddly enough, we're already kind of there. post-scout, everything is already runes. presumably iexpression would become a bunch of rules? i suppose it doesnt really need to. for now, all constants can be 



