module RefinementProofs.Knowledge exposing
    ( WithKnowledge
    , And, Or, Not, XOr, Implies
    , or, and, not
    , forgetNamedKnowledgeAndName
    , name2WithKnowledge
    , forget, imply
    , A, NoDomainKnowledge, NoNamedKnowledge, NoValueKnowledge, Proof, andIsFlippable, attachNamedKnowledge, axiomaticDomainKnowledge, axiomaticNamedKnowledge, axiomaticValueKnowledge, axiomaticallyAddDomainKnowledge, axiomaticallySetDomainKnowledge, d_modusPonens, d_since, detachNamedKnowledge, forgetNamedKnowledge, makeProof, n_elimAndL, n_elimAndR, n_introOrL, n_introOrR, n_inverse, n_makeAnd, n_makeOr, n_modusPonens, n_modusTollens, n_since, n_sinceNot, name, name2, raw, setNamedKnowledge, the, v_elimAndL, v_elimAndR, v_introAnd, v_introOrL, v_introOrR, v_inverse, v_makeAnd, v_makeOr, v_modusPonens, v_modusTollens, v_since, v_sinceNot, withName, withNoKnowledge
    )

{-| This library allows for more knowledge to be captured in the types, for both library writers and application coders.


# Definition

@docs WithKnowledge


# Logic operator types

@docs And, Or, Not, XOr, Implies


# Basic function

@docs axiom, or, and, not


# Logic operators

@docs introOrR, introOrL, makeOr, makeAnd, inverse, elimAndL, elimAndR, forget, modusPonens, since, modusTollens, sinceNot, imply

-}


{-| WithKnowledge make it possible to express stuff we know about `value` and more:

`valueKnowledge`, The actual value of `value` satisfies everything in `valueKnowledge`. For example that a string is not empty

`domainKnowledge`:

  - Knowledge that can be difficult to formally capture but that we have checked somewhere in the code. For example if the animal is cute (whatever that means in our domain)
  - Also it allows us to return knowledge that is either previously named or captured. (There is a compiler reason for why we cannot return named knowledge outsidet the context given by the `name` function)

`namedKnowledge`, Formally capture knowledge of something. Note that this can be knowledge of other values, since the type system keeps track of it. _**If the rules below are followed**_


#### Important!

  - The constructors for all captured knowledge must _**never**_ be exported! That is constructors akin to `Positive` or `NonEmpty`. _**It is not unlikely that you will add your own types for captured knowledge (perhaps `IsOneOfMyFavoriteAnimals`), when you do - keep the constructors hidden, keep constructors safe**_
  - Do not use a lambda for the function input to _**`name`**_. It must be a function with an _**explicit function signature**_!
  - Make sure that _**all names**_ are correct and _**unique**_ (excluding NamedKnowledge, the return value and per function signature), that will probably _**break**_ the type safety. There are a couple of safety mechanisms but do not count on them.


#### More info

The usage of `namedKnowledge` allows us to capture the knowledge on how different values interact. For example if a value is known to be in a dictionary

To get more information about named knowledge check name, A, makeProof, attachNamedKnowledge

-}
type WithKnowledge value valueKnowledge domainKnowledge namedKnowledge
    = WithKnowledge value


{-| Just the basic case when nothing is known about a value. This is used for three types of knowledge (`valueKnowledge domainKnowledge namedKnowledge`)
-}
type NoKnowledge
    = NoKnowledge


{-| Just the basic case when no value knowledge is available.
-}
type alias NoValueKnowledge =
    NoKnowledge


{-| Just the basic case when no domain knowledge is available.
-}
type alias NoDomainKnowledge =
    NoKnowledge


{-| Just the basic case when no named knowledge is available.
-}
type alias NoNamedKnowledge =
    NoKnowledge


{-| A negation of a proof
-}
type Not p
    = Not


{-| Exlusive or
-}
type alias XOr p1 p2 =
    Or (And p1 (Not p2)) (And (Not p1) p2)


{-| Implies that if "p1" holds than "p2" holds
-}
type Implies p1 p2
    = Implies


{-| Both "p1" and "p2" holds
-}
type And p1 p2
    = And


{-| "p1" or "p2" holds
-}
type Or p1 p2
    = Or


type Proof p
    = Proof


{-| Used to wrap a value in a WithKnowledge type with no associated knowledge
-}
withNoKnowledge : a -> WithKnowledge a NoValueKnowledge NoDomainKnowledge NoNamedKnowledge
withNoKnowledge x =
    WithKnowledge x


{-| Used by library writers to create proofs for named knowledge with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
makeProof : p -> Proof p
makeProof _ =
    Proof


{-| Detaches named knowledge. This is safe and a library writer should expect and allow its users to do this.
-}
detachNamedKnowledge : WithKnowledge a s c p -> Proof p
detachNamedKnowledge _ =
    Proof


{-| Attaches named knowledge. This is safe and a library writer should expect and allow its users to do this.
-}
attachNamedKnowledge : Proof n1 -> WithKnowledge a s c n -> WithKnowledge a s c (And n n1)
attachNamedKnowledge _ x =
    axiomInternal <| forget x


{-| Sets named knowledge, forgetting all previous named knowledge. This is always safe and a library writer should always expect and allow its users to do this.
-}
setNamedKnowledge : Proof n2 -> WithKnowledge a s d n1 -> WithKnowledge a s d n2
setNamedKnowledge _ x =
    axiomInternal <| forget x


{-| Used by library writers to add domain knowledge with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticallyAddDomainKnowledge : d2 -> WithKnowledge a s d1 n -> WithKnowledge a s (And d1 d2) n
axiomaticallyAddDomainKnowledge _ x =
    axiomInternal <| forget x


{-| Used by library writers to set domain knowledge with non-exported constructors, forgetting all previous domain knowledge.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticallySetDomainKnowledge : d2 -> WithKnowledge a s d1 n -> WithKnowledge a s d2 n
axiomaticallySetDomainKnowledge _ x =
    axiomInternal <| forget x


{-| This is used to name a type to allow us to express relations between different types. For example that an value is a key in a given dictonary.

You create a named variable with the `name` or `name2` functions

It is always safe to add a name to an value, just remember that it must live in the specific context created by the `name` functions

-}
type A a name
    = A a


{-| Remove the name from a value. This is always safe.
-}
the : A a name -> a
the (A x) =
    x


{-| Remove all knowledge and the name from a value. This is always safe.
-}
raw : WithKnowledge (A a name) v d n -> a
raw =
    the << forget


{-| This is used to name a type to allow us to express relations between different types. For example that an value is a key in a given dictonary.


### Do not use a lambda for the function input. It must be a function with an explicit function signature!


### Make sure that all names are correct and no two values (excluding NamedKnowledge) uses the same two name, that will _**break**_ the type safety.

-}
name : a -> (A a c -> b) -> b
name x f =
    f (A x)


{-| This is used to name two types to allow us to express relations between different types. For example that an value is a key in a given dictonary.


### Do not use a lambda for the function input. It must be a function with an explicit function signature!


### Make sure that all names are correct and no two values (excluding NamedKnowledge) uses the same two name, that will _**break**_ the type safety.

-}
name2 : a1 -> a2 -> (A a1 c1 -> A a2 c2 -> b) -> b
name2 x y f =
    name x
        (\named1 ->
            name y
                (\named2 ->
                    f named1 named2
                )
        )

{-| This is used to name two types to allow us to express relations between different types. For example that an value is a key in a given dictonary.


### Do not use a lambda for the function input. It must be a function with an explicit function signature!


### Make sure that all names are correct and no two values (excluding NamedKnowledge) uses the same two name, that will _**break**_ the type safety.

-}
name2WithKnowledge : WithKnowledge a1 v1 d1 n1 -> WithKnowledge a2 v2 d2 n2 -> (WithKnowledge (A a1 an1) v1 d1 n1 -> WithKnowledge (A a2 an2) v2 d2 n2  -> b) -> b
name2WithKnowledge x y f =
    name (forget x)
        (\named1 ->
            name (forget y)
                (\named2 ->
                    f (axiomInternal named1) (axiomInternal named2)
                )
        )


{-| This is used to use functions that add knowledge to a value without a name to work when we have a name attached.
-}
withName : (a -> Maybe (WithKnowledge a v d n)) -> A a name -> Maybe (WithKnowledge (A a name) v d n)
withName f x =
    Maybe.map (WithKnowledge << A << forget) <| f (the x)


{-| Used by library writers to create axioms with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticValueKnowledge : v -> a -> WithKnowledge a v NoKnowledge NoKnowledge
axiomaticValueKnowledge _ x =
    WithKnowledge x


{-| Used by library writers to create axioms with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticDomainKnowledge : d -> a -> WithKnowledge a NoKnowledge d NoKnowledge
axiomaticDomainKnowledge _ x =
    WithKnowledge x


{-| Used by library writers to create axioms with non-exported constructors.
Remember, the library/module RefinementProofs.constructors must not be exported!
-}
axiomaticNamedKnowledge : n -> a -> WithKnowledge a NoKnowledge NoKnowledge n
axiomaticNamedKnowledge _ x =
    WithKnowledge x


{-| DANGER ZONE!
Used internally by this module RefinementProofs.only to prove general logic. Must never be exported and used with extreme care!
-}
axiomInternal : a -> WithKnowledge a s c n
axiomInternal x =
    WithKnowledge x


{-| Used by library writers to create build a Or proof
-}
or : p1 -> p2 -> Or p1 p2
or _ _ =
    Or


{-| Used by library writers to create build a And proof
-}
and : p1 -> p2 -> And p1 p2
and _ _ =
    And


{-| Used by library writers to create build a Not proof
-}
not : p1 -> Not p1
not _ =
    Not


{-| For named knowledge. If a is proven to hold property p1 then classic logic gives that a holds either p1 or p2
-}
n_introOrR : WithKnowledge a v c p1 -> WithKnowledge a v c (Or p1 p2)
n_introOrR (WithKnowledge x) =
    axiomInternal x


{-| For named knowledge. Same as introOrR but introduces the p2 on the other side
-}
n_introOrL : WithKnowledge a v c p1 -> WithKnowledge a v c (Or p2 p1)
n_introOrL =
    axiomInternal << forget


{-| For named knowledge. A convience method to check two proofs at once for And
-}
n_makeAnd : (a -> Maybe (WithKnowledge a v d b)) -> (a -> Maybe (WithKnowledge a v d c)) -> a -> Maybe (WithKnowledge a v d (And b c))
n_makeAnd f g x =
    case ( f x, g x ) of
        ( Just _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| For named knowledge. A convience method to check two proofs for Or
-}
n_makeOr : (a -> Maybe (WithKnowledge a v d b)) -> (a -> Maybe (WithKnowledge a v d c)) -> a -> Maybe (WithKnowledge a v d (Or b c))
n_makeOr f g x =
    case ( f x, g x ) of
        ( Just _, _ ) ->
            Just <| axiomInternal x

        ( _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| For named knowledge. Used by library writers to show that when `p1` does not hold for a then `Not p1` holds.
-}
n_inverse : p1 -> (a -> Maybe (WithKnowledge a v c p1)) -> a -> Maybe (WithKnowledge a v c (Not p1))
n_inverse _ f x =
    case f x of
        Just _ ->
            Nothing

        Nothing ->
            Just <| axiomInternal x


{-| For named knowledge. Remove the left hand side of an And proof
-}
n_elimAndL : WithKnowledge a v c (And b1 b2) -> WithKnowledge a v c b1
n_elimAndL =
    axiomInternal << forget


{-| For named knowledge. Remove the right hand side of an And proof
-}
n_elimAndR : WithKnowledge a v c (And b1 b2) -> WithKnowledge a v c b2
n_elimAndR =
    axiomInternal << forget


{-| For named knowledge. If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
n_modusPonens : Implies p1 p2 -> WithKnowledge a v c p1 -> WithKnowledge a v c p2
n_modusPonens _ =
    axiomInternal << forget


{-| For named knowledge. Synonym for "modus ponens". If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
n_since : Implies p1 p2 -> WithKnowledge a v c p1 -> WithKnowledge a v c p2
n_since =
    n_modusPonens


{-| For named knowledge. If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
n_modusTollens : Implies p1 p2 -> WithKnowledge a v c (Not p1) -> WithKnowledge a v c (Not p2)
n_modusTollens _ =
    axiomInternal << forget


{-| For named knowledge. Synonym for "modus tollens". If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
n_sinceNot : Implies p1 p2 -> WithKnowledge a v c (Not p1) -> WithKnowledge a v c (Not p2)
n_sinceNot =
    n_modusTollens


{-| Used by library writers to imply proof relations
-}
imply : p1 -> p2 -> Implies p1 p2
imply _ _ =
    Implies


{-| Extract the actual value and remove all captured knowledge. This is always safe and a library writer should always expect and allow its users to do this.
-}
forget : WithKnowledge a v c n -> a
forget (WithKnowledge x) =
    x


{-| Remove named knowledge. This is always safe and a library writer should always expect and allow its users to do this.
-}
forgetNamedKnowledge : WithKnowledge a v c n -> WithKnowledge a v c NoNamedKnowledge
forgetNamedKnowledge (WithKnowledge x) =
    WithKnowledge x

{-| Remove named knowledge along with the name. This is always safe and a library writer should always expect and allow its users to do this.
-}
forgetNamedKnowledgeAndName : WithKnowledge (A a name) v c n -> WithKnowledge a v c NoNamedKnowledge
forgetNamedKnowledgeAndName (WithKnowledge x) =
    WithKnowledge <| the x


{-| For value knowledge. If a is proven to hold property p1 then classic logic gives that a holds either p1 or p2
-}
v_introOrR : WithKnowledge a v1 c n -> WithKnowledge a (Or v1 v2) c n
v_introOrR (WithKnowledge x) =
    axiomInternal x


{-| For value knowledge. Same as introOrR but introduces the p2 on the other side
-}
v_introOrL : WithKnowledge a v1 c p -> WithKnowledge a (Or v2 v1) c p
v_introOrL =
    axiomInternal << forget


{-| For value knowledge. A convience method to check two proofs at once for And
-}
v_makeAnd : (a -> Maybe (WithKnowledge a v1 d b)) -> (a -> Maybe (WithKnowledge a v2 d b)) -> a -> Maybe (WithKnowledge a (And v1 v2) d b)
v_makeAnd f g x =
    case ( f x, g x ) of
        ( Just _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| For value knowledge. Introduce another Value knowledge
-}
v_introAnd : (a -> Maybe (WithKnowledge a v1 d b)) -> WithKnowledge a v2 d b -> Maybe (WithKnowledge a (And v2 v1) d b)
v_introAnd f x =
    case f <| forget x of
        Just _ ->
            Just <| axiomInternal <| forget x

        _ ->
            Nothing


{-| For value knowledge. A convience method to check two proofs for Or
-}
v_makeOr : (a -> Maybe (WithKnowledge a v1 d b)) -> (a -> Maybe (WithKnowledge a v2 d b)) -> a -> Maybe (WithKnowledge a (Or v1 v2) d b)
v_makeOr f g x =
    case ( f x, g x ) of
        ( Just _, _ ) ->
            Just <| axiomInternal x

        ( _, Just _ ) ->
            Just <| axiomInternal x

        _ ->
            Nothing


{-| For value knowledge. Used by library writers to show that when `p1` does not hold for a then `Not p1` holds.
-}
v_inverse : v -> (a -> Maybe (WithKnowledge a v c p1)) -> a -> Maybe (WithKnowledge a (Not v) c p1)
v_inverse _ f x =
    case f x of
        Just _ ->
            Nothing

        Nothing ->
            Just <| axiomInternal x


{-| For value knowledge. Remove the left hand side of an And proof
-}
v_elimAndL : WithKnowledge a (And b1 b2) c n -> WithKnowledge a b1 c n
v_elimAndL =
    axiomInternal << forget


{-| For value knowledge. Remove the right hand side of an And proof
-}
v_elimAndR : WithKnowledge a (And b1 b2) c n -> WithKnowledge a b2 c n
v_elimAndR =
    axiomInternal << forget


{-| For value knowledge. If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
v_modusPonens : Implies v1 v2 -> WithKnowledge a v1 c p -> WithKnowledge a v2 c p
v_modusPonens _ =
    axiomInternal << forget


{-| For value knowledge. Synonym for "modus ponens". If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
v_since : Implies v1 v2 -> WithKnowledge a v1 c p -> WithKnowledge a v2 c p
v_since =
    v_modusPonens


{-| For value knowledge. If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
v_modusTollens : Implies v1 v2 -> WithKnowledge a (Not v1) c p -> WithKnowledge a (Not v2) c p
v_modusTollens _ =
    axiomInternal << forget


{-| For value knowledge. Synonym for "modus tollens". If a library implies that when p1 holds the p2 holds then you have proven (Not p2) by prove (Not p1)
-}
v_sinceNot : Implies v1 v2 -> WithKnowledge a (Not v1) c p -> WithKnowledge a (Not v2) c p
v_sinceNot =
    v_modusTollens


{-| For domain knowledge. If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
d_modusPonens : Implies d1 d2 -> WithKnowledge a v d1 p -> WithKnowledge a v d2 p
d_modusPonens _ =
    axiomInternal << forget


{-| For domain knowledge. Synonym for "modus ponens". If a library implies that when p1 holds the p2 holds then you have proven p2 by prove p1
-}
d_since : Implies d1 d2 -> WithKnowledge a v d1 p -> WithKnowledge a v d2 p
d_since =
    d_modusPonens


{-| And is always flippable
-}
andIsFlippable : Implies (And a b) (And b a)
andIsFlippable =
    Implies
