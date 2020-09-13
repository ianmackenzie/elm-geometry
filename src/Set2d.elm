module Set2d exposing
    ( Set2d
    , empty, singleton, fromList, fromPairs
    , isEmpty, size, boundingBox
    , toList, fold
    , search, overlapping, BoundsPredicate, SearchOperation(..), boundsPredicate, allOf, anyOf
    )

{-| This modules provides a way to efficiently search through large numbers of
2D geometric objects. For example, you can construct a set of tens of thousands
of 2D triangles and quickly search for all triangles within a given bounding
box.

@docs Set2d


# Construction

@docs empty, singleton, fromList, fromPairs


# Properties

@docs isEmpty, size, boundingBox


# Traversal

@docs toList, fold


# Searching

@docs search, overlapping, BoundsPredicate, SearchOperation, boundsPredicate, allOf, anyOf

-}

import BoundingBox2d exposing (BoundingBox2d)
import Geometry.Types as Types
import Quantity exposing (Quantity(..))


{-| A set of geometric objects of some type `a`, in a coordinate system with the
given `units` and `coordinates` type. The `units` and `coordinates` types will
often end up being repeated in the full type - for example a set of `Triangle3d`
values might be a

    Set2d Meters WorldCoordinates (Triangle3d Meters WorldCoordinates)

-}
type Set2d units coordinates a
    = Set2d (Node units coordinates a)
    | Empty


type Node units coordinates a
    = Leaf ( a, BoundingBox2d units coordinates )
    | Node (BoundingBox2d units coordinates) (Node units coordinates a) (Node units coordinates a)


{-| A set with nothing in it.
-}
empty : Set2d units coordinates a
empty =
    Empty


singleton : (a -> BoundingBox2d units coordinates) -> a -> Set2d units coordinates a
singleton bounds item =
    Set2d (Leaf ( item, bounds item ))


{-| Construct a set from a list of items, given a function that computes the
bounding box around each item. For example, if you had a list of `Triangle2d`
values you might use

    Set2d.fromList Triangle2d.boundingBox listOfTriangles

and if you had a list of `Point2d` values you might use

    Set2d.fromList BoundingBox2d.singleton listOfPoints

-}
fromList : (a -> BoundingBox2d units coordinates) -> List a -> Set2d units coordinates a
fromList bounds items =
    fromPairs (List.map (\item -> ( item, bounds item )) items)


{-| Construct a set given a list of pairs of items and their bounding boxes.
-}
fromPairs : List ( a, BoundingBox2d units coordinates ) -> Set2d units coordinates a
fromPairs pairs =
    case pairs of
        first :: rest ->
            let
                count =
                    List.length pairs
            in
            Set2d (fromPairsImpl count SortX first rest)

        [] ->
            Empty


type alias Pair units coordinates a =
    ( a, BoundingBox2d units coordinates )


type SortAxis
    = SortX
    | SortY


cycle : SortAxis -> SortAxis
cycle sortAxis =
    case sortAxis of
        SortX ->
            SortY

        SortY ->
            SortX


midX : Pair units coordinates a -> Quantity Float units
midX ( _, bounds ) =
    BoundingBox2d.midX bounds


midY : Pair units coordinates a -> Quantity Float units
midY ( _, bounds ) =
    BoundingBox2d.midY bounds


sortPredicate : SortAxis -> Pair units coordinates a -> Quantity Float units
sortPredicate sortAxis =
    case sortAxis of
        SortX ->
            midX

        SortY ->
            midY


fromPairsImpl : Int -> SortAxis -> Pair units coordinates a -> List (Pair units coordinates a) -> Node units coordinates a
fromPairsImpl count sortAxis first rest =
    case rest of
        [] ->
            Leaf first

        _ ->
            let
                sorted =
                    Quantity.sortBy (sortPredicate sortAxis) (first :: rest)

                leftCount =
                    count // 2

                rightCount =
                    count - leftCount
            in
            case ( List.take leftCount sorted, List.drop leftCount sorted ) of
                ( leftFirst :: leftRest, rightFirst :: rightRest ) ->
                    Node (BoundingBox2d.aggregateOf Tuple.second first rest)
                        (fromPairsImpl leftCount (cycle sortAxis) leftFirst leftRest)
                        (fromPairsImpl rightCount (cycle sortAxis) rightFirst rightRest)

                _ ->
                    -- Should never happen
                    Leaf first


{-| Check if a given set is empty.
-}
isEmpty : Set2d units coordinates a -> Bool
isEmpty set =
    set == Empty


{-| Iterate through all items in a given set. The order of iteration is
unspecified.
-}
fold : (a -> b -> b) -> b -> Set2d units coordinates a -> b
fold function init set =
    case set of
        Set2d root ->
            foldImpl function init root

        Empty ->
            init


foldImpl : (a -> b -> b) -> b -> Node units coordinates a -> b
foldImpl function current node =
    case node of
        Leaf ( item, _ ) ->
            function item current

        Node _ left right ->
            foldImpl function (foldImpl function current left) right


{-| Get all items in a set as a list.
-}
toList : Set2d units coordinates a -> List a
toList set =
    case set of
        Set2d root ->
            collectItems root []

        Empty ->
            []


collectItems : Node units coordinates a -> List a -> List a
collectItems node accumulated =
    case node of
        Leaf ( item, _ ) ->
            item :: accumulated

        Node _ left right ->
            accumulated
                |> collectItems right
                |> collectItems left


{-| Get the net bounding box containing all items in a set. Will return
`Nothing` if the set is empty.
-}
boundingBox : Set2d units coordinates a -> Maybe (BoundingBox2d units coordinates)
boundingBox set =
    case set of
        Set2d (Node bounds _ _) ->
            Just bounds

        Set2d (Leaf ( _, bounds )) ->
            Just bounds

        Empty ->
            Nothing


{-| Get the total number of items in a set.
-}
size : Set2d units coordinates a -> Int
size set =
    case set of
        Set2d root ->
            sizeImpl root

        Empty ->
            0


sizeImpl : Node units coordinates a -> Int
sizeImpl node =
    case node of
        Leaf _ ->
            1

        Node _ left right ->
            sizeImpl left + sizeImpl right


{-| A `Set2d` is internally a [binary tree](https://en.wikipedia.org/wiki/Binary_tree)
where each tree node stores a bounding box that contains all of its children.
For example, the root node stores a bounding box that contains all items in the
entire set.

Searching a `Set2d` involves visiting each node of the binary tree and then,
based on the bounding box of that node, deciding to do one of three things. For
example, say we were searching a set of `Point2d` values trying to find those
within a given search box:

  - If the bounding box of some node is fully contained in the given search box,
    then it is guaranteed that every point that is a child of that node must
    also be contained in the given bounding box. In this case, we can collect
    all points that are children of the current node without doing any further
    checks.
  - If, on the other hand, the node's bounding box doesn't overlap the given
    search box at all, then _none_ of the child points can possibly be within
    the given search box and there is no need to search any further.
  - Finally, if the node's bounding box partially overlaps the given search box,
    then we need to investigate further - some points below this node are
    within the search box but some are not.

The `SearchOperation` type defines which of these three should be done when
visiting each node of the tree.

-}
type SearchOperation
    = KeepAll
    | DiscardAll
    | InvestigateFurther


type BoundsPredicate units coordinates
    = BoundsPredicate (BoundingBox2d units coordinates -> SearchOperation)
    | AllOf (List (BoundsPredicate units coordinates))
    | AnyOf (List (BoundsPredicate units coordinates))


evaluate : BoundsPredicate units coordinates -> BoundingBox2d units coordinates -> SearchOperation
evaluate givenPredicate bounds =
    case givenPredicate of
        BoundsPredicate function ->
            function bounds

        AllOf subPredicates ->
            evaluateAllOf subPredicates bounds KeepAll

        AnyOf subPredicates ->
            evaluateAnyOf subPredicates bounds DiscardAll


and : SearchOperation -> SearchOperation -> SearchOperation
and first second =
    case first of
        KeepAll ->
            second

        DiscardAll ->
            DiscardAll

        InvestigateFurther ->
            case second of
                DiscardAll ->
                    DiscardAll

                _ ->
                    InvestigateFurther


or : SearchOperation -> SearchOperation -> SearchOperation
or first second =
    case first of
        KeepAll ->
            KeepAll

        DiscardAll ->
            second

        InvestigateFurther ->
            case second of
                KeepAll ->
                    KeepAll

                _ ->
                    InvestigateFurther


evaluateAllOf : List (BoundsPredicate units coordinates) -> BoundingBox2d units coordinates -> SearchOperation -> SearchOperation
evaluateAllOf boundsPredicates bounds currentResult =
    case boundsPredicates of
        first :: rest ->
            let
                firstResult =
                    case first of
                        BoundsPredicate function ->
                            function bounds

                        AllOf subPredicates ->
                            evaluateAllOf subPredicates bounds currentResult

                        AnyOf subPredicates ->
                            evaluateAnyOf subPredicates bounds DiscardAll

                updatedResult =
                    and currentResult firstResult
            in
            case updatedResult of
                DiscardAll ->
                    DiscardAll

                _ ->
                    evaluateAllOf rest bounds updatedResult

        [] ->
            currentResult


evaluateAnyOf : List (BoundsPredicate units coordinates) -> BoundingBox2d units coordinates -> SearchOperation -> SearchOperation
evaluateAnyOf boundsPredicates bounds currentResult =
    case boundsPredicates of
        first :: rest ->
            let
                firstResult =
                    case first of
                        BoundsPredicate function ->
                            function bounds

                        AllOf subPredicates ->
                            evaluateAllOf subPredicates bounds KeepAll

                        AnyOf subPredicates ->
                            evaluateAnyOf subPredicates bounds currentResult

                updatedResult =
                    or currentResult firstResult
            in
            case updatedResult of
                KeepAll ->
                    KeepAll

                _ ->
                    evaluateAllOf rest bounds updatedResult

        [] ->
            currentResult


{-| Search a set for items matching some particular criteria.
-}
search : BoundsPredicate units coordinates -> (a -> Bool) -> Set2d units coordinates a -> List a
search givenBoundsPredicate givenItemPredicate set =
    case set of
        Set2d root ->
            searchImpl givenBoundsPredicate givenItemPredicate root []

        Empty ->
            []


searchImpl : BoundsPredicate units coordinates -> (a -> Bool) -> Node units coordinates a -> List a -> List a
searchImpl givenBoundsPredicate givenItemPredicate node accumulated =
    case node of
        Leaf ( item, bounds ) ->
            case evaluate givenBoundsPredicate bounds of
                KeepAll ->
                    item :: accumulated

                DiscardAll ->
                    accumulated

                InvestigateFurther ->
                    if givenItemPredicate item then
                        item :: accumulated

                    else
                        accumulated

        Node bounds left right ->
            case evaluate givenBoundsPredicate bounds of
                KeepAll ->
                    accumulated
                        |> collectItems right
                        |> collectItems left

                DiscardAll ->
                    accumulated

                InvestigateFurther ->
                    accumulated
                        |> searchImpl givenBoundsPredicate givenItemPredicate right
                        |> searchImpl givenBoundsPredicate givenItemPredicate left


boundsPredicate : (BoundingBox2d units coordinates -> SearchOperation) -> BoundsPredicate units coordinates
boundsPredicate function =
    BoundsPredicate function


allOf : List (BoundsPredicate units coordinates) -> BoundsPredicate units coordinates
allOf givenPredicates =
    AllOf givenPredicates


anyOf : List (BoundsPredicate units coordinates) -> BoundsPredicate units coordinates
anyOf givenPredicates =
    AnyOf givenPredicates


overlapping : BoundingBox2d units coordinates -> BoundsPredicate units coordinates
overlapping givenBounds =
    boundsPredicate <|
        \nodeBounds ->
            if BoundingBox2d.isContainedIn givenBounds nodeBounds then
                KeepAll

            else if BoundingBox2d.intersects givenBounds nodeBounds then
                InvestigateFurther

            else
                DiscardAll
