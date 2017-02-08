module Abstract.Database.Predicates
	( equals'
	, equals'Flipped
	, (==..)
	, equals
	, equalsFlipped
	, (==.)
	, notEquals'
	, notEquals'Flipped
	, (/=..)
	, (!=..)
	, notEquals
	, notEqualsFlipped
	, (/=.)
	, (!=.)
	, lessThan'
	, lessThan'Flipped
	, (<..)
	, lessThan
	, lessThanFlipped
	, (<.)
	, lessThanOrEquals'
	, lessThanOrEquals'Flipped
	, (<=..)
	, lessThanOrEquals
	, lessThanOrEqualsFlipped
	, (<=.)
	, greaterThan'
	, greaterThan'Flipped
	, (>..)
	, greaterThan
	, greaterThanFlipped
	, (>.)
	, greaterThanOrEquals'
	, greaterThanOrEquals'Flipped
	, (>=..)
	, greaterThanOrEquals
	, greaterThanOrEqualsFlipped
	, (>=.)
	, sortBy'
	, sortBy
	, custom'
	, custom
	, negate
	) where

import Prelude (class Eq, class Ord, Ordering, flip, map, ($), (>>>))

import Data.Either (Either(Left, Right))
import Data.List   (singleton)
import Data.Maybe  (Maybe(Nothing))

import Abstract.Database.Types
	( Clause
		( Equals
		, NotEquals
		, LessThan
		, LessThanOrEquals
		, GreaterThan
		, GreaterThanOrEquals
		, SortBy
		, Custom
		, Negate
		)
	, DBObject(Maybe)
	, LogicConnector(And)
	, Query(Query)
	, SortOrder
	)
import Abstract.Database.Class
	( class FromDBObject
	, class ToDBObject
	, toDBObject
	)

type LeftPredicate datastore t a = a -> String -> Query datastore t 

type LeftPredicateFlipped datastore t a = String -> a -> Query datastore t

type RightPredicate datastore t a = a -> (t -> a) -> Query datastore t

type RightPredicateFlipped datastore t a = (t -> a) -> a -> Query datastore t

leftPredicate :: forall datastore t a. (FromDBObject t, ToDBObject a) =>
		 Clause                                               ->
		 LeftPredicate datastore t a
leftPredicate clause clauseValue fieldName = Query $ singleton $
	{ clause        : clause
	, logicConnector: And
	, clauseValue   : toDBObject clauseValue
	, predicate     : Left fieldName
	}

rightPredicate :: forall datastore t a. (FromDBObject t, ToDBObject a) =>
		  Clause                                               ->
		  RightPredicate datastore t a
rightPredicate clause clauseValue fn = Query $ singleton $
	{ clause        : clause
	, logicConnector: And
	, clauseValue   : toDBObject clauseValue
	, predicate     : Right (fn >>> toDBObject)
	}

equals' :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
	   LeftPredicate datastore t e
equals' = leftPredicate Equals

equals'Flipped :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
		  LeftPredicateFlipped datastore t e
equals'Flipped = flip equals'
infix 4 equals'Flipped as ==..

equals :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
	  RightPredicate datastore t e
equals = rightPredicate Equals

equalsFlipped :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
		 RightPredicateFlipped datastore t e
equalsFlipped = flip equals
infix 4 equalsFlipped as ==.

notEquals' :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
	      LeftPredicate datastore t e
notEquals' = leftPredicate NotEquals

notEquals'Flipped :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
		     LeftPredicateFlipped datastore t e
notEquals'Flipped = flip notEquals'
infix 4 notEquals'Flipped as /=..
infix 4 notEquals'Flipped as !=.. -- For the non-haskellers.

notEquals :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
	  RightPredicate datastore t e
notEquals = rightPredicate NotEquals

notEqualsFlipped :: forall datastore t e. (Eq e, FromDBObject t, ToDBObject e) =>
		    RightPredicateFlipped datastore t e
notEqualsFlipped = flip notEquals
infix 4 notEquals' as /=.
infix 4 notEquals' as !=. -- For the non-haskellers.

lessThan' :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
	     LeftPredicate datastore t o
lessThan' = leftPredicate LessThan

lessThan'Flipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
		    LeftPredicateFlipped datastore t o
lessThan'Flipped = flip lessThan'
infixl 4 lessThan'Flipped as <..

lessThan :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
	    RightPredicate datastore t o
lessThan = rightPredicate LessThan

lessThanFlipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
	    RightPredicateFlipped datastore t o
lessThanFlipped = flip lessThan
infixl 4 lessThanFlipped as <.

lessThanOrEquals' :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
		     LeftPredicate datastore t o
lessThanOrEquals' = leftPredicate LessThanOrEquals

lessThanOrEquals'Flipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
			    LeftPredicateFlipped datastore t o
lessThanOrEquals'Flipped = flip lessThanOrEquals'
infixl 4 lessThanOrEquals'Flipped as <=..

lessThanOrEquals :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
		    RightPredicate datastore t o
lessThanOrEquals = rightPredicate LessThanOrEquals

lessThanOrEqualsFlipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
			   RightPredicateFlipped datastore t o
lessThanOrEqualsFlipped = flip lessThanOrEquals
infixl 4 lessThanOrEqualsFlipped as <=.

greaterThan' :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
		LeftPredicate datastore t o
greaterThan' = leftPredicate GreaterThan

greaterThan'Flipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
		       LeftPredicateFlipped datastore t o
greaterThan'Flipped = flip greaterThan'
infixl 4 greaterThan'Flipped as >..

greaterThan :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
	       RightPredicate datastore t o
greaterThan = rightPredicate GreaterThan

greaterThanFlipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
		      RightPredicateFlipped datastore t o
greaterThanFlipped = flip greaterThan
infixl 4 greaterThanFlipped as >.

greaterThanOrEquals' :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
			LeftPredicate datastore t o
greaterThanOrEquals' = leftPredicate GreaterThanOrEquals

greaterThanOrEquals'Flipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
			       LeftPredicateFlipped datastore t o
greaterThanOrEquals'Flipped = flip greaterThanOrEquals'
infixl 4 greaterThanOrEquals'Flipped as >=..

greaterThanOrEquals :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
		       RightPredicate datastore t o
greaterThanOrEquals = rightPredicate GreaterThanOrEquals

greaterThanOrEqualsFlipped :: forall datastore t o. (Ord o, FromDBObject t, ToDBObject o) =>
			      RightPredicateFlipped datastore t o
greaterThanOrEqualsFlipped = flip greaterThanOrEquals
infixl 4 greaterThanOrEqualsFlipped as >=.

-- | A `Predicate` that will tell the underlying datastore to use this string
-- | as some value for sorting during a database query. One expected usage of
-- | this string value is that it is the name of a field, e.g.: `(sortBy desc
-- | "name")`.
-- |
-- | The `FromDBObject t` type constraint is included here so that this can be
-- | grouped with other Predicates operating on the same `t`.
sortBy' :: forall datastore t. FromDBObject t =>
	   LeftPredicate datastore t SortOrder
sortBy' order fieldName = Query $ singleton $
	{ clause        : SortBy { sortOrder: order }
	, logicConnector: And
	, clauseValue   : Maybe Nothing
	, predicate     : Left fieldName
	}

sortBy :: forall datastore t. FromDBObject t =>
	  SortOrder                          ->
	  (t -> Ordering)                    ->
	  Query datastore t
sortBy order fn = Query $ singleton $
	{ clause        : SortBy { sortOrder: order }
	, logicConnector: And
	, clauseValue   : Maybe Nothing
	, predicate     : Right (fn >>> toDBObject)
	}

-- | A `Predicate` that tells the underlying datastore to do some
-- | implementation-defined action with this string during a database query.
-- |
-- | The `FromDBObject t` type constraint is included here so that this can be
-- | grouped with other Predicates operating on the same `t`.
custom' :: forall datastore t. FromDBObject t =>
	   LeftPredicate datastore t String
custom' name p = Query $ singleton $
	{ clause        : Custom { name }
	, logicConnector: And
	, clauseValue   : Maybe Nothing
	, predicate     : Left p
	}

custom :: forall datastore t. FromDBObject t =>
	  String                             ->
	  (t -> Boolean)                     ->
	  Query datastore t
custom name fn = Query $ singleton $
	{ clause        : Custom { name }
	, logicConnector: And
	, clauseValue   : Maybe Nothing
	, predicate     : Right (fn >>> toDBObject)
	}

negate :: forall datastore t. FromDBObject t =>
	  Query datastore t                  ->
	  Query datastore t
negate (Query ps) = Query $ map negator ps
	where
		negator p = p { clause = Negate { clause: p.clause } }
