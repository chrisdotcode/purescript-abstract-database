module Abstract.Database
	( DATABASE
	, CollectionImpl
	, class ToDBObject
	, toDBObject
	, class FromDBObject
	, fromDBObject
	, class IsDBObject
	, Collection
	, Pagination(Pagination)
	, limit
	, skip
	, SortOrder(Ascending, Descending)
	, asc
	, desc
	, sortOrder
	, getClauseType
	, Clause
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
	, clause
	, LogicConnector(And, Or)
	, logicConnector
	, Predicate
	, Query(Query)
	, ordering
	, equals'
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
	, Entity(..)
	, class Database
	, openConnection
	, closeConnection
	, createCollection
	, getCollection
	, deleteCollection
	, getWhere
	, getWhere'
	, countWhere
	, insert
	, insert'
	, deleteWhere
	) where

import Prelude
	( class Eq
	, class Functor
	, class Ord
	, class Semigroup
	, class Show
	, Ordering(EQ, GT, LT)
	, Unit
	, compare
	, const
	, flip
	, map
	, negate
	, show
	, zero
	, ($)
	, (&&)
	, (<>)
	, (==)
	, (>=>)
	, (<<<)
	, (>>>)
	, (<$>)
	)

import Control.Alt           (class Alt, (<|>))
import Control.Monad.Eff     (Eff)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Except  (mapExcept, runExcept)
import Data.Array            (length, range, zipWith)
import Data.Foreign
	( F
	, Foreign
	, ForeignError(ErrorAtIndex)
	, readArray
	, toForeign
	)
import Data.Bifunctor        (lmap)
import Data.Foreign.Class    (read, write)
import Data.Foreign.Null     (Null(Null), readNull, writeNull)
import Data.Foreign.NullOrUndefined
	( NullOrUndefined(NullOrUndefined)
	, readNullOrUndefined
	)
import Data.Foreign.Undefined
	( Undefined(Undefined)
	, readUndefined
	, writeUndefined
	)
import Data.Either           (Either(Left, Right), either)
import Data.List             (List, singleton, uncons)
import Data.Maybe            (Maybe(Just, Nothing), maybe)
import Data.Monoid           (class Monoid, mempty)
import Data.Traversable      (sequence)
import Unsafe.Coerce         (unsafeCoerce)

foreign import data DATABASE       :: !
foreign import data CollectionImpl :: *

class FromDBObject a where
	fromDBObject :: Foreign -> F a

instance booleanFromDBObject :: FromDBObject Boolean where
	fromDBObject = read

instance intFromDBObject :: FromDBObject Int where
	fromDBObject = read

instance numberFromDBObject :: FromDBObject Number where
	fromDBObject = read

instance charFromDBObject :: FromDBObject Char where
	fromDBObject = read

instance stringFromDBObject :: FromDBObject String where
	fromDBObject = read

instance foreignFromDBObject :: FromDBObject Foreign where
	fromDBObject = read

instance nullFromDBObject :: FromDBObject a => FromDBObject (Null a) where
	fromDBObject = readNull fromDBObject

instance undefinedFromDBObject :: FromDBObject a => FromDBObject (Undefined a) where
	fromDBObject = readUndefined fromDBObject

instance nullOrUndefinedFromDBObject :: FromDBObject a => FromDBObject (NullOrUndefined a) where
	fromDBObject = readNullOrUndefined fromDBObject

instance arrayFromDBObject :: FromDBObject a => FromDBObject (Array a) where
	fromDBObject = readArray >=> fromDBObjectElements
		where
			fromDBObjectWith f          = mapExcept (lmap f) <<< fromDBObject
			fromDBObjectElement i value = fromDBObjectWith (map (ErrorAtIndex i)) value
			fromDBObjectElements arr    = sequence (zipWith fromDBObjectElement (range zero (length arr)) arr)


class ToDBObject a where
	toDBObject :: a -> Foreign

instance booleanToDBObject :: ToDBObject Boolean where
	toDBObject = write

instance intToDBObject :: ToDBObject Int where
	toDBObject = write

instance numberToDBObject :: ToDBObject Number where
	toDBObject = write

instance charToDBObject :: ToDBObject Char where
	toDBObject = write

instance stringToDBObject :: ToDBObject String where
	toDBObject = write

instance foreignToDBObject :: ToDBObject Foreign where
	toDBObject = write

instance nullToDBObject :: ToDBObject a => ToDBObject (Null a) where
	toDBObject (Null a) = maybe writeNull toDBObject a

instance undefinedToDBObject :: ToDBObject a => ToDBObject (Undefined a) where
	toDBObject (Undefined a) = maybe writeUndefined toDBObject a

instance nullOrUndefinedToDBObject :: ToDBObject a => ToDBObject (NullOrUndefined a) where
	toDBObject (NullOrUndefined a) = toDBObject (Null a)

instance arrayToDBObject :: ToDBObject a => ToDBObject (Array a) where
	toDBObject = toForeign <<< map toDBObject

class (FromDBObject a, ToDBObject a) <= IsDBObject a

instance booleanIsDBObject :: IsDBObject Boolean 

instance intIsDBObject :: IsDBObject Int 

instance numberIsDBObject :: IsDBObject Number 

instance charIsDBObject :: IsDBObject Char 

instance stringIsDBObject :: IsDBObject String 

instance foreignIsDBObject :: IsDBObject Foreign 

instance nullIsDBObject :: IsDBObject a => IsDBObject (Null a) 

instance undefinedIsDBObject :: IsDBObject a => IsDBObject (Undefined a) 

instance nullOrUndefinedIsDBObject :: IsDBObject a => IsDBObject (NullOrUndefined a) 

instance arrayIsDBObject :: IsDBObject a => IsDBObject (Array a) 

type Collection t = { collectionImpl :: CollectionImpl }

newtype Pagination = Pagination
	{ skip   :: Maybe Int
	, limit  :: Maybe Int
	}

instance showPagination :: Show Pagination where
	show (Pagination p) = "(Pagination {"
		<> " skip: "   <> show p.skip
		<> ", limit: " <> show p.limit
		<> " })"

instance eqPagination :: Eq Pagination where
	eq p1 p2 = show p1 == show p2

instance ordPagination :: Ord Pagination where
	compare p1 p2 = compare (show p1) (show p2)

instance semigroupPagination :: Semigroup Pagination where
	append (Pagination p1) (Pagination p2) = Pagination
		{ skip: p2.skip     <|> p1.skip   <|> Nothing
		, limit : p2.limit  <|> p1.limit  <|> Nothing
		}

defPagination :: { skip :: Maybe Int, limit :: Maybe Int }
defPagination =
	{ skip : Nothing
	, limit: Nothing
	}

instance monoidPagination :: Monoid Pagination where
	mempty = Pagination defPagination

limit :: Int -> Maybe Pagination
limit l = Just $ Pagination defPagination { limit = Just l }

skip :: Int -> Maybe Pagination
skip o = Just $ Pagination defPagination { skip = Just o }

data SortOrder = Ascending | Descending

instance showSortOrder :: Show SortOrder where
	show Ascending  = "Ascending"
	show Descending = "Descending"

instance eqSortOrder :: Eq SortOrder where
	eq Ascending  Ascending  = true
	eq Descending Descending = true
	eq _          _          = false

instance ordSortOrder :: Ord SortOrder where
	compare Descending Ascending  = LT
	compare Ascending  Descending = GT
	compare _          _          = EQ

asc :: SortOrder
asc = Ascending

desc :: SortOrder
desc = Descending

sortOrder:: { asc :: SortOrder, desc :: SortOrder }
sortOrder =
	{ asc : Ascending
	, desc: Descending
	}

data Clause = Equals
	    | NotEquals
	    | LessThan
	    | LessThanOrEquals
	    | GreaterThan
	    | GreaterThanOrEquals
	    | SortBy { sortOrder :: SortOrder }
	    | Custom { name      :: String    }
	    | Negate { clause    :: Clause    }

instance showClause :: Show Clause where
	show Equals              = "Equals"
	show NotEquals           = "NotEquals"
	show LessThan            = "LessThan"
	show LessThanOrEquals    = "LessThanOrEquals"
	show GreaterThan         = "GreaterThan"
	show GreaterThanOrEquals = "GreaterThanOrEquals"
	show (SortBy s)          =
		"SortBy ({ sortOrder: " <> show s.sortOrder <> " })"
	show (Custom n)          = "Custom ({ name: " <> show n.name <> " })"
	show (Negate c)          =
		"Negate ({ clause: " <> show c.clause <> " })"

instance eqClause :: Eq Clause where
	eq c1 c2 = show c1 == show c2

getClauseType :: Clause -> String
getClauseType Equals              = "Equals"
getClauseType NotEquals           = "NotEquals"
getClauseType LessThan            = "LessThan"
getClauseType LessThanOrEquals    = "LessThanOrEquals"
getClauseType GreaterThan         = "GreaterThan"
getClauseType GreaterThanOrEquals = "GreaterThanOrEquals"
getClauseType (SortBy _)          = "SortBy"
getClauseType (Custom _)          = "Custom"
getClauseType (Negate _)          = "Negate"

clause :: { equals              :: String
	  , notEquals           :: String
	  , lessThan            :: String
	  , lessThanOrEquals    :: String
	  , greaterThan         :: String
	  , greaterThanOrEquals :: String
	  , sortBy              :: String
	  , custom              :: String
	  , negate              :: String
	  }
clause =
	{ equals             : getClauseType Equals
	, notEquals          : getClauseType NotEquals
	, lessThan           : getClauseType LessThan
	, lessThanOrEquals   : getClauseType LessThanOrEquals
	, greaterThan        : getClauseType GreaterThan
	, greaterThanOrEquals: getClauseType GreaterThanOrEquals
	, sortBy             : getClauseType $ SortBy { sortOrder: desc }
	, custom             : getClauseType $ Custom { name: mempty }
	, negate             : getClauseType $ Negate { clause: Equals }
	}

data LogicConnector = And | Or

logicConnector :: { and :: LogicConnector, or :: LogicConnector }
logicConnector =
	{ and: And
	, or : Or
	}

instance showLogicConnector :: Show LogicConnector where
	show And = "And"
	show Or  = "Or"

instance eqLogicConnector :: Eq LogicConnector where
	eq And And = true
	eq Or  Or  = true
	eq _   _   = false

type Predicate t =
	{ clause         :: Clause
	, logicConnector :: LogicConnector
	, clauseValue    :: Foreign
	, predicate      :: Either String (Foreign -> Maybe Foreign)
	}

showPredicate :: forall t. Predicate t -> String
showPredicate p = "({"
	<> " clause: "          <> show p.clause
	<> ", logicConnector: " <> show p.logicConnector
	<> ", clauseValue: "    <> "(Foreign)"
	<> ", predicate: "      <> "(Foreign -> Maybe Foreign)"
	<> " })"

data Query datastore t = Query (List (Predicate t))

instance showQuery :: Show (Query datastore t) where
	show (Query l) =
		"(Query (datastore) " <> show (map showPredicate l) <> ")"

instance semigroupQuery :: Semigroup (Query datastore t) where
	append (Query l1) (Query l2) = Query (l1 <> l2)

instance monoidQuery :: Monoid (Query datastore t) where
	mempty = Query mempty

instance functorQuery :: Functor (Query datastore) where
	map _ q = unsafeCoerce q -- Father, forgive me.

instance altQuery :: Alt (Query datastore) where
	alt (Query l1) (Query l2) =
		case uncons l2 of
			Just { head, tail } ->
				Query (l1 <> connectorToOr head <> tail)
			Nothing             -> Query l1
		where
			connectorToOr = singleton <<< _ { logicConnector = Or }

orderingToInt :: Ordering -> Int
orderingToInt LT = -1
orderingToInt EQ = 0
orderingToInt GT = 1

ordering :: { lt :: Int, eq :: Int, gt :: Int }
ordering =
	{ lt: orderingToInt LT
	, eq: orderingToInt EQ
	, gt: orderingToInt GT
	}

leftPredicate :: forall t a datastore. (ToDBObject a) =>
		 Clause                               ->
		 a                                    ->
		 String                               ->
		 Query datastore t
leftPredicate clause_ clauseValue fieldName = Query $ singleton $
	{ clause        : clause_
	, logicConnector: And
	, clauseValue   : toDBObject clauseValue
	, predicate     : Left fieldName
	}

-- In this contexts this is used, it *should* never fail a conversion (because
-- this will most likely be used with data gotten back directly from the
-- datastore.
fromDBObject' :: forall t. FromDBObject t => Foreign -> Maybe t
fromDBObject' = fromDBObject >>> runExcept >>> either (const Nothing) Just

predicate' :: forall t a. (FromDBObject t, ToDBObject a) =>
	      (t -> a)                                   ->
	      Foreign                                    ->
	      Maybe Foreign
predicate' fn t' = (fn >>> toDBObject) <$> fromDBObject' t'

rightPredicate :: forall t a datastore. (FromDBObject t, ToDBObject a) =>
		  Clause                                               ->
		  a                                                    ->
		  (t -> a)                                             ->
		  Query datastore t
rightPredicate clause_ clauseValue fn = Query $ singleton $
	{ clause        : clause_
	, logicConnector: And
	, clauseValue   : toDBObject clauseValue
	, predicate     : Right $ predicate' fn
	}

equals' :: forall datastore t e. (ToDBObject e, Eq e) =>
	   e                                          ->
	   String                                     ->
	   Query datastore t
equals' = leftPredicate Equals

equals'Flipped :: forall datastore t e. (ToDBObject e, Eq e) =>
		  String                                     ->
		  e                                          ->
		  Query datastore t
equals'Flipped = flip equals'
infix 4 equals'Flipped as ==..

equals :: forall datastore t e. (FromDBObject t, ToDBObject e, Eq e) =>
	  e                                                          ->
	  (t -> e)                                                   ->
	  Query datastore t
equals = rightPredicate Equals

equalsFlipped :: forall datastore t e. (FromDBObject t, ToDBObject e, Eq e) =>
		 (t -> e)                                                   ->
		 e                                                          ->
		 Query datastore t
equalsFlipped = flip equals
infix 4 equalsFlipped as ==.

notEquals' :: forall datastore t e. (ToDBObject e, Eq e) =>
	      e                                          ->
	      String                                     ->
	      Query datastore t
notEquals' = leftPredicate NotEquals

notEquals'Flipped :: forall datastore t e. (ToDBObject e, Eq e) =>
		     String                                     ->
		     e                                          ->
		     Query datastore t
notEquals'Flipped = flip notEquals'
infix 4 notEquals'Flipped as /=..
infix 4 notEquals'Flipped as !=..

notEquals :: forall datastore t e. (FromDBObject t, ToDBObject e, Eq e) =>
	     e                                                          ->
	     (t -> e)                                                   ->
	     Query datastore t
notEquals = rightPredicate NotEquals

notEqualsFlipped :: forall datastore t e. (FromDBObject t, ToDBObject e, Eq e) =>
		    (t -> e)                                                   ->
		    e                                                          ->
		    Query datastore t
notEqualsFlipped = flip notEquals
infix 4 notEquals' as /=.
infix 4 notEquals' as !=.

lessThan' :: forall datastore t o. (ToDBObject o, Ord o) =>
	     o                                           ->
	     String                                      ->
	     Query datastore t
lessThan' = leftPredicate LessThan

lessThan'Flipped :: forall datastore t o. (ToDBObject o, Ord o) =>
	     String                                             ->
	     o                                                  ->
	     Query datastore t
lessThan'Flipped = flip lessThan'
infixl 4 lessThan'Flipped as <..

lessThan :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
	    o                                                           ->
	    (t -> o)                                                    ->
	    Query datastore t
lessThan = rightPredicate LessThan

lessThanFlipped :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
		   (t -> o)                                                    ->
		   o                                                           ->
		   Query datastore t
lessThanFlipped = flip lessThan
infixl 4 lessThanFlipped as <.

lessThanOrEquals' :: forall datastore t o. (ToDBObject o, Ord o) =>
		     o                                           ->
		     String                                      ->
		     Query datastore t
lessThanOrEquals' = leftPredicate LessThanOrEquals

lessThanOrEquals'Flipped :: forall datastore t o. (ToDBObject o, Ord o) =>
			    String                                      ->
			    o                                           ->
			    Query datastore t
lessThanOrEquals'Flipped = flip lessThanOrEquals'
infixl 4 lessThanOrEquals'Flipped as <=..

lessThanOrEquals :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
		    o                                                           ->
		    (t -> o)                                                    ->
		    Query datastore t
lessThanOrEquals = rightPredicate LessThanOrEquals

lessThanOrEqualsFlipped :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
			   (t -> o)                                                    ->
			   o                                                           ->
			   Query datastore t
lessThanOrEqualsFlipped = flip lessThanOrEquals
infixl 4 lessThanOrEqualsFlipped as <=.

greaterThan' :: forall datastore t o. (ToDBObject o, Ord o) =>
		o                                           ->
		String                                      ->
		Query datastore t
greaterThan' = leftPredicate GreaterThan

greaterThan'Flipped :: forall datastore t o. (ToDBObject o, Ord o) =>
		       String                                      ->
		       o                                           ->
		       Query datastore t
greaterThan'Flipped = flip greaterThan'
infixl 4 greaterThan'Flipped as >..

greaterThan :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
	       o                                                           ->
	       (t -> o)                                                    ->
	       Query datastore t
greaterThan = rightPredicate GreaterThan

greaterThanFlipped :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
	       (t -> o)                                                           ->
	       o                                                                  ->
	       Query datastore t
greaterThanFlipped = flip greaterThan
infixl 4 greaterThanFlipped as >.

greaterThanOrEquals' :: forall datastore t o. (ToDBObject o, Ord o) =>
			o                                           ->
			String                                      ->
			Query datastore t
greaterThanOrEquals' = leftPredicate GreaterThanOrEquals

greaterThanOrEquals'Flipped :: forall datastore t o. (ToDBObject o, Ord o) =>
			       String                                      ->
			       o                                           ->
			       Query datastore t
greaterThanOrEquals'Flipped = flip greaterThanOrEquals'
infixl 4 greaterThanOrEquals'Flipped as >=..

greaterThanOrEquals :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
		       o                                                           ->
		       (t -> o)                                                    ->
		       Query datastore t
greaterThanOrEquals = rightPredicate GreaterThanOrEquals

greaterThanOrEqualsFlipped :: forall datastore t o. (FromDBObject t, ToDBObject o, Ord o) =>
		       (t -> o)                                                    ->
		       o                                                           ->
		       Query datastore t
greaterThanOrEqualsFlipped = flip greaterThanOrEquals
infixl 4 greaterThanOrEqualsFlipped as >=.

sortBy' :: forall datastore t. SortOrder -> String -> Query datastore t
sortBy' order fieldName = Query $ singleton $
	{ clause        : SortBy { sortOrder: order }
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Left fieldName
	}

sortBy :: forall datastore t. FromDBObject t =>
	  SortOrder                          ->
	  (t -> Ordering)                    ->
	  Query datastore t
sortBy order fn = Query $ singleton $
	{ clause        : SortBy { sortOrder: order }
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Right $ predicate' (fn >>> orderingToInt)
	}

custom' :: forall datastore t. String -> String -> Query datastore t
custom' name p = Query $ singleton $
	{ clause        : Custom { name }
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Left p
	}

custom :: forall datastore t. FromDBObject t =>
	  String                             ->
	  (t -> Boolean)                     ->
	  Query datastore t
custom name fn = Query $ singleton $
	{ clause        : Custom { name }
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Right $ predicate' fn
	}

data Entity key a = Entity key a | Entity' a

instance showEntity :: (Show key, Show a) => Show (Entity key a) where
	show (Entity key a) = "(Entity"
		<> " (" <> show key <> ")"
		<> " (" <> show a   <> ")"
		<> ")"
	show (Entity' a)     = "(Entity' (" <> show a <> "))"

instance eqEntity :: (Eq key, Eq a) => Eq (Entity key a) where
	eq (Entity k1 a1) (Entity k2 a2) = k1 == k2 && a1 == a2
	eq (Entity'   a1) (Entity'   a2) = a1 == a2
	eq              _              _ = false

instance ordEntity :: (Ord key, Ord a) => Ord (Entity key a) where
	compare (Entity k1 a1) (Entity k2 a2) = compare k1 k2 <> compare a1 a2
	compare (Entity'   a1) (Entity'   a2) = compare a1 a2
	-- I'm not entirely sure how an Entity with a key can be 'greater' than
	-- one without, but I've just decided to choose this implementation and
	-- stick with it.
	compare (Entity'    _) (Entity _   _) = LT
	compare (Entity _   _) (Entity'    _) = GT

class Database datastore where
	openConnection :: forall e. String -> Eff (db :: DATABASE | e) datastore
	closeConnection :: forall e. datastore -> Eff (db :: DATABASE | e) Unit

	createCollection :: forall e. String ->
			    datastore        ->
			    Eff (db :: DATABASE | e) Unit
	getCollection    :: forall e t. String ->
			    datastore          ->
			    Eff (db :: DATABASE, ref :: REF | e) (Ref (Collection t))
	deleteCollection :: forall e. String ->
			    datastore        ->
			    Eff (db :: DATABASE | e) Unit

	getWhere :: forall e t. FromDBObject t =>
		    Maybe Pagination           ->
		    Query datastore t          ->
		    Ref (Collection t)         ->
		    Eff (db :: DATABASE, ref :: REF | e) (Maybe t)

	getWhere' :: forall e t. FromDBObject t =>
		    Maybe Pagination            ->
		    Query datastore t           ->
		    Ref (Collection t)          ->
		    Eff (db :: DATABASE, ref :: REF | e) (List t)

	countWhere :: forall e t. FromDBObject t =>
		      Query datastore t          ->
		      Ref (Collection t)         ->
		      Eff (db :: DATABASE, ref :: REF | e) Int

	insert :: forall e t b. ToDBObject t =>
		  t                          ->
		  Ref (Collection t)         ->
		  Eff (db :: DATABASE, ref :: REF | e) b

	insert' :: forall e t b. ToDBObject t =>
		   List t                     ->
		   Ref (Collection t)         ->
		   Eff (db :: DATABASE, ref :: REF | e) b

	deleteWhere :: forall e t b. ToDBObject t =>
		       Query datastore t          ->
		       Ref (Collection t)         ->
		       Eff (db :: DATABASE, ref :: REF | e) b
