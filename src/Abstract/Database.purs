module Abstract.Database
	( DATABASE
	, CollectionImpl
	, Collection
	, Pagination
	, limit
	, skip
	, SortOrder
	, asc
	, desc
	, sortOrder
	, Clause
	, clause
	, LogicConnector
	, logicConnector
	, Predicate
	, SQL
	, Native
	, Query
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
	, class Database
	, createDatabase
	, deleteDatabase
	, importDatabase
	, exportDatabase
	, openDatabase
	, saveDatabase
	, closeDatabase
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
	, flip
	, map
	, negate
	, show
	, ($)
	, (<>)
	, (==)
	, (<<<)
	, (>>>)
	)

import Control.Alt            (class Alt, (<|>))
import Control.Monad.Eff      (Eff)
import Control.Monad.Eff.Ref  (REF, Ref)
import Data.Foreign           (Foreign)
import Data.Foreign.Class     (class AsForeign, class IsForeign, write)
import Data.Foreign.Undefined (writeUndefined)
import Data.Either            (Either(Left, Right))
import Data.List              (List, singleton, uncons)
import Data.Maybe             (Maybe(Just, Nothing))
import Data.Monoid            (class Monoid, mempty)
import Unsafe.Coerce          (unsafeCoerce)

foreign import data DATABASE       :: !
foreign import data CollectionImpl :: *

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
	    | SortBy SortOrder
	    | Custom String -- ^ The name of the custom clause.
	    | Negate Clause

instance showClause :: Show Clause where
	show Equals              = "Equals"
	show NotEquals           = "NotEquals"
	show LessThan            = "LessThan"
	show LessThanOrEquals    = "LessThanOrEquals"
	show GreaterThan         = "GreaterThan"
	show GreaterThanOrEquals = "GreaterThanOrEquals"
	show (SortBy s)          = "SortBy (" <> show s <> ")"
	show (Custom c)          = "Custom (" <> show c <> ")"
	show (Negate c)          = "Negate (" <> show c <> ")"

instance eqClause :: Eq Clause where
	eq c1 c2 = show c1 == show c2

clause :: { equals              :: Clause             
	  , notEquals           :: Clause          
	  , lessThan            :: Clause           
	  , lessThanOrEquals    :: Clause   
	  , greaterThan         :: Clause        
	  , greaterThanOrEquals :: Clause
	  , sortBy              :: SortOrder -> Clause
	  , custom              :: String    -> Clause
	  , negate              :: Clause    -> Clause   
	  }
clause =
	{ equals             : Equals
	, notEquals          : NotEquals
	, lessThan           : LessThan
	, lessThanOrEquals   : LessThanOrEquals
	, greaterThan        : GreaterThan
	, greaterThanOrEquals: GreaterThanOrEquals
	, sortBy             : SortBy
	, custom             : Custom
	, negate             : Negate
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
	, predicate      :: Either String (t -> Foreign)
	}

showPredicate :: forall t. Predicate t -> String
showPredicate p = "({"
	<> " clause: "          <> show p.clause
	<> ", logicConnector: " <> show p.logicConnector
	<> ", clauseValue: "    <> "(IsForeign a)"
	<> ", predicate: "      <> "(t -> (IsForeign a))"
	<> " })"

data SQL
data Native

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

leftPredicate :: forall t a datastore. (AsForeign a) =>
		 Clause                              ->
		 a                                   ->
		 String                              ->
		 Query datastore t
leftPredicate clause_ clauseValue fieldName = Query $ singleton $
	{ clause        : clause_
	, logicConnector: And
	, clauseValue   : write clauseValue
	, predicate     : Left fieldName
	}

rightPredicate :: forall t a datastore. (AsForeign a) =>
		  Clause                              ->
		  a                                   ->
		  (t -> a)                            ->
		  Query datastore t
rightPredicate clause_ clauseValue fn = Query $ singleton $
	{ clause        : clause_
	, logicConnector: And
	, clauseValue   : write clauseValue
	, predicate     : Right (fn >>> write)
	}

equals' :: forall t e. (AsForeign e, Eq e) => e -> String -> Query SQL t
equals' = leftPredicate Equals

equals'Flipped :: forall t e. (AsForeign e, Eq e) =>
		  String                          ->
		  e                               ->
		  Query SQL t
equals'Flipped = flip equals'
infix 4 equals'Flipped as ==..

equals :: forall t e. (AsForeign e, Eq e) => e -> (t -> e) -> Query Native t
equals = rightPredicate Equals

equalsFlipped :: forall t e. (AsForeign e, Eq e) =>
		 (t -> e)                        ->
		 e                               ->
		 Query Native t
equalsFlipped = flip equals
infix 4 equalsFlipped as ==.

notEquals' :: forall t e. (AsForeign e, Eq e) => e -> String -> Query SQL t
notEquals' = leftPredicate NotEquals

notEquals'Flipped :: forall t e. (AsForeign e, Eq e) =>
		     String                          ->
		     e                               ->
		     Query SQL t
notEquals'Flipped = flip notEquals'
infix 4 notEquals'Flipped as /=..
infix 4 notEquals'Flipped as !=..

notEquals :: forall t e. (AsForeign e, Eq e) => e -> (t -> e) -> Query Native t
notEquals = rightPredicate NotEquals

notEqualsFlipped :: forall t e. (AsForeign e, Eq e) =>
		    (t -> e)                        ->
		    e                               ->
		    Query Native t
notEqualsFlipped = flip notEquals
infix 4 notEquals' as /=.
infix 4 notEquals' as !=.

lessThan' :: forall t o. (AsForeign o, Ord o) => o -> String -> Query SQL t
lessThan' = leftPredicate LessThan

lessThan'Flipped :: forall t o. (AsForeign o, Ord o) =>
		    String                           ->
		    o                                ->
		    Query SQL t
lessThan'Flipped = flip lessThan'
infixl 4 lessThan'Flipped as <..

lessThan :: forall t o. (AsForeign o, Ord o) => o -> (t -> o) -> Query Native t
lessThan = rightPredicate LessThan

lessThanFlipped :: forall t o. (AsForeign o, Ord o) =>
		    (t -> o)                        ->
		    o                               ->
		    Query Native t
lessThanFlipped = flip lessThan
infixl 4 lessThanFlipped as <.

lessThanOrEquals' :: forall t o. (AsForeign o, Ord o) =>
		     o                                ->
		     String                           ->
		     Query SQL t
lessThanOrEquals' = leftPredicate LessThanOrEquals

lessThanOrEquals'Flipped :: forall t o. (AsForeign o, Ord o) =>
		    String                                   ->
		    o                                        ->
		    Query SQL t
lessThanOrEquals'Flipped = flip lessThanOrEquals'
infixl 4 lessThanOrEquals'Flipped as <=..

lessThanOrEquals :: forall t o. (AsForeign o, Ord o) =>
		    o                                ->
		    (t -> o)                         ->
		    Query Native t
lessThanOrEquals = rightPredicate LessThanOrEquals

lessThanOrEqualsFlipped :: forall t o. (AsForeign o, Ord o) =>
		    (t -> o)                                ->
		    o                                       ->
		    Query Native t
lessThanOrEqualsFlipped = flip lessThanOrEquals
infixl 4 lessThanOrEqualsFlipped as <=.

greaterThan' :: forall t o. (AsForeign o, Ord o) => o -> String -> Query SQL t
greaterThan' = leftPredicate GreaterThan

greaterThan'Flipped :: forall t o. (AsForeign o, Ord o) =>
		    String                              ->
		    o                                   ->
		    Query SQL t
greaterThan'Flipped = flip greaterThan'
infixl 4 greaterThan'Flipped as >..

greaterThan :: forall t o. (AsForeign o, Ord o) => o -> (t -> o) -> Query Native t
greaterThan = rightPredicate GreaterThan

greaterThanFlipped :: forall t o. (AsForeign o, Ord o) =>
		    (t -> o)                           ->
		    o                                  ->
		    Query Native t
greaterThanFlipped = flip greaterThan
infixl 4 greaterThanFlipped as >.

greaterThanOrEquals' :: forall t o. (AsForeign o, Ord o) =>
		     o                                   ->
		     String                              ->
		     Query SQL t
greaterThanOrEquals' = leftPredicate GreaterThanOrEquals

greaterThanOrEquals'Flipped :: forall t o. (AsForeign o, Ord o) =>
		    String                                      ->
		    o                                           ->
		    Query SQL t
greaterThanOrEquals'Flipped = flip greaterThanOrEquals'
infixl 4 greaterThanOrEquals'Flipped as >=..

greaterThanOrEquals :: forall t o. (AsForeign o, Ord o) =>
		    o                                   ->
		    (t -> o)                            ->
		    Query Native t
greaterThanOrEquals = rightPredicate GreaterThanOrEquals

greaterThanOrEqualsFlipped :: forall t o. (AsForeign o, Ord o) =>
		    (t -> o)                                   ->
		    o                                          ->
		    Query Native t
greaterThanOrEqualsFlipped = flip greaterThanOrEquals
infixl 4 greaterThanOrEqualsFlipped as >=.

sortBy' :: forall t. SortOrder -> String -> Query SQL t
sortBy' order fieldName = Query $ singleton $
	{ clause        : SortBy order
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Left fieldName
	}

sortBy :: forall t. SortOrder -> (t -> Ordering) -> Query Native t
sortBy order fn = Query $ singleton $
	{ clause        : SortBy order
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Right (fn >>> orderingToInt >>> write)
	}

custom' :: forall t. String -> String -> Query SQL t
custom' name p = Query $ singleton $
	{ clause        : Custom name
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Left p
	}

custom :: forall t. String -> (t -> Boolean) -> Query Native t
custom name fn = Query $ singleton $
	{ clause        : Custom name
	, logicConnector: And
	, clauseValue   : writeUndefined
	, predicate     : Right (fn >>> write)
	}

class Database d where
	createDatabase :: forall e. String  -> Eff (db :: DATABASE | e) Unit
	deleteDatabase :: forall e. String  -> Eff (db :: DATABASE | e) Unit
	importDatabase :: forall e. Foreign -> Eff (db :: DATABASE | e) Unit
	exportDatabase :: forall e. Eff (db :: DATABASE | e) Foreign
	openDatabase   :: forall e. String  -> Eff (db :: DATABASE | e) Unit
	saveDatabase   :: forall e. Eff (db :: DATABASE | e) Unit
	closeDatabase  :: forall e. String  -> Eff (db :: DATABASE | e) Unit

	createCollection :: forall e. String   -> Eff (db :: DATABASE | e) Unit
	getCollection    :: forall e t. String ->
			    Eff (db :: DATABASE | e) (Ref (Collection t))
	deleteCollection :: forall e. String   -> Eff (db :: DATABASE | e) Unit

	getWhere :: forall e a t. IsForeign t =>
		    Maybe Pagination          ->
		    Query a t                 ->
		    Ref (Collection t)        ->
		    Eff (db :: DATABASE, ref :: REF | e) (Maybe t)

	getWhere' :: forall e a t. IsForeign t =>
		    Maybe Pagination           ->
		    Query a t                  ->
		    (Ref (Collection t))       ->
		    Eff (db :: DATABASE, ref :: REF | e) (List t)

	countWhere :: forall e a t. IsForeign t =>
		      Query a t                 ->
		      Ref (Collection t)        ->
		      Eff (db :: DATABASE, ref :: REF | e) Int

	insert :: forall e t b. IsForeign t =>
		  t                         ->
		  Ref (Collection t)        ->
		  Eff (db :: DATABASE, ref :: REF | e) b

	insert' :: forall e t b. IsForeign t =>
		   List t                    ->
		   Ref (Collection t)        ->
		   Eff (db :: DATABASE, ref :: REF | e) b

	deleteWhere :: forall e t a b. IsForeign t =>
		       Query a t                   ->
		       Ref (Collection t)          ->
		       Eff (db :: DATABASE, ref :: REF | e) b
