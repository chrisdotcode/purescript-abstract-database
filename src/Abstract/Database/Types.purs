module Abstract.Database.Types
	( DATABASE
	, CollectionImpl
	, DBObject
		( Boolean
		, Int
		, Number
		, String
		, Maybe
		, Array
		, DBObject
		)
	, isBoolean
	, isInt
	, isNumber
	, isString
	, isMaybe
	, isArray
	, isDBObject
	, Collection
	, Pagination(Pagination)
	, limit
	, skip
	, SortOrder(Ascending, Descending)
	, asc
	, desc
	, ordering
	, sortOrder
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
	, getClauseType
	, LogicConnector(And, Or)
	, logicConnector
	, Predicate
	, Query(Query)
	, Entity(..)
	) where

import Prelude
	( class Eq
	, class Functor
	, class Ord
	, class Semigroup
	, class Show
	, Ordering(EQ, GT, LT)
	, compare
	, map
	, negate
	, show
	, ($)
	, (&&)
	, (<>)
	, (==)
	, (<<<)
	)

import Control.Alt   (class Alt, (<|>))
import Data.Either   (Either)
import Data.List     (List, singleton, uncons)
import Data.StrMap   (StrMap)
import Data.Maybe    (Maybe(Just, Nothing))
import Data.Monoid   (class Monoid, mempty)
import Unsafe.Coerce (unsafeCoerce)

foreign import data DATABASE       :: !
foreign import data CollectionImpl :: *

data DBObject = Boolean Boolean
	     | Int Int
	     | Number Number
	     | String String
	     | Maybe (Maybe DBObject)
	     | Array (Array DBObject)
	     | DBObject (StrMap DBObject)

instance showDBObject :: Show DBObject where
	show (Boolean  b) = "(Boolean "   <> show b <> ")"
	show (Int      i) = "(Int "       <> show i <> ")"
	show (Number   n) = "(Number "    <> show n <> ")"
	show (String   s) = "(String "    <> show s <> ")"
	show (Maybe    m) = "(Maybe "     <> show m <> ")"
	show (Array    a) = "(Array "     <> show a <> ")"
	show (DBObject o) = "(DBObject "  <> show o <> ")"

instance eqDBObject :: Eq DBObject where
	eq (Boolean  b1) (Boolean  b2) = b1 == b2
	eq (Int      i1) (Int      i2) = i1 == i2
	eq (Number   n1) (Number   n2) = n1 == n2
	eq (String   s1) (String   s2) = s1 == s2
	eq (Maybe    m1) (Maybe    m2) = m1 == m2
	eq (Array    a1) (Array    a2) = a1 == a2
	eq (DBObject o1) (DBObject o2) = o1 == o2
	eq             _             _ = false

isBoolean :: DBObject -> Boolean
isBoolean (Boolean _) = true
isBoolean          _  = false

isInt :: DBObject -> Boolean
isInt (Int _) = true
isInt      _  = false

isNumber :: DBObject -> Boolean
isNumber (Number _) = true
isNumber         _  = false

isString :: DBObject -> Boolean
isString (String _) = true
isString         _  = false

isMaybe :: DBObject -> Boolean
isMaybe (Maybe _) = true
isMaybe        _  = false

isArray :: DBObject -> Boolean
isArray (Array _) = true
isArray        _  = false

isDBObject :: DBObject -> Boolean
isDBObject (DBObject _) = true
isDBObject           _  = false

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
	, clauseValue    :: DBObject
	, predicate      :: Either String (t -> DBObject)
	}

showPredicate :: forall t. Predicate t -> String
showPredicate p = "({"
	<> " clause: "          <> show p.clause
	<> ", logicConnector: " <> show p.logicConnector
	<> ", clauseValue: "    <> "(DBObject)"
	<> ", predicate: "      <> "(Either String (FromDBObject t => t -> DBObject)"
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
