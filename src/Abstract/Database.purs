module Abstract.Database
	( module Abstract.Database.Types
	, module Abstract.Database.Class
	, module Abstract.Database.Predicates
	, module Abstract.Database
	) where

import Abstract.Database.Types
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
	)
import Abstract.Database.Class
	( readProp
	, readPropFlipped
	, (..)
	, readProp'
	, readProp'Flipped
	, (.?)
	, readPropAtIndex
	, readPropAtIndexFlipped
	, (.!!)
	, readPropAtIndex'
	, readPropAtIndex'Flipped
	, (.!!?)
	, readPropDefault
	, readPropDefaultFlipped
	, writeProp
	, (.=)
	, dbObject
	, toForeign
	, fromForeign
	, typeMismatch
	, class FromDBObject
	, fromDBObject
	, fromDBObjectEither'
	, fromDBObject'
	, class ToDBObject
	, toDBObject
	, class IsDBObject
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
	)
import Abstract.Database.Predicates
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
	)
