module Abstract.Database.Class
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
	) where

import Prelude
	( class Ord
	, Ordering(EQ, GT, LT)
	, Unit
	, const
	, flip
	, id
	, map
	, pure
	, show
	, unit
	, ($)
	, (<>)
	, (>>=)
	, (<<<)
	, (>>>)
	, (<$>)
	, (<*>)
	)

import Control.Alt             ((<|>))
import Control.Monad.Aff       (Aff)
import Control.Monad.Eff.Ref   (REF, Ref)
import Data.Array              (index, mapWithIndex)
import Data.Foreign
	( Foreign
	, tagOf
	, toForeign
	, unsafeFromForeign
	, writeObject
	) as F
import Data.Foreign            (Prop(Prop))
import Data.Foreign.Null       (Null(Null), writeNull)
import Data.Foreign.NullOrUndefined
	( NullOrUndefined(NullOrUndefined)
	)
import Data.Foreign.Undefined
	( Undefined(Undefined)
	)
import Data.Either
	( Either(Left, Right)
	, either
	)
import Data.Foldable           (class Foldable, foldr)
import Data.DateTime           (DateTime)
import Data.JSDate             (JSDate, fromDateTime, toDateTime)
import Data.List
	( List
	, fromFoldable
	, toUnfoldable
	)
import Data.List.NonEmpty      (NonEmptyList, fromFoldable, toList) as NE
import Data.StrMap             (StrMap, fromFoldable, lookup, toUnfoldable) as M
import Data.Maybe
	( Maybe(Just, Nothing)
	, fromJust
	, fromMaybe
	, maybe
	)
import Data.Set                (Set, fromFoldable, toUnfoldable) as S
import Data.String             (singleton, toChar) as Str
import Data.String.Regex       (Regex, regex, source)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple              (Tuple(Tuple))
import Partial.Unsafe          (unsafePartial)

import Abstract.Database.Types

foreign import toISOString :: JSDate -> String

readProp :: forall a. FromDBObject a =>
	    String                   ->
	    M.StrMap DBObject        ->
	    Either String a
readProp key o = case M.lookup key o of
	Just k -> fromDBObject k
	_      -> Left $ "Could not find key: "
		<> show key
		<> " in DBOBject: "
		<> show o
		<> "."

readPropFlipped :: forall a. FromDBObject a =>
		   M.StrMap DBObject        ->
		   String                   ->
		   Either String a
readPropFlipped = flip readProp
infix 8 readPropFlipped as ..

readProp' :: forall a. FromDBObject a =>
	     String                   ->
	     M.StrMap DBObject        ->
	     Either String (Maybe a)
readProp' key o = case M.lookup key o of
	Just k -> Just <$> fromDBObject k
	_      -> pure Nothing

readProp'Flipped :: forall a. FromDBObject a =>
		    M.StrMap DBObject        ->
		    String                   ->
		    Either String (Maybe a)
readProp'Flipped = flip readProp'
infix 8 readProp'Flipped as .?

readPropAtIndex :: forall a. FromDBObject a =>
		   Int                      ->
		   Array DBObject           ->
		   Either String a
readPropAtIndex i xs = case xs `index` i of
	Just el -> fromDBObject el
	Nothing -> Left $ show xs
		<> " does not have an element at index "
		<> show i <> "."

readPropAtIndexFlipped :: forall a. FromDBObject a =>
			  Array DBObject           ->
			  Int                      ->
			  Either String a
readPropAtIndexFlipped = flip readPropAtIndex
infix 8 readPropAtIndexFlipped as .!! -- .[]

readPropAtIndex' :: forall a. FromDBObject a =>
		   Int                       ->
		   Array DBObject            ->
		   Either String (Maybe a)
readPropAtIndex' i xs = case xs `index` i of
	Just el -> Just <$> fromDBObject el
	Nothing -> pure Nothing

readPropAtIndex'Flipped :: forall a. FromDBObject a =>
			   Array DBObject            ->
			   Int                       ->
			   Either String (Maybe a)
readPropAtIndex'Flipped = flip readPropAtIndex'
infix 8 readPropAtIndex'Flipped as .!!? -- .[]?

readPropDefault :: forall a. a -> Either String (Maybe a) -> Either String a
readPropDefault a = map (fromMaybe a)

readPropDefaultFlipped :: forall a. Either String (Maybe a) ->
			  a                                 ->
			  Either String a
readPropDefaultFlipped = flip readPropDefault
infix 8 readPropDefaultFlipped as ..=

writeProp :: forall a. ToDBObject a => String -> a -> Tuple String DBObject
writeProp k v = Tuple k (toDBObject v)
infixl 8 writeProp as .=

dbObject :: forall f. (Foldable f) => f (Tuple String DBObject) -> DBObject
dbObject = M.fromFoldable >>> DBObject

toForeign :: DBObject -> F.Foreign
toForeign (Boolean      b) = F.toForeign b
toForeign (Int          i) = F.toForeign i
toForeign (Number       n) = F.toForeign n
toForeign (String       s) = F.toForeign s
toForeign (Maybe (Just a)) = F.toForeign $ toForeign a
toForeign (Maybe  Nothing) = writeNull
toForeign (Array        a) = F.toForeign $ map toForeign a
toForeign (DBObject     o) = F.writeObject $ foldr fold [] $ toUnfoldable' o
	where
		toUnfoldable' :: M.StrMap DBObject -> Array (Tuple String DBObject)
		toUnfoldable' = M.toUnfoldable

		fold (Tuple k v) o' = o' <> [ Prop { key: k, value: toForeign v } ]

foreign import objFold :: forall a. ToDBObject a          =>
			  (String -> a -> Tuple String a) ->
			  F.Foreign                       ->
			  Array (Tuple String DBObject)
foreign import toString :: F.Foreign -> String

-- | Attempts to convert a primitiveish Foreign value to a DBObject. The
-- | primitiveish types that are supported are:
-- |
-- | - Undefined
-- | - Null
-- | - Boolean
-- | - Number
-- | - String
-- | - RegExp
-- | - Date
-- | - Array
-- | - Object
-- |
-- | Any supplied Foreign type that is not a member of the above is converted
-- | to a string using its `.toString()` method. **This is strongly *not*
-- | recommended**, and instead it is recommended to create a custom PureScript
-- | type, convert your Foreign to this type, and then create a ToDBObject
-- | instance for that type, then use it.
fromForeign :: F.Foreign -> DBObject
fromForeign f = case F.tagOf f of
	"Undefined" -> Maybe Nothing
	"Null"      -> Maybe Nothing
	"Boolean"   -> Boolean $ F.unsafeFromForeign f
	"Number"    -> Number $ F.unsafeFromForeign f
	"String"    -> String $ F.unsafeFromForeign f
	"RegExp"    -> String $ source $ F.unsafeFromForeign f
	"Date"      -> String $ toISOString $ F.unsafeFromForeign f
	"Array"     -> Array $ map toDBObject $ F.unsafeFromForeign f :: Array F.Foreign
	"Object"    -> dbObject $ objFold writeProp f
	_           -> String $ toString f

typeMismatch :: forall a. String -> DBObject -> Either String a
typeMismatch expected actual = Left $
	"Expected to convert a initial type of `" <> expected    <>
	"`, but instead got: `"                   <> show actual <>
	"`, which has a type of `"                <> typeName    <>
	"`"
	where
		typeName = case actual of
			Boolean  _ -> "Boolean"
			Int      _ -> "Int"
			Number   _ -> "Number"
			String   _ -> "String"
			Maybe    _ -> "Maybe"
			Array    _ -> "Array"
			DBObject _ -> "DBObject"

class FromDBObject a where
	fromDBObject :: DBObject -> Either String a

instance fromDBObjectDBObject :: FromDBObject DBObject where
	fromDBObject = pure <<< id

instance fromDBObjectUndefined :: FromDBObject a => FromDBObject (Undefined a) where
	fromDBObject (Maybe (Just a)) = (Just >>> Undefined) <$> fromDBObject a
	fromDBObject (Maybe  Nothing) = pure $ Undefined Nothing
	fromDBObject               o  = typeMismatch "Maybe" o

instance fromDBObjectNull :: FromDBObject a => FromDBObject (Null a) where
	fromDBObject (Maybe (Just a)) = (Just >>> Null) <$> fromDBObject a
	fromDBObject (Maybe  Nothing) = pure $ Null Nothing
	fromDBObject               o  = typeMismatch "Maybe" o

instance fromDBObjectNullOrUndefined :: FromDBObject a => FromDBObject (NullOrUndefined a) where
	fromDBObject (Maybe (Just a)) = (Just >>> NullOrUndefined) <$> fromDBObject a
	fromDBObject (Maybe  Nothing) = pure $ NullOrUndefined Nothing
	fromDBObject               o  = typeMismatch "Maybe" o

instance fromDBObjectUnit :: FromDBObject Unit where
	fromDBObject (Maybe Nothing) = pure unit
	fromDBObject              o  = typeMismatch "Maybe" o

instance fromDBObjectBoolean :: FromDBObject Boolean where
	fromDBObject (Boolean b) = pure b
	fromDBObject          o  = typeMismatch "Boolean" o

instance fromDBObjectInt :: FromDBObject Int where
	fromDBObject (Int i) = pure i
	fromDBObject      o  = typeMismatch "Int" o

instance fromDBObjectNumber :: FromDBObject Number where
	fromDBObject (Number n) = pure n
	fromDBObject         o  = typeMismatch "Number" o

instance fromDBObjectChar :: FromDBObject Char where
	fromDBObject (String c) = maybe (error c) pure $ Str.toChar c
		where
			error c' = Left $ "Was not able to convert "
				<> show c'
				<> " into a `Char` because its length was too long."

	fromDBObject o = typeMismatch "String" o

instance fromDBObjectString :: FromDBObject String where
	fromDBObject (String s) = pure s
	fromDBObject         o  = typeMismatch "String" o

instance fromDBObjectRegex :: FromDBObject Regex where
	fromDBObject (String r) = either (error r) pure $ regex r noFlags
		where
			error r' e = Left $ "Was not able to convert "
				<> show r'
				<> " into a Regular Expression. The conversion error is:\n\t"
				<> e

	fromDBObject o = typeMismatch "String" o

foreign import parseISODateTimeString :: (String -> Either String JSDate) -> -- Left
					 (JSDate -> Either String JSDate) -> -- Right
					 String                           -> -- The ISO DateTime string
					 Either String JSDate

instance fromDBObjectDateTime :: FromDBObject DateTime where
	fromDBObject (String d) =
		either (error d) toDateTime' $
			parseISODateTimeString Left Right d
		where
			error d' e = Left $ "Was not able to convert "
				<> show d'
				<> " into a DateTime. The conversion error is:\n\t"
				<> e

			fromJust'   = unsafePartial fromJust
			toDateTime' = toDateTime >>> fromJust' >>> pure

	fromDBObject o = typeMismatch "String" o

instance fromDBObjectArray :: FromDBObject a => FromDBObject (Array a) where
	fromDBObject (Array a) = foldr fold (Right []) $ mapWithIndex fromDBObject'' a -- XXX Maybe I can move this to JS to avoid the O(n*2)
		where
			fromDBObject'' i a' = Tuple i (fromDBObject a')

			errorAt :: Int -> String -> String
			errorAt i e = "\tindex " <> show i <> ": " <> e <> "\n"

			firstError :: Int -> String -> String
			firstError i e = "Was not able to convert the entire `Array`. Below are the errors found at their corresponding indices:\n" <> errorAt i e

			fold (Tuple i (Left  e)) (Left  es) = Left $ es <> errorAt i e
			fold (Tuple i (Left  e)) (Right xs) = Left $ firstError i e
			fold (Tuple i (Right x)) (Left  es) = Left es
			fold (Tuple i (Right x)) (Right xs) = Right $ xs <> [x]

	fromDBObject o = typeMismatch "Array" o

instance fromDBObjectForeign :: FromDBObject F.Foreign where
	fromDBObject = pure <<< toForeign

instance fromDBObjectOrdering :: FromDBObject Ordering where
	fromDBObject (String "LT") = pure LT
	fromDBObject (String "EQ") = pure EQ
	fromDBObject (String "GT") = pure GT
	fromDBObject            o  = typeMismatch "String" o

instance fromDBObjectTuple :: (FromDBObject a, FromDBObject b) => FromDBObject (Tuple a b) where
	fromDBObject (Array [a1, a2]) = Tuple <$> (fromDBObject a1) <*> (fromDBObject a2)
	fromDBObject (Array        a) = Left $ "Was not able to convert "
		<> show a
		<> " into a `Tuple` because it does not have *exactly* two elements."

	fromDBObject o = typeMismatch "Array" o

instance fromDBObjectMaybe :: FromDBObject a => FromDBObject (Maybe a) where
	fromDBObject (Maybe (Just a)) = Just <$> fromDBObject a
	fromDBObject (Maybe  Nothing) = pure Nothing
	fromDBObject               o  = typeMismatch "Maybe" o

-- | This instance biases towards attempting to evaluate a `Right` first.
-- | Failing that, it attempts to evaluate a potential `Left`.
instance fromDBObjectEither :: (FromDBObject a, FromDBObject b) => FromDBObject (Either a b) where
	fromDBObject (DBObject o) =
		(o.."Right") <|> (o.."Left") >>= fromDBObject

	fromDBObject o = typeMismatch "DBObject" o

-- | This function biases towards attempting to evaluate a `Left` first.
-- | Failing that, it attempts to evaluate a potential `Right`.
fromDBObjectEither' :: forall a b. (FromDBObject a, FromDBObject b) =>
		       DBObject                                     ->
		       Either String (Either a b)
fromDBObjectEither' (DBObject o) = (o.."Left") <|> (o.."Right") >>= fromDBObject
fromDBObjectEither'           o  = typeMismatch "DBObject" o

instance fromDBObjectList :: FromDBObject a => FromDBObject (List a) where
	fromDBObject (Array a) = fromFoldable' <$> fromDBObject (Array a) -- XXX Maybe I can move this to JS to avoid the O(n*2)
		where
			fromFoldable' :: Array a -> List a
			fromFoldable' = fromFoldable

	fromDBObject o = typeMismatch "Array" o

instance fromDBObjectNonEmptyList :: FromDBObject a => FromDBObject (NE.NonEmptyList a) where
	fromDBObject (Array a) =
		fromDBObject'' (Array a) >>= (NE.fromFoldable >>> maybe error Right) -- XXX Maybe I can move this to JS to avoid the O(n*2)
			where
				fromDBObject'' :: forall a'. FromDBObject a' => DBObject -> Either String (Array a')
				fromDBObject'' = fromDBObject

				error = Left $ "Was not able to convert "
					<> show a
					<> " into a `NonEmptyList` because it was empty."

	fromDBObject o = typeMismatch "Array" o

instance fromDBObjectSet :: (FromDBObject a, Ord a) => FromDBObject (S.Set a) where
	fromDBObject (Array a) =
		S.fromFoldable <$> fromDBObject (Array a) :: Either String (Array a) -- XXX Maybe I can move this to JS to avoid the O(n*2)
	fromDBObject o = typeMismatch "Array" o

instance fromDBObjectStrMap :: (FromDBObject a) => FromDBObject (M.StrMap a) where
	fromDBObject (DBObject o) = map M.fromFoldable $ foldr fold (Right []) $ mapWithIndex fromDBObject'' $ toUnfoldable' o -- XXX Maybe I can move this to JS to avoid the O(n*4)
		where
			toUnfoldable' :: M.StrMap DBObject -> Array (Tuple String DBObject)
			toUnfoldable' = M.toUnfoldable

			fromDBObject'' i (Tuple k a) = Tuple i (Tuple k (fromDBObject a))

			errorAt :: Int -> String -> String
			errorAt i e = "\tindex " <> show i <> ": " <> e <> "\n"

			firstError :: Int -> String -> String
			firstError i e = "Was not able to convert the entire `StrMap`. Below are the errors that were be found at their corresponding indices (indices are based on if you were to turn this `StrMap` into an `Array (Tuple String a)`):\n" <> errorAt i e

			fold (Tuple i (Tuple k (Left  e))) (Left  es) = Left $ es <> errorAt i e
			fold (Tuple i (Tuple k (Left  e))) (Right xs) = Left $ firstError i e
			fold (Tuple i (Tuple k (Right x))) (Left  es) = Left es
			fold (Tuple i (Tuple k (Right x))) (Right xs) = Right $ xs <> [(Tuple k x)]

	fromDBObject o = typeMismatch "DBOBject" o

fromDBObject' :: forall a. FromDBObject a => DBObject -> Maybe a
fromDBObject' = fromDBObject >>> either (const Nothing) pure

class ToDBObject a where
	toDBObject :: a -> DBObject

instance toDBObjectDBObject :: ToDBObject DBObject where
	toDBObject = id

instance toDBObjectUndefined :: ToDBObject a => ToDBObject (Undefined a) where
	toDBObject (Undefined a) = Maybe $ map toDBObject a

instance toDBObjectNull :: ToDBObject a => ToDBObject (Null a) where
	toDBObject (Null a) = Maybe $ map toDBObject a

instance toDBObjectNullOrUndefined :: ToDBObject a => ToDBObject (NullOrUndefined a) where
	toDBObject (NullOrUndefined a) = Maybe $ map toDBObject a

instance toDBObjectUnit :: ToDBObject Unit where
	toDBObject _ = Maybe Nothing

instance toDBObjectBoolean :: ToDBObject Boolean where
	toDBObject = Boolean

instance toDBObjectInt :: ToDBObject Int where
	toDBObject = Int

instance toDBObjectNumber :: ToDBObject Number where
	toDBObject = Number

instance toDBObjectChar :: ToDBObject Char where
	toDBObject = Str.singleton >>> String

instance toDBObjectString :: ToDBObject String where
	toDBObject = String

instance toDBObjectRegex :: ToDBObject Regex where
	toDBObject = source >>> String

instance toDBObjectDateTime :: ToDBObject DateTime where
	toDBObject = fromDateTime >>> toISOString >>> String

instance toDBObjectArray :: ToDBObject a => ToDBObject (Array a) where
	toDBObject = Array <<< map toDBObject

instance toDBObjectForeign :: ToDBObject F.Foreign where
	toDBObject = fromForeign

instance toDBObjectOrdering :: ToDBObject Ordering where
	toDBObject LT = String "LT"
	toDBObject EQ = String "EQ"
	toDBObject GT = String "GT"

instance toDBObjectTuple :: (ToDBObject a, ToDBObject b) => ToDBObject (Tuple a b) where
	toDBObject (Tuple a b) = Array [toDBObject a, toDBObject b]

instance toDBObjectMaybe :: ToDBObject a => ToDBObject (Maybe a) where
	toDBObject = Maybe <<< map toDBObject

instance toDBObjectEither :: (ToDBObject a, ToDBObject b) => ToDBObject (Either a b) where
	toDBObject (Left  l) = dbObject [ "Left"  .= l ]
	toDBObject (Right r) = dbObject [ "Right" .= r ]

instance toDBObjectList :: ToDBObject a => ToDBObject (List a) where
	toDBObject = toUnfoldable' >>> toDBObject
		where
			toUnfoldable' :: List a -> Array a
			toUnfoldable' = toUnfoldable

instance toDBObjectNonEmptyList :: ToDBObject a => ToDBObject (NE.NonEmptyList a) where
	toDBObject = NE.toList >>> toDBObject

instance toDBObjectSet :: ToDBObject a => ToDBObject (S.Set a) where
	toDBObject = toUnfoldable' >>> toDBObject
		where
			toUnfoldable' :: forall a'. ToDBObject a =>
					 S.Set a'                ->
					 Array a'
			toUnfoldable' = S.toUnfoldable

instance toDBObjectStrMap :: ToDBObject a => ToDBObject (M.StrMap a) where
	toDBObject = toUnfoldable' >>> map toProp >>> dbObject -- XXX Maybe I can move this to JS to avoid the O(n*2)
		where
			toUnfoldable' :: M.StrMap a -> Array (Tuple String a)
			toUnfoldable' = M.toUnfoldable

			toProp (Tuple k v) = k .= v

class (FromDBObject a, ToDBObject a) <= IsDBObject a

instance dbObjectIsDBObject        :: IsDBObject DBObject
instance undefinedIsDBObject       :: IsDBObject a => IsDBObject (Undefined a) 
instance nullIsDBObject            :: IsDBObject a => IsDBObject (Null a) 
instance nullOrUndefinedIsDBObject :: IsDBObject a => IsDBObject (NullOrUndefined a) 
instance unitIsDBObject            :: IsDBObject Unit
instance booleanIsDBObject         :: IsDBObject Boolean 
instance intIsDBObject             :: IsDBObject Int 
instance numberIsDBObject          :: IsDBObject Number 
instance charIsDBObject            :: IsDBObject Char 
instance stringIsDBObject          :: IsDBObject String 
instance regexIsDBObject           :: IsDBObject Regex
instance datetimeIsDBObject        :: IsDBObject DateTime
instance arrayIsDBObject           :: IsDBObject a => IsDBObject (Array a)
instance foreignIsDBObject         :: IsDBObject F.Foreign
instance orderingIsDBObject        :: IsDBObject Ordering
instance tupleIsDBObject           :: (IsDBObject a, IsDBObject b) => IsDBObject (Tuple a b)
instance maybeIsDBObject           :: IsDBObject a => IsDBObject (Maybe a)
instance eitherIsDBObject          :: (IsDBObject a, IsDBObject b) => IsDBObject (Either a b)
instance listIsDBObject            :: IsDBObject a => IsDBObject (List a)
instance nonEmptyListIsDBObject    :: IsDBObject a => IsDBObject (NE.NonEmptyList a)
instance setIsDBObject             :: (IsDBObject a, Ord a) => IsDBObject (S.Set a)
instance strMapIsDBObject          :: IsDBObject a => IsDBObject (M.StrMap a)

class Database datastore where
	openConnection :: forall e. String -> Aff (db :: DATABASE | e) datastore

	closeConnection :: forall e. datastore -> Aff (db :: DATABASE | e) Unit

	createCollection :: forall e. String ->
			    datastore        ->
			    Aff (db :: DATABASE | e) Unit

	getCollection    :: forall e t. String ->
			    datastore          ->
			    Aff (db :: DATABASE, ref :: REF | e) (Ref (Collection t))

	deleteCollection :: forall e. String ->
			    datastore        ->
			    Aff (db :: DATABASE | e) Unit

	getWhere :: forall e t. FromDBObject t =>
		    Maybe Pagination           ->
		    Query datastore t          ->
		    Ref (Collection t)         ->
		    Aff (db :: DATABASE, ref :: REF | e) (Maybe t)

	getWhere' :: forall e t. FromDBObject t =>
		    Maybe Pagination            ->
		    Query datastore t           ->
		    Ref (Collection t)          ->
		    Aff (db :: DATABASE, ref :: REF | e) (List t)

	countWhere :: forall e t. FromDBObject t =>
		      Query datastore t          ->
		      Ref (Collection t)         ->
		      Aff (db :: DATABASE, ref :: REF | e) Int

	insert :: forall e t b. ToDBObject t =>
		  t                          ->
		  Ref (Collection t)         ->
		  Aff (db :: DATABASE, ref :: REF | e) b

	insert' :: forall e t b. ToDBObject t =>
		   List t                     ->
		   Ref (Collection t)         ->
		   Aff (db :: DATABASE, ref :: REF | e) b

	deleteWhere :: forall e t b. ToDBObject t =>
		       Query datastore t          ->
		       Ref (Collection t)         ->
		       Aff (db :: DATABASE, ref :: REF | e) b
