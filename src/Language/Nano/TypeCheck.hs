{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

import qualified Data.List as L
import           Text.Printf (printf)  
import           Control.Exception (throw)

--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO Type
typeOfFile f = parseFile f >>= typeOfExpr

typeOfString :: String -> IO Type
typeOfString s = typeOfExpr (parseString s)

typeOfExpr :: Expr -> IO Type
typeOfExpr e = do
  let (!st, t) = infer initInferState preludeTypes e
  if (length (stSub st)) < 0 then throw (Error ("count Negative: " ++ show (stCnt st)))
  else return t

--------------------------------------------------------------------------------
-- Problem 1: Warm-up
--------------------------------------------------------------------------------

-- | Things that have free type variables
class HasTVars a where
  freeTVars :: a -> [TVar]

-- | Type variables of a type
instance HasTVars Type where
  -- freeTVars t     = error "TBD: type freeTVars"
  freeTVars (TVar name) = [ name ]
  freeTVars (TList t) = freeTVars t
  freeTVars (i :=> x)
    | i /= x = freeTVars i ++ freeTVars x
    | otherwise = freeTVars i
  freeTVars _ = []

-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars (Mono t) = freeTVars t
  freeTVars (Forall as t) = freeTVars t L.\\ [as]

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]  
  
-- | Lookup a variable in the type environment  
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend the type environment with a new biding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma  

-- | Lookup a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TVar -> Subst -> Type
lookupTVar a [] = TVar a
lookupTVar a ((v,t):xs)
  | v == a = t
  | xs == [] = TVar a
  | otherwise = lookupTVar a xs

-- | Remove a type variable from a substitution
removeTVar :: TVar -> Subst -> Subst
removeTVar _ [] = []
removeTVar a ((v,t):xs)
  | a == v = xs
  | otherwise = (v,t) : removeTVar a xs
     
-- | Things to which type substitutions can be apply
class Substitutable a where
  apply :: Subst -> a -> a

-- | Apply substitution to type
instance Substitutable Type where  
  -- apply sub t         = error "TBD: type apply"
  apply _ TInt = TInt
  apply _ TBool = TBool
  apply sub (TVar a) = lookupTVar a sub
  apply sub (i :=> x) = apply sub i :=> apply sub x
  apply sub (TList a) = TList (apply sub a)
  
-- | Apply substitution to poly-type
instance Substitutable Poly where    
  -- apply sub s         = error "TBD: poly apply"
  apply sub (Mono t) = Mono (apply sub t)
  apply sub (Forall as t) = Forall as (apply (removeTVar as sub) t)

-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where  
  apply sub to = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip to
      
-- | Apply substitution to a type environment
instance Substitutable TypeEnv where  
  apply sub gamma = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip gamma
      
-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TVar -> Type -> Subst
extendSubst sub a t = (a,apply sub t) : sub
  -- where
  --   sub' = apply sub [(a,t)]
      
--------------------------------------------------------------------------------
-- Problem 2: Unification
--------------------------------------------------------------------------------
      
-- | State of the type inference algorithm      
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
} deriving (Eq,Show)

-- | Initial state: empty substitution; 0 type variables
initInferState = InferState [] 0

-- | Fresh type variable number n
freshTV n = TVar $ "a" ++ show n      
    
-- | Extend the current substitution of a state with a new type assignment   
extendState :: InferState -> TVar -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n
        
-- | Unify a type variable with a type; 
--   if successful return an updated state, otherwise throw an error
unifyTVar :: InferState -> TVar -> Type -> InferState
unifyTVar st a t
  | t == TVar a = st
  | a `elem` freeTVars t = throw (Error ("type error: cannot unify " ++ a ++ " and " ++ (typeString t) ++ " (occurs check)"))
  | otherwise = InferState [(a,t)] 1
    
-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st TInt TInt = st
unify st TBool TBool = st
unify st (TVar a) t = unifyTVar st a t
unify st t (TVar a) = unifyTVar st a t
-- unify st (l :=> r) (l' :=> r') = unify (unify st l l') (apply (stSub st) r) (apply (stSub st) r')
unify st (t1 :=> t2) (t1' :=> t2') = InferState k 0
  where
    a = unify st t1 t1'
    b = apply (stSub a) t2
    c = apply (stSub a) t2'
    d = unify st b c
    k = stSub d ++ stSub a
unify st a b = throw (Error ("type error: cannot unify " ++ (typeString a) ++ " and " ++ (typeString b) ++ " here"))

--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------    
  
infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)
infer st gamma (EVar x)        = (InferState [] i, t)
  where
    (i,t) = instantiate (stCnt st) (lookupVarType x gamma)
infer st gamma (ELam x body)   = (iBody, (apply (stSub iBody) fTV) :=> tBody)
  where
    fTV             = freshTV (stCnt st)
    newState = InferState (stSub st) ((+) (stCnt st) 1)
    gamma'          = extendTypeEnv x (Forall [] (Mono fTV)) gamma
    (iBody, tBody)  = infer newState gamma' body
    

infer st gamma (EApp f e)    = (u, apply (stSub u) tO)
  where
    tO        = freshTV (stCnt st)
    (iF, tF)  = infer st gamma f
    gamma'    = apply (stSub iF) gamma
    (iE, tE)  = infer st gamma' e
    u         = unify iE (apply (stSub iE) tF) (tE :=> tO)
    newSt = InferState (stSub u) ((+) (stCnt u) 1) 
-- infer st gamma (EApp f e)    = (newState, apply (stSub newState) ftv)
--   where
--     ftv = freshTV 0
--     (su1, tF) = infer st gamma f
--     gamma' = apply (stSub su1) gamma
--     (su2, tEs) = infer su1 gamma' e
--     newState = unify (apply (stSub su2) tF) (tEs :=> ftv)
infer st gamma (ELet x e1 e2)  = infer iE1 gamma'' e2
  where
    (iE1, tE1)  = infer st gamma e1 -- infer the type of e1
    gamma'      = apply (stSub iE1) gamma
    s1          = generalize gamma' tE1
    gamma''     = extendTypeEnv x s1 gamma'

infer st gamma (EBin op e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"    
infer st gamma ENil = infer st gamma (EVar "[]")

-- | Generalize type variables inside a type
generalize :: TypeEnv -> Type -> Poly
generalize gamma t = generalizeFold t xs
  where
    ft = freeTVars t
    fg = freeTVars gamma
    xs = (ft L.\\ fg)

generalizeFold :: Type -> [TVar] -> Poly
generalizeFold t [] = Mono t
generalizeFold t (x:xs)
  | xs == [] = Forall x (Mono t)
  | otherwise = Forall x (generalizeFold t xs)
    
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n t = instantiateHelper [] n t
-- instantiate n (Forall as t)

instantiateHelper :: Subst -> Int -> Poly -> (Int,Type)
instantiateHelper sub n (Forall as t) = instantiateHelper ((zip [as] [freshTV n]) ++ sub) (n+1) t
instantiateHelper sub n (Mono t) = (n, apply sub t)
      
-- | Types of built-in operators and functions      
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono $ TInt :=> TInt :=> TInt)
  , ("-",    Mono $ TInt :=> TInt :=> TInt)
  , ("*",    Mono $ TInt :=> TInt :=> TInt)
  , ("/",    Mono $ TInt :=> TInt :=> TInt)
  , ("==",   forall "z1" ("z1" :=> "z1" :=> TBool))
  , ("!=",   forall "z1" ("z1" :=> "z1" :=> TBool))
  , ("<",    forall "z1" ("z1" :=> "z1" :=> TBool))
  , ("<=",   forall "z1" ("z1" :=> "z1" :=> TBool))
  , ("&&",   forall "z1" ("z1" :=> "z1" :=> TBool))
  , ("||",   forall "z1" ("z1" :=> "z1" :=> TBool))
  , ("if",   forall "z1" (TBool :=> "z1" :=> "z1"))
  -- lists:   
  , ("[]",  Mono $ TList "[]" :=> TList "[]")
  , (":",   forall "y1" ("y1" :=> TList "y1" :=> TList "y1"))
  , ("head", forall "y1" (TList "y1" :=> "y1"))
  , ("tail", forall "y1" (TList "y1" :=> "y1"))
  ]