import Data.Char (chr, ord)
import Control.Monad.State as MS
import Control.Monad.Par as MP
import Control.DeepSeq
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text


data IList a = Nil | Cons a (IList a)
type Stream a = IVar (IList a)
type Name = String
data Term = Var Name | Lam Name Term | App Term Term
type TVar = Int
data Type = TVar TVar | Arr Type Type
  deriving Eq

instance NFData (Type) where
  rnf e = ()

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons x Nil) = rnf x
  rnf (Cons x xs) = rnf x


instance Show Type where
  show (TVar α) = greek (α `mod` 5) : index (α `div` 5)
    where
      greek n = chr (ord 'α' + n)
      index 0 = ""
      index n = show n
  show (Arr t₁ t₂) = "(" ++ show t₁ ++ " → " ++ show t₂ ++ ")"

instance Show a => Show (IList a) where
  show (Nil) = "[]"
  show list = "[" ++ (aux list) 
    where
         aux (Nil) = "]"
         aux (Cons x xs) = show x ++ aux xs 
    
type Constraint = (Type, Type)
type Constraints = [Constraint]

type AnnotateState a = State (Int, [(Name, TVar)]) a

annotate :: Term -> (Type, Constraints)
annotate e = evalState (aux e) (0, [])
  where
    aux :: Term -> AnnotateState (Type, Constraints)
    aux (Var x) = do
      t <- getType x
      return (t, [])
    aux (Lam x e) = do
      t <- bind x
      (s, cs) <- aux e
      unbind x
      return (Arr t s, cs)
    aux (App e₁ e₂) = do
      (t₁, cs₁) <- aux e₁
      (t₂, cs₂) <- aux e₂
      s <- newType
      return (s, (t₁, Arr t₂ s) : cs₁ ++ cs₂)

newType :: AnnotateState Type
newType = do
  (next, env) <- MS.get
  MS.put (next + 1, env)
  return $ TVar next

getType :: Name -> AnnotateState Type
getType x = do
  (next, env) <- MS.get
  case lookup x env of
    Nothing -> do
      MS.put (next + 1, (x, next) : env)
      return $ TVar next
    Just α -> return $ TVar α

bind :: Name -> AnnotateState Type
bind x = do
  (next, env) <- MS.get
  MS.put (next + 1, (x, next) : env)
  return $ TVar next

unbind :: Name -> AnnotateState ()
unbind x = do
  (next, env) <- MS.get
  MS.put (next, removeFirst x env)

removeFirst :: Name -> [(Name, TVar)] -> [(Name, TVar)]
removeFirst _ [] = []
removeFirst x ((y, _) : env) | x == y = env
removeFirst x (p : env) = p : removeFirst x env

type Solution = [(TVar, Type)]

subst :: TVar -> Type -> Type -> Type
subst α s (TVar β) | α == β = s
subst α s (Arr t₁ t₂) = Arr (subst α s t₁) (subst α s t₂)
subst _ _ t = t

resolve :: Constraints -> Solution
resolve = aux []
  where
    aux :: Solution -> Constraints -> Solution
    aux sol [] = sol
    aux sol ((t, s) : cs) | t == s = aux sol cs
    aux sol ((Arr t₁ t₂, Arr s₁ s₂) : cs) = aux sol ((t₁, s₁) : (t₂, s₂) : cs)
    aux sol ((TVar α, t) : _) | α `occurs` t = error "occur check"
    aux sol ((TVar α, t) : cs) = aux ((α, t) : sol') cs'
      where
        sol' = substInSolution α t sol
        cs'  = substInConstraints α t cs
    aux sol ((t, s) : cs) = aux sol ((s, t) : cs)

    substInSolution :: TVar -> Type -> Solution -> Solution
    substInSolution α s = map (\(β, t) -> (β, subst α s t))

    substInConstraints :: TVar -> Type -> Constraints -> Constraints
    substInConstraints α s = map (\(t₁, t₂) -> (subst α s t₁, subst α s t₂))

    occurs :: TVar -> Type -> Bool
    occurs α (TVar β) = α == β
    occurs α (Arr t₁ t₂) = α `occurs` t₁ || α `occurs` t₂

inference :: Term -> Type
inference e = foldr (uncurry subst) t sol
  where
    (t, cs) = annotate e
    sol = resolve cs


inferenceExps :: [Term] -> Par[Type]
inferenceExps exps = do
        c1 <- new
        c2 <- new
        fork(MP.put c1 ((map (inference) . fstpart) exps))
        fork(MP.put c2 ((map (inference) . sndpart) exps))
        rc1 <- MP.get c1
        rc2 <- MP.get c2
        return (rc1 ++ rc2)
    where
      fstpart exps = drop (div (length exps) 2) exps
      sndpart exps = take (div (length exps) 2) exps

strToList :: NFData a => IList a -> Par (Stream a)
strToList Nil = error "the list is empty"
strToList list =  do
        var <- new
        fork $ MP.put var list
        return var

infExps :: IList Term -> Par (Stream (Type))
infExps exps = (strToList . iMap ( inference)) exps
  where
       iMap :: (a -> b) -> IList a -> IList b
       iMap f Nil = Nil
       iMap f (Cons x xs) = Cons (f x) (iMap f xs)

getStr :: Par (Stream a) -> Stream a
getStr str = runPar str

getIlist :: Stream a -> IList a
getIlist str = runPar (MP.get str)

e_id :: Term
e_id = Lam "x" $ Var "x"

e_twice :: Term
e_twice = Lam "f" $ Lam "x" $ App (Var "f") (App (Var "f") (Var "x"))

e_comp :: Term
e_comp = Lam "f" $ Lam "g" $ Lam "x" $ App (Var "f") (App (Var "g")
                                                      (Var "x"))
e_omega :: Term
e_omega = Lam "x" $ (App (Var "x") (Var "x"))

e_bad :: Term
e_bad = App (Var "x") (Lam "x" (Var "x"))

fun :: Term
fun = Lam "x" $ Lam "y" $ Lam "z" $ App (Var "x") (App (Var "y") (Var "z"))

main :: IO ()
main = (mapM_ print . runPar . inferenceExps) [e_id, e_twice, e_comp, fun]
