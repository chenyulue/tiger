{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
module Ch01.SLP where

import Data.Functor ( void )
import Control.Monad.Except

-- Program 1.5: Tree representation of a straignt-line program
type ID = String

data BinOp = Plus | Minus | Times | Div deriving (Show, Eq)

data Stm = CompoundStd Stm Stm
         | AssignStm ID Exp
         | PrintStm [Exp]
         deriving (Show, Eq)

data Exp = IDExp ID
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp
         deriving (Show, Eq)

-- a := 5 + 3; b := (print(a, a-1), 10*a); print(b)
-- Tree representation for the above program        
prog :: Stm
prog =  CompoundStd (AssignStm "a" 
                               (OpExp (NumExp 5) 
                                      Plus 
                                      (NumExp 3)))
                    (CompoundStd (AssignStm "b" 
                                            (EseqExp (PrintStm [IDExp "a", 
                                                                OpExp (IDExp "a") 
                                                                      Minus 
                                                                      (NumExp 1)]) 
                                                     (OpExp (NumExp 10) 
                                                            Times 
                                                            (IDExp "a"))))
                                 (PrintStm [IDExp "b"]))

-- maxArgs tells the maximum number of arguments of any print statement  
-- within any subexpression of agiven statement.
maxArgs :: Stm -> Int
maxArgs = \case
    CompoundStd stm1 stm2 -> max (maxArgs stm1) (maxArgs stm2)
    AssignStm _ exp -> maxArgsInExp exp
    PrintStm expList -> max (length expList) $ maximum (map maxArgsInExp expList)
    where
      maxArgsInExp (EseqExp stm exp) = max (maxArgs stm) (maxArgsInExp exp)
      maxArgsInExp (OpExp exp1 _ exp2) = max (maxArgsInExp exp1) (maxArgsInExp exp2)
      maxArgsInExp _ = 0
-- >>> maxArgs prog
-- 2

-- interp interprets a program in this language.
interp :: Stm -> IO ()
interp stm = do 
      result <- runExceptT $ interpStm stm []
      case result of
            Left errInfo -> putStrLn errInfo
            Right _ -> return ()

type Env = [(ID, Int)]

interpExp :: Exp -> Env -> ExceptT String IO (Int, Env)
interpExp (IDExp id) env = 
      case lookup id env of
            Just result -> return (result, env) 
            Nothing -> throwError $ "Error: " ++ id ++ " is not defined."
            
interpExp (NumExp num) env = return (num, env)

interpExp (OpExp exp1 op exp2) env1 = do
      (result1, env2) <- interpExp exp1 env1
      (result2, env3) <- interpExp exp2 env2
      case op of
            Plus -> return (result1 + result2, env3)
            Minus -> return (result1 - result2, env3)
            Times -> return (result1 * result2, env3)
            Div -> return (div result1 result2, env3)

interpExp (EseqExp stm exp) env1 = do
      env2 <- interpStm stm env1
      interpExp exp env2

interpStm :: Stm -> Env -> ExceptT String IO Env 
interpStm (CompoundStd stm1 stm2) env1 = interpStm stm1 env1 >>= interpStm stm2

interpStm (AssignStm id exp) env1 = do
      (result, env2) <- interpExp exp env1
      return $ (id, result) : env2

interpStm (PrintStm []) env = liftIO (putChar '\n') >> return env

interpStm (PrintStm (exp:exps)) env1 = do
      (result, env2) <- interpExp exp env1
      liftIO (putStr (show result) >> putChar ' ')
      interpStm (PrintStm exps) env2

-- >>> interp prog
-- ()

{- Preliminary implemention
interp :: Stm -> IO ()
interp stm = do
      result <- interpStm stm []
      case result of
            Left errInfo -> putStrLn errInfo
            Right _ -> return ()

type Table = [(ID, Int)]

interpStm :: Stm -> Table -> IO (Either String Table)
interpStm (CompoundStd stm1 stm2) tb1 = do
      table <- interpStm stm1 tb1
      case table of
            Right tb2 -> interpStm stm2 tb2
            errInfo@(Left _) -> return errInfo

interpStm (AssignStm id exp) tb1 = do
      table <- interpExp exp tb1
      case table of
            Right (result, tb2) -> return . Right $ (id, result) : tb2
            Left errInfo -> return (Left errInfo)

interpStm (PrintStm []) tb = putChar '\n' >> return (Right tb)
interpStm (PrintStm (exp:exps)) tb1 = do
      table <- interpExp exp tb1
      case table of
            Right (result, tb2) -> putStr (show result) >> putChar ' ' >> interpStm (PrintStm exps) tb2
            Left errInfo -> return (Left errInfo)

interpExp :: Exp -> Table -> IO (Either String (Int, Table))
interpExp (IDExp id) tb = 
      case lookup id tb of
            Just n -> return $ Right (n, tb)
            Nothing -> return . Left $ "Error: " ++ id ++ " is not defined."

interpExp (NumExp n) tb = return $ Right (n, tb)

interpExp (OpExp exp1 op exp2) tb1 = do
      result1 <- interpExp exp1 tb1
      case result1 of
            Left errInfo -> return . Left $ errInfo
            Right (r1, tb2) -> do
                  result2 <- interpExp exp2 tb2
                  case result2 of
                        Left errInfo -> return . Left $ errInfo
                        Right (r2, tb3) -> case op of
                              Plus -> return . Right $ (r1 + r2, tb3)
                              Minus -> return . Right $ (r1 - r2, tb3)
                              Times -> return . Right $ (r1 * r2, tb3)
                              Div -> return . Right $ (div r1 r2, tb3)

interpExp (EseqExp stm exp) tb1 = do
      table <- interpStm stm tb1
      case table of
            Left errInfo -> return . Left $ errInfo
            Right tb2 -> interpExp exp tb2
-}

-- Exercises 1.1
type Key = String
data Tree = Leaf | Node Tree Key Tree deriving (Show, Eq)


