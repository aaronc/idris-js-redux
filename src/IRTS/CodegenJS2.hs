module IRTS.CodegenJS2(codegenJS2) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import IRTS.Defunctionalise
import Idris.Core.TT

import Data.Maybe
import Data.Char

codegenJS2 :: CodeGenerator
codegenJS2 ci = do let out = concatMap genDDecl (defunDecls ci) ++ "\n" ++
                             concatMap doDebug (simpleDecls ci) 
                   writeFile (outputFile ci) (helpers ++ "\n" ++
                                                        out ++ "\n" ++ 
                                                        start ++ "\n" ++ 
                                              "\n\n")

start = js2name (sMN 0 "runMain") ++ "();"

helpers = errCode ++ "\n" ++ 
          doEcho ++ "\n" ++
          doRead ++ "\n" ++
          doAppend ++ "\n"

errCode = "function error($str) { console.log(\"$str\\n\"); exit(0); }"
doEcho = "function idris_writeStr($str) { console.log($str); }"
doRead = "function idris_readStr() { return fgets(STDIN); }"
doAppend = "function idris_append($l, $r) { return ($l + $r); }"

munge :: Char -> String
munge '<' = "LT"
munge '>' = "GT"
munge '=' = "EQ"
munge '+' = "PLUS"
munge '-' = "MINUS"
munge '&' = "AMP"
munge '|' = "BAR"
munge '(' = "LPAREN"
munge ')' = "RPAREN"
munge '{' = "RBRACE"
munge '}' = "RBRACE"
munge '@' = "CIRCA"
munge '#' = "SHARP"
munge '*' = "STAR"
munge x = show (fromEnum x)

js2name :: Name -> String
js2name n = concatMap js2char (showCG n)
  where js2char x | isAlpha x || isDigit x = [x]
                  | x == '_' = "_"
                  | x == '.' = "$"
                  | x == ' ' = "_"
                  | x == ',' = "_"
                  | otherwise = "_" ++ (munge x) ++ "_"

var :: Name -> String
var n = js2name n

loc :: Int -> String
loc i = "loc" ++ show i

genDDecl :: (Name, DDecl) -> String
genDDecl (n, decl) =
  case decl of
    (DFun _ _ _) -> ""
    (DConstructor name tag arity) -> "var " ++ n ++ " = \"" ++ (show t) ++ "\";\n" where n = (js2name name)


doDebug :: (Name, SDecl) -> String
doDebug (n, decl) =  "/*" ++ (show decl) ++ "*/\n//" ++ (show n) ++ "\n" ++ (doCodegen (n, decl))

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args i def) = cgFun n args def

cgFun :: Name -> [Name] -> SExp -> String
cgFun n args def 
    = "function " ++ js2name n ++ "("
                  ++ showSep "," (map (loc . fst) (zip [0..] args)) ++ ") {\n"
                  ++ cgBody doRet def ++ "\n}\n\n"
  where doRet :: String -> String -- Return the calculated expression
        doRet str = "return " ++ str ++ ";"

-- cgBody converts the SExp into a chunk of js2 which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgBody :: (String -> String) -> SExp -> String
cgBody ret (SV (Glob n)) = ret $ js2name n ++ "()"
cgBody ret (SV (Loc i)) = ret $ loc i 
cgBody ret (SApp _ f args) = ret $ js2name f ++ "(" ++ 
                                   showSep "," (map cgVar args) ++ ")"
cgBody ret (SLet (Loc i) v sc)
   = cgBody (\x -> loc i ++ " = " ++ x ++ ";\n") v ++
     cgBody ret sc
cgBody ret (SUpdate n e)
   = cgBody ret e
cgBody ret (SProj e i)
   = ret $ cgVar e ++ "[" ++ show (i + 1) ++ "]"
cgBody ret (SCon _ t n args)
   = ret $ "[" ++ showSep "," 
              ((js2name n) : (map cgVar args)) ++ "]"
cgBody ret (SCase _ e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SChkCase e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SConst c) = ret $ cgConst c
cgBody ret (SOp op args) = ret $ cgOp op (map cgVar args)
cgBody ret SNothing = ret "0"
cgBody ret (SError x) = ret $ "error( " ++ show x ++ ")"
cgBody ret _ = ret $ "error(\"NOT IMPLEMENTED!!!!\")"

cgAlt :: (String -> String) -> String -> SAlt -> String
cgAlt ret scr (SConstCase t exp)
   = "case " ++ show t ++ ":\n" ++ cgBody ret exp
cgAlt ret scr (SDefaultCase exp) = "default:\n" ++ cgBody ret exp
cgAlt ret scr (SConCase lv t n args exp)
   = "case " ++ js2name n ++ ":\n"
             ++ project 1 lv args ++ "\n" ++ cgBody ret exp
   where project i v [] = ""
         project i v (n : ns) = loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                  ++ project (i + 1) (v + 1) ns

cgVar :: LVar -> String
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show (ord i) -- Treat Char as ints, because JS2 treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x | isTypeConst x = "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus (ATInt _)) [l, r] 
     = "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LMinus (ATInt _)) [l, r] 
     = "(" ++ l ++ " - " ++ r ++ ")"
cgOp (LTimes (ATInt _)) [l, r] 
     = "(" ++ l ++ " * " ++ r ++ ")"
cgOp (LEq (ATInt _)) [l, r] 
     = "(" ++ l ++ " == " ++ r ++ ")"
cgOp (LSLt (ATInt _)) [l, r] 
     = "(" ++ l ++ " < " ++ r ++ ")"
cgOp (LSLe (ATInt _)) [l, r] 
     = "(" ++ l ++ " <= " ++ r ++ ")"
cgOp (LSGt (ATInt _)) [l, r] 
     = "(" ++ l ++ " > " ++ r ++ ")"
cgOp (LSGe (ATInt _)) [l, r] 
     = "(" ++ l ++ " >= " ++ r ++ ")"
cgOp LStrEq [l,r] = "(" ++ l ++ " == " ++ r ++ ")"
cgOp LStrRev [x] = "strrev(" ++ x ++ ")"
cgOp LStrLen [x] = x ++ ".length"
cgOp LStrHead [x] = x ++ "[0]"
cgOp LStrIndex [x, y] = x ++ "[" ++ y ++ "])"
cgOp LStrTail [x] = x ".substr(1)"

cgOp (LIntStr _) [x] = "\"" ++ x ++ "\""
cgOp (LChInt _) [x] = x
cgOp (LIntCh _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
cgOp LReadStr [_] = "idris_readStr()"
cgOp LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp LStrCons [l,r] = "idris_append(chr(" ++ l ++ "), " ++ r ++ ")"
cgOp (LStrInt _) [x] = x
cgOp op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



