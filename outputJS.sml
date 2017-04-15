(**Anirudh Venkatesh
Main file which we run the program with. It calls the parse function which
builds the tree. This file has all the code to output the tree as js code **)

use "parseFile.sml";

fun out file s = TextIO.output (file, s);

fun expressionString (EXP_NUM n) =
   if n < 0 then "-" ^ (Int.toString (~n)) ^ " " else (Int.toString n) ^ " "
  | expressionString (EXP_ID s) = (String.toString s) ^ " "
  | expressionString (EXP_LAMBDA {id: string, returnedVal: expression})= 
      ("(function(" ^id^"){ return " ^ (expressionString returnedVal) ^ "})") 
  | expressionString (EXP_PLUS {left: expression, rht: expression})=
      ("(" ^(expressionString left) ^ " + " ^ (expressionString rht) ^ ")")
  | expressionString (EXP_TIMES {left: expression, rht: expression})=
      ("(" ^ (expressionString left) ^ " * " ^ (expressionString rht) ^ ")")
  | expressionString (EXP_IFLEQ {guard: expression, thenstmt: expression, elsestmt: expression}) =
      ("(" ^ (expressionString guard) ^ "<=0 ?" ^ (expressionString thenstmt) ^ ":" ^ (expressionString elsestmt) ^ ")")
  | expressionString (EXP_PRINT {expr: expression}) = 
      ("( console.log (" ^ (expressionString expr) ^ "))")
  | expressionString (EXP_STATEMENT {first: expression, second: expression})=
      ("(" ^ (expressionString first) ^ "(" ^ (expressionString second) ^ "))")

and sourceElementsString els =
   String.concat (List.map expressionString els)
;

fun programString (PROGRAM {elems}) =
   sourceElementsString elems

fun printAST () =
   let
      val outfile = TextIO.openOut("10k-primesOutput.out")
   in
      out outfile (programString (parse ()))
   end
;
 
