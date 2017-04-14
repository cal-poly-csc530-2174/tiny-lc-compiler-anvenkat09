(**Anirudh Venkatesh
This file contains the different datatypes which will be used when building the
tree**)
datatype expression =
     EXP_ID of string
   | EXP_NUM of int
   | EXP_LAMBDA of {id: string, returnedVal: expression} 
   | EXP_PLUS of {left: expression, rht: expression}
   | EXP_TIMES of {left: expression, rht: expression}
   | EXP_IFLEQ of {guard: expression, thenstmt: expression, elsestmt: expression}
   | EXP_PRINT of {expr: expression}
   | EXP_STATEMENT of {first: expression, second: expression}
;

datatype program =
  PROGRAM of {elems: expression list}
;
