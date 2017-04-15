(**Anirudh Venkatesh
This file does the parsing and building of the tree**)

use "buildTokens.sml";
use "astDataTypes.sml";

fun parseExpression (tok: token) fstr =
(**   if matchIdentifier tok then
      let
         val (tok1, expr) = stringFromId tok fstr
      in
         (tok1, EXP_ID expr) 
      end
   else if matchNum tok then
      let      
         val (tok1, expr) = numFromTok tok fstr
      in
         (tok1, EXP_NUM expr)
      end
**)
   if tok = TK_PLUS then
      let
         val (tok1, expr) = parseSourceElement (nextToken fstr) fstr
         val (tok2, expr2) = parseSourceElement (tok1) fstr
      in 
         (tok2, EXP_PLUS{left = expr, rht = expr2})
      end
   else if tok = TK_TIMES then
      let
         val (tok1, expr) = parseSourceElement (nextToken fstr) fstr
         val (tok2, expr2) = parseSourceElement (tok1) fstr
      in
         (tok2, EXP_TIMES{left = expr, rht = expr2})
      end
   else if tok = TK_LAMBDA then
      let
         val (tok1, expr) = parseLambda (nextToken fstr) fstr
      in
         (tok1, expr)
      end
   else if tok = TK_IFLEQ then
      let
         val (tok1, expr) = parseSourceElement (nextToken fstr) fstr
         val (tok2, expr2) = parseSourceElement (tok1) fstr
         val (tok3, expr3) = parseSourceElement (tok2) fstr
      in
         (tok3, EXP_IFLEQ {guard = expr, thenstmt = expr2, elsestmt = expr3})
      end
   else if tok = TK_PRINT then
      let
         val (tok1, expr) = parseSourceElement (nextToken fstr) fstr
      in
         (tok1, EXP_PRINT{expr = expr})
      end 
   else
      let
         val (tok1, expr) = parseSourceElement (tok) fstr
         val (tok2, expr2) = parseSourceElement (tok1) fstr
      in
         (tok2, EXP_STATEMENT{first = expr, second = expr2})
      end 

(**fun parseExpression (tok as TK_LAMBDA) fstr =
   let
      val (tok1, expr) = parseLambda (nextToken fstr) fstr
   in
      (tok1, expr)
   end 
| parseExpression (tok: token) fstr =
   (TextIO.output(TextIO.stdErr, "not a keyword " ^ returnString(tok)); OS.Process.exit(OS.Process.failure))
**)

and stringFromId (TK_ID strng) fstr =
   ((nextToken fstr), strng)

and matchIdentifier id =
   case id of
      TK_ID id => true
      |_ => false

and matchNum num =
   case num of
      TK_NUM num => true
      |_ => false

and numFromTok (TK_NUM num) fstr =
   ((nextToken fstr), num)

and parseLambda (tok: token) fstr =
   if tok = TK_LPAREN then
      let 
         val nt = nextToken fstr
      in
         if matchIdentifier nt then
            let
               val (tok1, strng) = stringFromId nt fstr
            in
               if tok1 = TK_RPAREN then
                  let
                     val (tok2, data) = parseSourceElement (nextToken fstr) fstr
                  in
                     (tok2, EXP_LAMBDA {id = strng, returnedVal = data})
                  end
               else
                  (TextIO.output(TextIO.stdErr, "missing rht paren"); OS.Process.exit(OS.Process.failure))
            end
         else
            (TextIO.output(TextIO.stdErr, "not an identifier"); OS.Process.exit(OS.Process.failure))
      end
   else
      (TextIO.output(TextIO.stdErr, "missing left paren");
OS.Process.exit(OS.Process.failure))

and parseSourceElement (TK_LPAREN) fstr =
   let
      val (tok1, expr) = parseExpression (nextToken fstr) fstr
   in
      if tok1 = TK_RPAREN then
         (nextToken fstr, expr)
      else 
         (TextIO.output(TextIO.stdErr, "errors yay" ^ returnString(tok1));OS.Process.exit OS.Process.failure)
   end
| parseSourceElement (TK_NUM x) fstr =
   (nextToken fstr, EXP_NUM x)
| parseSourceElement (TK_ID x) fstr =
   (nextToken fstr, EXP_ID x)
| parseSourceElement (tok: token) fstr =
   (TextIO.output(TextIO.stdErr, "errors yay" ^ returnString(tok));
OS.Process.exit(OS.Process.failure))

and checkEOFParser (tok: token)=
   if tok <> TK_EOF then
      (TextIO.output(TextIO.stdErr, "not eof" ^ returnString(tok));OS.Process.exit(OS.Process.failure))
   else
      []

and checkFirstSetSource (tok: token) =
   case tok of
    TK_NUM n => true
   |TK_ID n => true
   |TK_LPAREN => true
   |_ => false

fun parseProgram (tok: token) fstr =
   if (checkFirstSetSource tok) then
      let
         val (tok1,srcElem) = parseSourceElement tok fstr
      in
         (srcElem)::(parseProgram tok1 fstr)
      end
   else
      checkEOFParser tok
;

fun parse fileName =
   let
      val fstr = TextIO.openIn(fileName)
      val tok = nextToken fstr
      val elementList = parseProgram tok fstr
   in
      PROGRAM {elems = (elementList)}
   end       
;  
