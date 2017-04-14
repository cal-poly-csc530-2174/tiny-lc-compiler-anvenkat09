val keywords = ["else", "false", "function", "if", "new", "println", "return",
"this", "true", "typeof", "undefined", "var", "while", "lambda", "ifleq0"]
val mathExp = [#"{", #"}", #"(", #")", #"[", #"]", #",", #";", #"?", #":", #".",
#"+", #"*", #"/", #"%", #"<", #">", #"!", #"=", #"|", #"&"]
val checkExp = [#"<", #">", #"!", #"=", #"_", #"-"]

datatype token =
    TK_ELSE
   |TK_FALSE
   |TK_FUNCTION
   |TK_IF
   |TK_IFLEQ
   |TK_NEW
   |TK_PRINT
   |TK_RETURN
   |TK_THIS
   |TK_TRUE
   |TK_UNDEFINED
   |TK_TYPEOF
   |TK_VAR
   |TK_WHILE
   |TK_ASSIGN
   |TK_LBRACE
   |TK_RBRACE
   |TK_LPAREN
   |TK_RPAREN
   |TK_LBRACKET
   |TK_COMMA
   |TK_RBRACKET
   |TK_SEMI
   |TK_LAMBDA
   |TK_QUESTION
   |TK_DOT
   |TK_COLON
   |TK_PLUS
   |TK_TIMES
   |TK_DIVIDE
   |TK_MOD
   |TK_LT
   |TK_GT
   |TK_NOT
   |TK_NE
   |TK_EQ
   |TK_GE
   |TK_LE
   |TK_STRING of string
   |TK_NUM of int
   |TK_ID of string
   |TK_OR
   |TK_AND
   |TK_EOF
;

fun returnType (st: string) =
   case st of
       "else" => TK_ELSE
      |"false" => TK_FALSE 
      |"lambda" => TK_LAMBDA
      |"ifleq0" => TK_IFLEQ
      |"function" => TK_FUNCTION
      |"if" => TK_IF
      |"new" => TK_NEW
      |"println" => TK_PRINT
      |"return" => TK_RETURN
      |"this" => TK_THIS
      |"true" => TK_TRUE
      |"undefined" => TK_UNDEFINED
      |"typeof" => TK_TYPEOF
      |"var" => TK_VAR
      |"while" => TK_WHILE
      |"=" => TK_ASSIGN
      |"{" => TK_LBRACE
      |"}" => TK_RBRACE
      |"(" => TK_LPAREN
      |")" => TK_RPAREN
      |"[" => TK_LBRACKET
      |"," => TK_COMMA
      |"]" => TK_RBRACKET
      |";" => TK_SEMI
      |"?" => TK_QUESTION
      |"." => TK_DOT
      |":" => TK_COLON
      |"+" => TK_PLUS
      |"*" => TK_TIMES
      |"/" => TK_DIVIDE
      |"%" => TK_MOD
      |"<" => TK_LT
      |">" => TK_GT
      |"!" => TK_NOT
      |"!=" => TK_NE
      |"==" => TK_EQ
      |">=" => TK_GE
      |"<=" => TK_LE
      |"||" => TK_OR
      |"&&" => TK_AND
;

fun returnString (tok: token) =
   case tok of
       TK_ELSE => "else"
      |TK_LAMBDA => "lambda"
      |TK_FALSE => "false" 
      |TK_FUNCTION => "function"
      |TK_IFLEQ => "ifleq0"
      |TK_IF => "if"
      |TK_NEW => "new"
      |TK_PRINT => "println"
      |TK_RETURN => "return"
      |TK_THIS => "this"
      |TK_TRUE => "true"
      |TK_UNDEFINED => "undefined"
      |TK_TYPEOF => "typeof"
      |TK_VAR => "var"
      |TK_WHILE => "while"
      |TK_ASSIGN => "="
      |TK_LBRACE => "{"
      |TK_RBRACE => "}"
      |TK_LPAREN => "("
      |TK_RPAREN => ")"
      |TK_LBRACKET => "["
      |TK_COMMA => ","
      |TK_RBRACKET => "]"
      |TK_SEMI => ";"
      |TK_QUESTION => "?"
      |TK_DOT => "."
      |TK_COLON => ":"
      |TK_PLUS => "+"
      |TK_TIMES => "*"
      |TK_DIVIDE => "/"
      |TK_MOD => "%"
      |TK_LT => "<"
      |TK_GT => ">"
      |TK_NOT => "!"
      |TK_NE => "!="
      |TK_EQ => "=="
      |TK_GE => ">="
      |TK_LE => "<="
      |TK_NUM n => Int.toString n
      |TK_ID i => i
      |TK_STRING s => s
      |TK_OR => "||"
      |TK_AND => "&&"
      |TK_EOF => "eof"
;

(** reads strings starting with quotation marks **)
fun readString instream (appendString: string) =
   let
      val newChar = TextIO.lookahead instream
   in
      (**checks for extra characters like \b \n \t etc **)
      if isSome(newChar) andalso Option.valOf(newChar) = #"\\"
         then (TextIO.input1(instream); 
            if (Option.valOf(TextIO.lookahead instream) = #"b")
               then ((TextIO.input1 instream); readString instream (appendString ^ "\b"))
            else if (Option.valOf(TextIO.lookahead instream) = #"v")
               then ((TextIO.input1 instream); readString instream (appendString ^ "\v"))
            else if (Option.valOf(TextIO.lookahead instream) = #"n")
               then ((TextIO.input1 instream); readString instream (appendString ^ "\n"))
            else if (Option.valOf(TextIO.lookahead instream) = #"t")
               then ((TextIO.input1 instream); readString instream (appendString ^ "\t"))
            else if (Option.valOf(TextIO.lookahead instream) = #"r")
               then ((TextIO.input1 instream); readString instream (appendString ^ "\r"))
            else if (Option.valOf(TextIO.lookahead instream) = #"f")
               then ((TextIO.input1 instream); readString instream (appendString ^ "\f"))
            else if (Option.valOf(TextIO.lookahead instream) = #"\"")
               then ((TextIO.input1 instream); readString instream(appendString ^ "\""))
            else if(Option.valOf(TextIO.lookahead instream) = #"\\")
               then ((TextIO.input1 instream); readString instream (appendString ^ "\\"))
            else
               (TextIO.output(TextIO.stdErr, "invalid escape sequence: '" ^ "\\" ^ (str(Option.valOf(TextIO.lookahead(instream)))) ^ "'\n");
               OS.Process.exit(OS.Process.failure))
            )   
      (** checks for the end of the string. end quote marks = " **)
      else if isSome(newChar) andalso Option.valOf(newChar) = #"\""
         then (TextIO.input1(instream);
            TK_STRING(appendString)
               )
      (** if still not end, then keeps adding to current string **)
      else if isSome(newChar) 
         then readString instream (appendString ^ (str(Option.valOf(TextIO.input1 instream))))

      (** means that it resulted in NONE and that means that there were no terminating quotes **)
      else
         (TextIO.output(TextIO.stdErr, "string not terminated" ^ "\n"); OS.Process.exit(OS.Process.failure))
   end
;

(** reads strings starting with an integer value **)
fun readInt instream (st: string) =
   let
      val next = TextIO.lookahead instream
   in
      (**checks value ahead and reports if its a letter, if so, it appends to
current string and passes it back in**)
      if isSome(next) andalso Char.isDigit(Option.valOf(next))
         then readInt instream (st ^ str(Option.valOf(TextIO.input1(instream))))
      (** if its not a letter, then it doesn't consume the character, but breaks
the recursion as current token is done **)
      else
         (** NEED TO PRINT VALUE HERE **)
         TK_NUM(Option.valOf(Int.fromString st))
   end
;

(**Skips space if start of string is space and keeps checking, otherwise breaks**)
fun skipSpace instream =
   let
      val next = TextIO.lookahead instream
   in
      if isSome(next) andalso Char.isSpace(Option.valOf(next))
         then (TextIO.input1(instream);skipSpace instream) 
      else
         ()
   end
;

(** compares to math list and outputs it if it belongs in the list as a valid
math exp, special cases are checked with if statements like ==, &&, ||, !=, <=,
and >= **)
fun readSymbols instream =
   let
      val newChar = Option.valOf(TextIO.lookahead(instream))
   in
      (TextIO.input1(instream);
      if (List.exists (fn neC => (neC = newChar)) mathExp) then
      
         if newChar = #"=" then
            if isSome(TextIO.lookahead instream) andalso str(Option.valOf(TextIO.lookahead instream)) = "="
               then (TextIO.input1(instream); 
                  returnType("=="))
            else
               returnType(str(newChar))
      
         else if newChar = #"&" then
            if isSome(TextIO.lookahead instream) andalso str(Option.valOf(TextIO.lookahead instream)) = "&" 
               then (TextIO.input1(instream);
                  returnType("&&"))
            else
              (TextIO.output(TextIO.stdErr, "invalid symbol: '" ^ str(newChar) ^ "'\n");OS.Process.exit(OS.Process.failure))
         
         else if newChar = #">" then
            if isSome(TextIO.lookahead instream) andalso str(Option.valOf(TextIO.lookahead instream)) = "="
               then (TextIO.input1(instream); 
                  returnType(">="))
            else
               returnType(str(newChar))

         else if newChar = #"<" then
            if isSome(TextIO.lookahead instream) andalso str(Option.valOf(TextIO.lookahead instream)) = "="
               then (TextIO.input1(instream);
                  returnType("<="))
            else
               returnType(str(newChar))

         else if newChar = #"!" then
            if isSome(TextIO.lookahead instream) andalso str(Option.valOf(TextIO.lookahead instream)) = "="
               then (TextIO.input1(instream);
                  returnType("!="))
            else
               returnType(str(newChar))

         else if newChar = #"|" then
            if isSome(TextIO.lookahead instream) andalso str(Option.valOf(TextIO.lookahead instream)) = "|"
               then (TextIO.input1(instream); 
                  returnType("||"))
            else   
               (TextIO.output(TextIO.stdErr, "invalid symbol: '" ^ str(newChar) ^ "'\n");OS.Process.exit(OS.Process.failure))

         else
            returnType(str(newChar))
      else
         (TextIO.output(TextIO.stdErr, "invalid symbol: '" ^ str(newChar) ^ "'\n"); OS.Process.exit(OS.Process.failure))
     ) 
   end
;
      
(**Prints the alphanumeric characters based on whether they are identifiers or
keywords by comparing them to the list of keywords **)
fun printAlphaNumeric [] (st: string) =
   TK_ID(st)
| printAlphaNumeric kwds (st:string) =
   if st = (hd kwds)
      then returnType(st)
   else
      printAlphaNumeric (tl kwds) st
;

(** deals with the strings that start with a letter but can also have numbers in
them, calls the print function and prints it out at the end **)
fun readAlphaNumeric instream (st: string) =
   let
      val next = TextIO.lookahead instream
   in
      if isSome(next) andalso (Char.isAlphaNum(Option.valOf(next)) orelse (List.exists (fn x => x = Option.valOf(next)) checkExp))
         then readAlphaNumeric instream (st ^ str(Option.valOf(TextIO.input1 instream)))  
      else
         printAlphaNumeric keywords st
   end
;

(**main function, basically looks at current char when other functions are done
+ also looks at very first char, and triggers the respective functions based on
what that char is **)
fun recognizeToken instream =
   let
      val newChar = TextIO.lookahead(instream)
   in 
(**      if isSome(newChar) then
         if Char.isAlpha(Option.valOf(newChar))
            then readAlphaNumeric instream ""
         else if Char.isSpace(Option.valOf(newChar))
            then ((skipSpace instream); recognizeToken instream)
         else if Char.isDigit(Option.valOf(newChar))
            then readInt instream ""
         else if Option.valOf(newChar) = #"\""
            then (TextIO.input1(instream);readString instream "")
         else
            readSymbols instream
      else 
         TK_EOF
**)
      if isSome(newChar) then
         if Char.isSpace(Option.valOf(newChar))
            then ((skipSpace instream); recognizeToken instream)
         else if Char.isDigit(Option.valOf(newChar))
            then readInt instream ""
         else if Option.valOf(newChar) = #"\""
            then (TextIO.input1(instream);readString instream "")
         else if (List.exists (fn x => x = Option.valOf(newChar)) mathExp) 
            then readSymbols instream
         else
            readAlphaNumeric instream ""
      else 
         TK_EOF
   end
;

fun nextToken instream = 
   recognizeToken(instream)
;
