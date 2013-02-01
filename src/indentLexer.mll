{
(*
 We want to lexer OCaml code, line by line. Multi-line tokens must be
split into several tokens with EOL separators.
For example, "(*\nabc\ndef*)" becomes
COMMENT_BEGIN "(*" :: EOL :: COMMENT_INSIDE "abc" :: EOL :: COMMENT_END "def*)"
*)


  type token =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOL
  | EOF
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IN
  | INCLUDE
  | INFIXOP0
  | INFIXOP1
  | INFIXOP2
  | INFIXOP3
  | INFIXOP4
  | INHERIT
  | INITIALIZER
  | INT
  | INT32
  | INT64
  | LABEL
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LESS
  | LESSMINUS
  | LET
  | LIDENT
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL
  | OR
  | PLUS
  | PLUSDOT
  | PREFIXOP
  | PRIVATE
  | QUESTION
  | QUESTIONQUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH

  | COMMENT_BEGIN (* multi-line comments *)
  | COMMENT_INSIDE
  | COMMENT_END

  | STRING_BEGIN
  | STRING_INSIDE
  | STRING_INDENTED
  | STRING_END

  | QUOTATION_BEGIN
  | QUOTATION_INSIDE
  | QUOTATION_END

  | ILLEGAL_CHAR
  | LINENUM

  let string_of_token token =
    match token with
    | AMPERAMPER -> "AMPERAMPER"
    | AMPERSAND -> "AMPERSAND"
    | AND -> "AND"
    | AS -> "AS"
    | ASSERT -> "ASSERT"
    | BACKQUOTE -> "BACKQUOTE"
    | BANG -> "BANG"
    | BAR -> "BAR"
    | BARBAR -> "BARBAR"
    | BARRBRACKET -> "BARRBRACKET"
    | BEGIN -> "BEGIN"
    | CHAR -> "CHAR"
    | CLASS -> "CLASS"
    | COLON -> "COLON"
    | COLONCOLON -> "COLONCOLON"
    | COLONEQUAL -> "COLONEQUAL"
    | COLONGREATER -> "COLONGREATER"
    | COMMA -> "COMMA"
    | CONSTRAINT -> "CONSTRAINT"
    | DO -> "DO"
    | DONE -> "DONE"
    | DOT -> "DOT"
    | DOTDOT -> "DOTDOT"
    | DOWNTO -> "DOWNTO"
    | ELSE -> "ELSE"
    | END -> "END"
    | EQUAL -> "EQUAL"
    | EXCEPTION -> "EXCEPTION"
    | EXTERNAL -> "EXTERNAL"
    | FALSE -> "FALSE"
    | FLOAT -> "FLOAT"
    | FOR -> "FOR"
    | FUN -> "FUN"
    | FUNCTION -> "FUNCTION"
    | FUNCTOR -> "FUNCTOR"
    | GREATER -> "GREATER"
    | GREATERRBRACE -> "GREATERRBRACE"
    | GREATERRBRACKET -> "GREATERRBRACKET"
    | IF -> "IF"
    | IN -> "IN"
    | INCLUDE -> "INCLUDE"
    | INFIXOP0 -> "INFIXOP0"
    | INFIXOP1 ->  "INFIXOP1"
    | INFIXOP2 ->  "INFIXOP2"
    | INFIXOP3 ->  "INFIXOP3"
    | INFIXOP4 ->  "INFIXOP4"
    | INHERIT -> "INHERIT"
    | INITIALIZER -> "INITIALIZER"
    | INT -> "INT"
    | INT32 -> "INT32"
    | INT64 -> "INT64"
    | LABEL -> "LABEL"
    | LAZY -> "LAZY"
    | LBRACE -> "LBRACE"
    | LBRACELESS -> "LBRACELESS"
    | LBRACKET -> "LBRACKET"
    | LBRACKETBAR -> "LBRACKETBAR"
    | LBRACKETLESS -> "LBRACKETLESS"
    | LBRACKETGREATER -> "LBRACKETGREATER"
    | LESS -> "LESS"
    | LESSMINUS -> "LESSMINUS"
    | LET -> "LET"
    | LIDENT -> "LIDENT"
    | LPAREN -> "LPAREN"
    | MATCH -> "MATCH"
    | METHOD -> "METHOD"
    | MINUS -> "MINUS"
    | MINUSDOT -> "MINUSDOT"
    | MINUSGREATER -> "MINUSGREATER"
    | MODULE -> "MODULE"
    | MUTABLE -> "MUTABLE"
    | NATIVEINT -> "NATIVEINT"
    | NEW -> "NEW"
    | OBJECT -> "OBJECT"
    | OF -> "OF"
    | OPEN -> "OPEN"
    | OPTLABEL -> "OPTLABEL"
    | OR -> "OR"
    | PLUS -> "PLUS"
    | PLUSDOT -> "PLUSDOT"
    | PREFIXOP -> "PREFIXOP"
    | PRIVATE -> "PRIVATE"
    | QUESTION -> "QUESTION"
    | QUESTIONQUESTION -> "QUESTIONQUESTION"
    | QUOTE -> "QUOTE"
    | RBRACE -> "RBRACE"
    | RBRACKET -> "RBRACKET"
    | REC -> "REC"
    | RPAREN -> "RPAREN"
    | SEMI -> "SEMI"
    | SEMISEMI -> "SEMISEMI"
    | SHARP -> "SHARP"
    | SIG -> "SIG"
    | STAR -> "STAR"
    | STRUCT -> "STRUCT"
    | THEN -> "THEN"
    | TILDE -> "TILDE"
    | TO -> "TO"
    | TRUE -> "TRUE"
    | TRY -> "TRY"
    | TYPE -> "TYPE"
    | UIDENT -> "UIDENT"
    | UNDERSCORE -> "UNDERSCORE"
    | VAL -> "VAL"
    | VIRTUAL -> "VIRTUAL"
    | WHEN -> "WHEN"
    | WHILE -> "WHILE"
    | WITH -> "WITH"

    | EOL -> "EOL"
    | EOF -> "EOF"

    | COMMENT_BEGIN -> "COMMENT_BEGIN"
    | COMMENT_INSIDE -> "COMMENT_INSIDE"
    | COMMENT_END -> "COMMENT_END"

    | STRING_BEGIN -> "STRING_BEGIN"
    | STRING_INSIDE -> "STRING_INSIDE"
    | STRING_INDENTED -> "STRING_INDENTED"
    | STRING_END -> "STRING_END"

    | QUOTATION_BEGIN -> "QUOTATION_BEGIN"
    | QUOTATION_INSIDE -> "QUOTATION_INSIDE"
    | QUOTATION_END -> "QUOTATION_END"

    | ILLEGAL_CHAR -> "ILLEGAL_CHAR"
    | LINENUM -> "LINENUM"

type tokenizer =
| CommentTokenizer
| StringTokenizer
| QuotationTokenizer

let token_end = ref 0
let buffered_token = ref None
let tokenizers = ref []
let string_indented = ref false

let string_inside ts indented =
  match ts with
  | StringTokenizer :: CommentTokenizer :: _ -> COMMENT_INSIDE
  | [ StringTokenizer ] ->
    if indented then STRING_INDENTED else STRING_INSIDE
  | _ -> assert false

(* The table of keywords *)

let create_hashtable n list =
  let t = Hashtbl.create n in
  List.iter (fun (x,y) -> Hashtbl.add t x y) list;
  t

let keyword_table =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3;
    "land", INFIXOP3;
    "lor", INFIXOP3;
    "lxor", INFIXOP3;
    "lsl", INFIXOP4;
    "lsr", INFIXOP4;
    "asr", INFIXOP4;
]

let newline = ref true
let line_num = ref 0
let spaces = ref [||]
let tabs = ref [||]
let eols = ref [||]

let array_resize old_array new_len default =
  let new_array = Array.create new_len default in
  Array.blit old_array 0 new_array 0 (Array.length old_array);
  new_array

let resize_arrays line_num =
  let max_nlines = Array.length !tabs in
  if max_nlines  <= line_num then begin
    let new_len = max_nlines * 2 + 1000 in
    tabs := array_resize !tabs new_len 0;
    spaces := array_resize !spaces new_len 0;
    eols := array_resize !eols new_len 0;
  end;
  ()

let set_indent indent line_num =
  let ntabs = ref 0 in
  let nspaces = ref 0 in
  for i = 0 to String.length indent - 1 do
    match indent.[i] with
      ' ' -> incr nspaces
    | '\t' -> incr ntabs
    | '\r' -> ()
    | _ -> assert false
  done;
  resize_arrays line_num;
  (!tabs).(line_num) <- !ntabs;
  (!spaces).(line_num) <- !nspaces;
  ()

let set_eol eol line_num =
  resize_arrays line_num;
  (!eols).(line_num) <- eol


let error lexbuf msg =
  Printf.eprintf "Error in %S with lexeme %S\n%!" msg (Lexing.lexeme lexbuf);
  List.iter (fun t ->
    Printf.eprintf "\t%s\n%!"
      (match t with
        CommentTokenizer -> "in comment"
      | StringTokenizer -> " in string"
      | QuotationTokenizer -> " in quotation"
      )
  ) !tokenizers;
  exit 2

}

let newline = '\010'
let blank = [' ' '\009' ]
let useless = [ '\013' '\012' ]
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' '\'']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222' '`']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
    let hex_literal =
      '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
        let oct_literal =
          '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
            let bin_literal =
              '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
                let int_literal =
                  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
    ('.' ['0'-'9' '_']* )?
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?


    rule token = parse
      | newline
          {
            set_eol (Lexing.lexeme_end lexbuf) !line_num;
            EOL }
      | blank +
          { if !newline then
              set_indent (Lexing.lexeme lexbuf) !line_num;
            token lexbuf }
      | useless + { token lexbuf }
      | "_"
          { UNDERSCORE }
      | "~"
          { TILDE }
      | "~" lowercase identchar * ':'
          { LABEL }
      | "?"  { QUESTION }
      | "??" { QUESTIONQUESTION }
      | "?" lowercase identchar * ':'
          { OPTLABEL }
      | lowercase identchar *
          { let s = Lexing.lexeme lexbuf in
            try
              Hashtbl.find keyword_table s
            with Not_found ->
              LIDENT }
      | uppercase identchar *
          { UIDENT }      (* No capitalized keywords *)
      | int_literal
          { INT }
      | float_literal
          { FLOAT }
      | int_literal "l"
          { INT32 }
      | int_literal "L"
          { INT64 }
      | int_literal "n"
          { NATIVEINT }
      | "\"" {
        tokenizers := StringTokenizer :: !tokenizers;
        string_indented := false;
        STRING_BEGIN }
      | "'" [^ '\\' '\'' '\010' '\013'] "'"
          { CHAR }
      | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
          { CHAR }
      | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
          { CHAR }
      | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
          { CHAR }
      | "'\\" _
          { CHAR }
      | "(*" {
        tokenizers := CommentTokenizer :: !tokenizers;
        COMMENT_BEGIN }
      | "<:" identchar * "<" {
        tokenizers := QuotationTokenizer :: !tokenizers;
        QUOTATION_BEGIN }

      | "#" [' ' '\t']* (['0'-'9']+) [' ' '\t']*
          ("\"" ([^ '\010' '\013' '"' ] *) "\"")?
          [^ '\010'] * newline
          {
            set_eol (Lexing.lexeme_end lexbuf) !line_num;
            buffered_token := Some EOL;
            LINENUM
          }
      | "#"  { SHARP }
      | "&"  { AMPERSAND }
      | "&&" { AMPERAMPER }
      | "`"  { BACKQUOTE }
      | "'"  { QUOTE }
      | "("  { LPAREN }
      | ")"  { RPAREN }
      | "*"  { STAR }
      | ","  { COMMA }
      | "->" { MINUSGREATER }
      | "."  { DOT }
      | ".." { DOTDOT }
      | ":"  { COLON }
      | "::" { COLONCOLON }
      | ":=" { COLONEQUAL }
      | ":>" { COLONGREATER }
      | ";"  { SEMI }
      | ";;" { SEMISEMI }
      | "<"  { LESS }
      | "<-" { LESSMINUS }
      | "="  { EQUAL }
      | "["  { LBRACKET }
      | "[|" { LBRACKETBAR }
      | "[<" { LBRACKETLESS }
      | "[>" { LBRACKETGREATER }
      | "]"  { RBRACKET }
      | "{"  { LBRACE }
      | "{<" { LBRACELESS }
      | "|"  { BAR }
      | "||" { BARBAR }
      | "|]" { BARRBRACKET }
      | ">"  { GREATER }
      | ">]" { GREATERRBRACKET }
      | "}"  { RBRACE }
      | ">}" { GREATERRBRACE }
      | "!"  { BANG }

      | "!=" { INFIXOP0 }
      | "+"  { PLUS }
      | "+." { PLUSDOT }
      | "-"  { MINUS }
      | "-." { MINUSDOT }

      | "!" symbolchar +
          { PREFIXOP }
      | ['~' '?'] symbolchar +
          { PREFIXOP }
      | ['=' '<' '>' '|' '&' '$'] symbolchar *
          { INFIXOP0 }
      | ['@' '^'] symbolchar *
          { INFIXOP1 }
      | ['+' '-'] symbolchar *
          { INFIXOP2 }
      | "**" symbolchar *
          { INFIXOP4 }
      | ['*' '/' '%'] symbolchar *
          { INFIXOP3 }

      | eof { EOF }
      | _
          { ILLEGAL_CHAR }

    and comment = parse
    "(*"
      { tokenizers := CommentTokenizer :: !tokenizers;
        comment lexbuf
      }
      | "*)"
          { match !tokenizers with
            | [] -> assert false
            | [ CommentTokenizer ] ->
              tokenizers := [];
              COMMENT_END
            | CommentTokenizer :: ts ->
              tokenizers := ts;
              token_end := Lexing.lexeme_end lexbuf;
              comment lexbuf
            | _ :: _ -> assert false
          }
      | "\""
          {
            token_end := Lexing.lexeme_end lexbuf;
            tokenizers := StringTokenizer :: !tokenizers;
            string_indented := false;
            string lexbuf }
      | "''"
          {
            token_end := Lexing.lexeme_end lexbuf;
            comment lexbuf }

      | "'" [^ '\\' '\'' '\010' '\013' ] "'"
          {
            token_end := Lexing.lexeme_end lexbuf;
            comment lexbuf }
      | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
          {
            token_end := Lexing.lexeme_end lexbuf;
            comment lexbuf }
      | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
          {
            token_end := Lexing.lexeme_end lexbuf;
            comment lexbuf }
      | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
          {
            token_end := Lexing.lexeme_end lexbuf;
            comment lexbuf }
      | eof
          {
            tokenizers := [];
            COMMENT_INSIDE
          }
      | useless + {
        token_end := Lexing.lexeme_end lexbuf;
        comment lexbuf }
      | newline    {
        set_eol (Lexing.lexeme_end lexbuf) !line_num;
        buffered_token := Some EOL; COMMENT_INSIDE }
      | [ ^ '\'' '\\' '\010' '\013' '(' '*' ]+
          {
            token_end := Lexing.lexeme_end lexbuf;
            comment lexbuf }
      | [ '\'' '\\' '(' '*' ]
          {
            token_end := Lexing.lexeme_end lexbuf;
            comment lexbuf
          }
      | _ { error lexbuf "comment" }

    and string = parse
    '"'
      {
        match !tokenizers with
          [] -> assert false
        | [ StringTokenizer ] ->
          tokenizers := [];
          STRING_END
        | StringTokenizer :: ( (CommentTokenizer :: _) as ts) ->
          tokenizers := ts;
          token_end := Lexing.lexeme_end lexbuf;
          comment lexbuf
        | _ -> assert false
      }
      | useless + {
        token_end := Lexing.lexeme_end lexbuf;
        string lexbuf }
      | [ ^ '\"' '\\' '\010' '\013' ]+
          {
            token_end := Lexing.lexeme_end lexbuf;
            string lexbuf }

      | '\\' ([ '\013' ]* as return) newline ([' ' '\t'] * as indent)
          {
            let with_indent = !string_indented in
            string_indented := true;
            set_eol (Lexing.lexeme_start lexbuf +
                       String.length return + 1
            ) !line_num;
            buffered_token := Some EOL;
            set_indent indent (!line_num+1);
            string_inside !tokenizers with_indent
          }
      | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
          { token_end := Lexing.lexeme_end lexbuf;
            string lexbuf }
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
          { token_end := Lexing.lexeme_end lexbuf;
            string lexbuf }
      | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
          { token_end := Lexing.lexeme_end lexbuf;
            string lexbuf }
      | '\\' _
          { token_end := Lexing.lexeme_end lexbuf;
            string lexbuf }
      | newline
          {
            let with_indent = !string_indented in
            string_indented := false;
            set_eol (Lexing.lexeme_end lexbuf) !line_num;
            buffered_token := Some EOL; string_inside !tokenizers with_indent
          }
      | eof
          { let ts = !tokenizers in
            tokenizers := []; string_inside ts !string_indented }

      | _ { error lexbuf "string" }

          {

          let quotation _lexbuf = EOF

          let line_begin = ref 0

          let init () =
            newline := true;
            line_num := 0;
            tabs := [||];
            spaces := [||];
            tokenizers := [];
            line_begin := 0

          let update_line_begin lexbuf tok =
            match tok with
            | EOL ->
              incr line_num;
              newline := true
            | EOF ->
              line_begin := Lexing.lexeme_start lexbuf;
              set_eol !line_begin !line_num
            | _ ->
              newline := false

          type tok = {
            token : token;
            tok_line : int;
            tok_begin : int;
            tok_end : int;
            tok_indent : int;
          }

          let token lexbuf =
            match !buffered_token with
              Some token ->
                buffered_token := None;
                let tok_begin = Lexing.lexeme_start lexbuf in
                let tok_end = Lexing.lexeme_end lexbuf in
                let tok_indent = tok_begin - !line_begin in
                let tok_line = !line_num in
                update_line_begin lexbuf token;
                { token; tok_line; tok_begin; tok_end; tok_indent }

            | None ->
              match !tokenizers with
              | [] ->
                let token = token lexbuf in
                let tok_begin = Lexing.lexeme_start lexbuf in
                let tok_end = Lexing.lexeme_end lexbuf in
                let tok_indent = tok_begin - !line_begin in
                let tok_line = !line_num in
                update_line_begin lexbuf token;
                { token; tok_line; tok_begin; tok_end; tok_indent }

              | CommentTokenizer :: _ ->
                let tok_begin = Lexing.lexeme_end lexbuf in
                let tok_indent = tok_begin - !line_begin in
                let token = comment lexbuf in
                let tok_end = !token_end in
                let tok_line = !line_num in
                update_line_begin lexbuf token;
                { token; tok_line; tok_begin; tok_end; tok_indent }

              | StringTokenizer :: _ ->
                let tok_begin = Lexing.lexeme_end lexbuf in
                let tok_indent = tok_begin - !line_begin in
                let token = string lexbuf in
                let tok_end = !token_end in
                let tok_line = !line_num in
                update_line_begin lexbuf token;
                { token; tok_line; tok_begin; tok_end; tok_indent }

              | QuotationTokenizer :: _ ->
                let tok_begin = Lexing.lexeme_end lexbuf in
                let tok_indent = tok_begin - !line_begin in
                let token = quotation lexbuf in
                let tok_end = !token_end in
                let tok_line = !line_num in
                update_line_begin lexbuf token;
                { token; tok_line; tok_begin; tok_end; tok_indent }

          let string_of_tok tok =
            Printf.sprintf "{ %s at %d-%d indent=%d }"
              (string_of_token tok.token)
              tok.tok_begin tok.tok_end
              tok.tok_indent

          let indent line =
            (!spaces).(line), (!tabs).(line)

          type line = {
            nspaces : int;
            ntabs : int;
            bol : int;
            eol : int;
          }

          let lines () =
            let lines = Array.create (!line_num+1) {
              nspaces = 0;
              ntabs = 0;
              bol = 0;
              eol = 0;
            } in
            for i = 0 to !line_num do
              let line_pos =
                if i = 0 then 0 else (!eols).(i-1) in
              let nspaces = (!spaces).(i) in
              let ntabs = (!tabs).(i) in
              let bol = line_pos + nspaces + ntabs in
              let eol = (!eols).(i) in
              lines.(i) <- { nspaces; ntabs; bol; eol }
            done;
            lines

}
