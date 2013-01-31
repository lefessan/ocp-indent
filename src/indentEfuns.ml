open IndentLexer

let indentation = ref 2

let print_indentations list =
  print_string "Indentations :"; print_newline ();
  List.iter (fun (indent, list) ->
      List.iter (fun pos ->
          Printf.printf "Line at %d with %d" pos indent
      ) list
  ) list;
  print_newline ()

let print_stack stack =
  print_string "Indentation stack:"; print_newline ();
  let rec iter stack =
    match stack with
      [] -> ()
    | (token, indent) :: stack ->
        Printf.printf "Token %s indent %d"
          (string_of_token token) indent;
        print_newline ();
        iter stack
  in
  iter stack

(*
let rec pop_to_quote stack =
  match stack with
    [] -> ([],0)
  | (BEGINQ,indent) :: _ -> stack, indent+ !indentation
  | _ :: stack -> pop_to_quote stack
*)

let rec pop_to_top stack =
  match stack with
    [] -> ([],0)
  | (STRUCT,indent) :: _ -> stack, indent+ !indentation
(*  | (BEGINQ,indent) :: _ -> stack, indent+ !indentation *)
  | (SIG,indent) :: _ -> stack, indent+ !indentation
  | (LBRACE,indent) :: _ -> stack, indent+ !indentation
  | (OBJECT,indent) :: _ -> stack, indent+ !indentation
  | _ :: stack -> pop_to_top stack

let rec pop_to kwd stack =
  match stack with
    [] -> ([],0)
  | (kwd',indent) :: stack when kwd' = kwd -> stack, indent
  | _ :: stack -> pop_to kwd stack

let rec pop_to_kwds kwds stack =
  match stack with
    [] -> ([],SEMISEMI, 0)
  | (kwd,indent) :: stack when List.memq kwd kwds ->
      stack, kwd, indent
  | _ :: stack -> pop_to_kwds kwds stack

let fix indent eols indents =
  match eols with
    [] -> indents
  | _ ->
      match indents with
        (pindent,peols) :: tail when pindent = indent ->
          (indent, eols @ peols) :: tail
      | _ ->  (indent,eols) :: indents

let rec pop_indentation indents =
  match indents with
    [] -> raise Not_found
  | (indent, eols) :: indents ->
      match eols with
        [] -> pop_indentation indents
      | eol :: eols ->
          (indent, eol, (indent,eols) :: indents)

let token_offset prev_tok =
  match prev_tok with

  | CHAR | GREATERRBRACE | GREATERRBRACKET | FALSE | FLOAT | INFIXOP0
  | INFIXOP1 | INFIXOP2 | INFIXOP3 | INFIXOP4 | INT | LESS | LESSMINUS
  | LIDENT | DONE | END | BARRBRACKET | UIDENT | UNDERSCORE | STRING_BEGIN
  | STRING_INSIDE | STRING_END
  | PREFIXOP | QUESTION | QUOTE | RBRACE  | RBRACKET (* | RULE _ | PARSE *)
    ->  !indentation

  | AMPERAMPER | AMPERSAND | AND | AS | ASSERT | BAR | BARBAR | BEGIN
  | CLASS | COLON | COLONCOLON | COLONEQUAL | COLONGREATER | COMMA
  |   CONSTRAINT | DO | DOT | DOTDOT | DOWNTO | ELSE | EQUAL | EXCEPTION
  | EXTERNAL | FOR | FUN | FUNCTION | FUNCTOR | GREATER | IF | IN
  | INCLUDE | INHERIT | INITIALIZER | LAZY | LBRACE | LBRACELESS
  | LBRACKET | LBRACKETBAR | LBRACKETLESS | LET | LPAREN | MATCH
  | METHOD | MINUSGREATER | MODULE | MUTABLE | NEW | OBJECT | OF | OPEN
  | OR (* | PARSER *) | PRIVATE
  | REC | RPAREN | SEMI | SEMISEMI
  | SHARP | SIG | STAR | STRUCT (* | SUBTRACTIVE *) | THEN | TO | TRUE | TRY
  | TYPE | VAL (* | VALUE *) | VIRTUAL | WHEN | WHILE | WITH
    -> 0

  | _ -> 0

let rec parse lexbuf prev_tok stack eols indent indents =
  let tok = IndentLexer.token lexbuf in
  let pos = tok.tok_indent in
  let token = tok.token in
  match token with
    EOL  -> parse lexbuf prev_tok stack (pos::eols) indent indents
  | EOF  -> fix indent  (pos :: eols) indents
  | STRING_BEGIN -> (0,[0]) :: (fix indent eols indents)
  | COMMENT_BEGIN -> ( !indentation,[0]) :: (fix 0 eols indents)
  | COMMENT_END -> parse lexbuf prev_tok stack [] indent (fix 0 eols indents)
  | LET ->
      (*
  indentation des LETs: Il faut savoir s'il s'agit d'un LET avec ou sans IN.
   Pour cela, on regarde simplement le token precedent.
   Voici ceux qui ne peuvent pas introduire un TOP-LET
*)
      begin
        match prev_tok with
          IN | THEN | COLONCOLON | INFIXOP0 | INFIXOP0 | INFIXOP1 |
          INFIXOP2 | INFIXOP3 | INFIXOP4 (* | SUBTRACTIVE *)
        | STAR | EQUAL | LESS |
          GREATER | OR | BARBAR | AMPERAMPER | AMPERSAND | COLONEQUAL |
          LESSMINUS | LPAREN | LBRACKET | LBRACKETBAR | MATCH | TRY |
          WHILE | DO | TO | DOWNTO | BEGIN | MINUSGREATER | WHEN | COMMA |
          SEMI | QUESTION | QUOTE | BAR | IF (* | BEGINQ *) ->
            (* On reste dans le bloc precedent, donc avec la meme indentation *)
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
            (fix indent eols indents)

        | ELSE ->
            (* On reste dans le bloc precedent, mais avec une indentation plus
        petite car on est sorti du IF THEN ELSE *)
            parse lexbuf token ((token,indent- !indentation) :: stack) [] indent
              (fix (indent- !indentation) eols indents)

        | _ ->
            (* On est dans un nouveau LET toplevel *)
            let (stack, indent) = pop_to_top stack in
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
            (fix indent eols indents)

      end

  | VAL (* | VALUE *) | EXTERNAL | TYPE | EXCEPTION | OPEN
  | INCLUDE | CLASS (* | RULE *) | METHOD | INITIALIZER | VIRTUAL ->
      (* On est dans une pharse toplevel *)
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
      (fix indent eols indents)

  | SEMISEMI ->
      let (stack,indent) = pop_to_top stack in
      parse lexbuf token stack [] indent (fix indent eols indents)

  | MODULE ->
      if prev_tok = LET then
        (* LET MODULE *)
        parse lexbuf token stack [] indent (fix indent eols indents)
      else
        (* On est dans une pharse toplevel *)
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
      (fix indent eols indents)

  | EQUAL ->
      let (stack',kwd,indent') = pop_to_kwds [ (*DEF; *) BAR] stack in
      (* if we find a DEF, we are the first = after the DEF, ie a process
      follows. We put a BAR to prevent any other EQUAL to match this DEF.
  Other EQUALs should not be affected by this JoCaml need. *)

(*      if kwd = DEF then
        parse lexbuf token
          ((BAR,indent)::stack) [] indent (fix indent eols indents)
      else *)
        parse lexbuf token stack [] indent (fix indent eols indents)

  | AND ->
      let (stack,kwd,indent) = pop_to_kwds
          [LET;TYPE (*;RULE *);CLASS(* ;DEF ;LOC *)] stack in
      parse lexbuf token ((kwd,indent)::stack)
      [] (indent+ !indentation) (fix indent eols indents)
(*  | OR ->
      let (stack',kwd,indent') = pop_to_kwds  [DEF] stack in
      if kwd = DEF then
        parse lexbuf token stack
          [] (indent'+ !indentation) (fix indent' eols indents)
      else
        parse lexbuf token stack
          [] indent (fix indent eols indents)
*)
  | IN ->
      (* partially terminate a LET structure *)
      let (stack,indent) = pop_to LET stack in
      parse lexbuf token ((IN,indent)::stack)
      [] indent (fix indent eols indents)

(*  | DEF
  | LOC ->
      parse lexbuf token ((token,indent)::stack)
      [] (indent+ !indentation) (fix indent eols indents)
*)
  | DO ->
(* starts a DO ... DONE structure *)
      let (stack',kwd,indent') = pop_to_kwds [WHILE;FOR(* ;LOC *)] stack in
(*      if kwd = LOC then begin
      (* LOC ... DO { ... } *)
        parse lexbuf DO stack [] (indent'+ !indentation)
        (fix indent' eols indents)
      end else *)
      if kwd = SEMISEMI then begin
        parse lexbuf token stack
        [] indent (fix indent eols indents)
        end else begin
          parse lexbuf DO ((DO,indent') :: stack') [] (indent'+ !indentation)
          (fix indent' eols indents)
        end
(* These keywords start multi-keywords block structures. *)

(* This symbol has different meanings in lexer files *)
  | LBRACE          (* LBRACE ... RBRACE *)
    ->
      if prev_tok = RBRACE &&
        (match stack with
            (BAR,_) :: _
(*          | (PARSE,_) :: _ *)
            -> true
          | _ -> false) then
        parse lexbuf SEMISEMI [] [] 0
          (fix 0 eols indents)
      else
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
      (fix (indent+offset) eols indents)

(* Terminated structures *)
  | LPAREN          (* LPAREN ... RPAREN *)
  | LBRACELESS      (* LBRACELESS ... GREATERRBRACE  *)
  | LBRACKET        (* LBRACKET ... RBRACKET  *)
  | LBRACKETBAR     (* LBRACKETBAR ... BARRBRACKET *)
  | LBRACKETLESS    (* LBRACKETLESS ... GREATERRBRACKET *)
  | BEGIN           (* BEGIN ... END  *)
(*  | BEGINQ *)
    ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
      (fix (indent+offset) eols indents)
  | COLON
    ->
      let offset = token_offset prev_tok in
      parse lexbuf token ((token,indent) :: stack) [] indent
        (fix (indent+offset) eols indents)

  | STRUCT          (* STRUCT ... END *)
  | SIG             (* SIG ... END *)
  | FOR             (* FOR ... TO/DOWNTO... DO  ... DONE  *)
  | WHILE           (* WHILE ... DO ... DONE  *)
  | OBJECT

(* Non-terminated structures *)
  | MATCH           (*  MATCH ... WITH ...  *)
  | TRY             (*  TRY ... WITH ...  *)
  | FUNCTION        (*  FUNCTION ... MINUSGREATER ...  *)
  | FUN             (*  FUN ... MINUSGREATER ...  *)
(*  | PARSER          (*  PARSER ... MINUSGREATER ...  *) *)
  | IF              (*  TRY ... WITH ...  *)
    ->
      begin
        match prev_tok with
          ELSE ->
            (* On reste dans le bloc precedent, mais avec une indentation plus
            petite car on est sorti du IF THEN ELSE *)
            parse lexbuf token ((token,indent- !indentation) :: stack) [] indent
              (fix (indent- !indentation) eols indents)

        | _ ->
            parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
            (fix indent eols indents)
      end

(* Deterministic Terminators *)
  | RPAREN
  | RBRACE
  | GREATERRBRACE
  | RBRACKET
  | BARRBRACKET
  | GREATERRBRACKET ->
      (* find corresponding block delimiter *)
      let kwd = List.assoc token
          [
          RPAREN, LPAREN;
          RBRACE, LBRACE;
          RBRACKET,LBRACKET;
          BARRBRACKET, LBRACKETBAR;
          GREATERRBRACE, LBRACELESS;
          GREATERRBRACKET, LBRACKETLESS
        ]
      in
      let (stack,indent) = pop_to kwd stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
      (* Non-deterministic terminators *)
  | END
  | DONE
(*  | ENDQ *)
    ->
      let kwds = List.assoc token
          [
          END,[BEGIN;STRUCT;SIG (*; EXTEND *)];
          DONE, [FOR;WHILE;DO;TO;DOWNTO];
         (* ENDQ, [BEGINQ]; *)
        ]
      in
      let (stack,kwd, indent) = pop_to_kwds kwds stack in
      parse lexbuf token stack [] indent (fix indent eols indents)
  | WITH ->
      let (stack,kwd,indent) = pop_to_kwds [MATCH;TRY;LBRACE] stack in
      if kwd = LBRACE then
        parse lexbuf token ((LBRACE,indent)::stack) [] (indent+ !indentation)
        (fix indent eols indents)
      else
        parse lexbuf token ((WITH,indent)::stack) [] (indent+ !indentation)
        (fix indent eols indents)
  | BAR ->
      let (stack,kwd,indent) =
        pop_to_kwds [WITH;FUNCTION;BAR;TYPE(* ;PARSE*);LPAREN;LBRACE(*;DEF *);
          LBRACKET]
          stack in
      let kwd =
        match kwd with
          TYPE | LPAREN (*| PARSE*) | LBRACE (*| DEF*) | LBRACKET -> kwd
        | _ -> BAR
      in
      parse lexbuf token ((kwd,indent)::stack) [] (indent+ !indentation)
      (fix indent eols indents)
  | MINUSGREATER ->
      let (stack,kwd,indent) =
        pop_to_kwds [WITH;FUN;BAR;FUNCTION;TYPE;LPAREN;EXTERNAL;VAL;COLON;LBRACKET] stack in
      begin
        match kwd with
          TYPE | LPAREN | EXTERNAL | VAL | COLON ->
            let offset = token_offset prev_tok in
            parse lexbuf token ((kwd,indent)::stack) [] indent
              (fix (indent+offset) eols indents)
        | LBRACKET ->
            parse lexbuf token ((LBRACKET,indent):: (BAR, indent):: stack) []
              (if kwd = FUN then indent+ !indentation else indent+ 2 * !indentation)
            (fix (indent+ !indentation) eols indents)
        | _ ->
            parse lexbuf token ((BAR,indent)::stack) []
              (if kwd = FUN then indent+ !indentation else indent+ 2 * !indentation)
            (fix (indent+ !indentation) eols indents)
      end
  | THEN ->
      let (stack,indent) = pop_to IF stack in
      parse lexbuf token ((THEN,indent)::stack) [] (indent+ !indentation)
      (fix indent eols indents)
  | ELSE ->
      let (stack,indent) = pop_to THEN stack in
      parse lexbuf token ((ELSE,indent)::stack) [] (indent+ !indentation)
      (fix indent eols indents)
  | SEMI ->
      let _old_stack = stack in
(* le ; termine un THEN ... ou ELSE ... s'il n'y a pas
   construction infinie (LET, IN, MATCH, BAR) avant *)
      let (stack1,_,indent1) = pop_to_kwds [THEN;ELSE(*;VALUE*)] stack in
      let (stack2,_,_) = pop_to_kwds
          [
          LET; IN; COLONCOLON; INFIXOP0; INFIXOP0; INFIXOP1; INFIXOP2;
          INFIXOP3; INFIXOP4(*; SUBTRACTIVE*); STAR; EQUAL; LESS; GREATER;
          OR; BARBAR; AMPERAMPER; AMPERSAND; COLONEQUAL; LESSMINUS;
          LPAREN; LBRACKET; LBRACKETBAR; MATCH; TRY; IF; WHILE; DO; TO;
          DOWNTO; BEGIN; MINUSGREATER; WHEN; COMMA; SEMI; QUESTION;
          QUOTE; BAR; LBRACE;(* EXTEND*)
        ]
          stack in
      let new_stack, new_indent =
        if List.length stack1 > List.length stack2 then
(* le THEN ou ELSE est en premier *)
          stack1, indent1
        else
        (* on continue tout simplement *)
          stack, indent
      in
      parse lexbuf token new_stack [] new_indent
        (fix indent eols indents)

(*  | PARSE           (* RULE ... PARSE ... *)
    ->
      begin
        match stack with
          (RULE,_) :: _ ->
            parse lexbuf token ((token,indent) :: stack) [] indent
              (fix indent eols indents)
        | _ ->
            let offset = token_offset prev_tok in
            parse lexbuf token stack [] indent
              (fix (indent+offset) eols indents)
      end *)
  | _ ->

      let offset = token_offset prev_tok in
      parse lexbuf token stack [] indent
        (fix (indent+offset) eols indents)

let get_indentations pos lexbuf =
  parse lexbuf SEMISEMI [] [] 0 []

(*
let print_exc e s =
  Printf.printf "Caught exception %s in %s" (Printexc.to_string e) s;
  print_newline ()

(* Now, use the indentation from the parser *)

let compute_indentations buf start_point end_point =
  let text = buf.buf_text in
  let curseur = Text.dup_point text start_point in
(* init indentation *)
  let _pos = get_position text end_point in
  let lexbuf = lexing text curseur end_point in
  try
    let indentations =
      get_indentations (get_position text start_point) lexbuf in
    remove_point text curseur;
    indentations
  with
    e ->
      remove_point text curseur;
      raise e

let find_phrase_start buf curseur =
  let text = buf.buf_text in
  try
    let _ = Text.search_backward text (snd !!start_regexp) curseur in ()
  with
    Not_found -> Text.set_position text curseur 0

let indent_between_points buf start_point end_point =
  let text = buf.buf_text in
  let session = start_session text in
  let curseur = dup_point text start_point in
  try
    find_phrase_start buf curseur;
    let indentations = compute_indentations buf curseur end_point in
(* remove the Eof indentation *)
    let _,_,indentations = pop_indentation indentations in
(* indent other lines *)
    let rec iter indents =
      let (current,pos,indents) = pop_indentation indents in
      set_position text curseur (pos+1);
      set_indent text curseur current;
      iter indents
    in
    iter indentations
  with
    e ->
      commit_session text session;
      remove_point text curseur

(* Interactive: indent all lines of the current block *)
let indent_phrase frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  indent_between_points buf point point

let indent_region frame =
  let buf = frame.frm_buffer in
  let point = frame.frm_point in
  let mark = Ebuffer.get_mark buf point in
  let (start_point,end_point) =
    if point < mark then (point,mark) else (mark,point)
  in
  indent_between_points buf start_point end_point


let indent_buffer frame =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let start_point = add_point text in
  let end_point = add_point text in
  set_position text end_point (Text.size text);
  indent_between_points buf start_point end_point;
  remove_point text start_point;
  remove_point text end_point
*)

let string_of_channel ic =
  let s = String.create 32768 in
  let b = Buffer.create 1000 in
  let rec iter ic b s =
    let nread = input ic s 0 32768 in
    if nread > 0 then begin
      Buffer.add_substring b s 0 nread;
      iter ic b s
    end
  in
  iter ic b s;
  Buffer.contents b

let tokenize_file filename =
  let ic = open_in filename in
  let s = string_of_channel ic in
  close_in ic;
  let lexbuf = Lexing.from_string s in
  try
  while true do
    let tok = token lexbuf in
    match tok.token with
      EOF -> raise Exit
    | _ -> ()
  done
  with Exit ->
    close_in ic;
    let lines = IndentLexer.lines () in
    Printf.printf "line:  spc tab bol   eol   content\n%!";
    Array.iteri (fun i line ->
      Printf.printf "%4d: [%2d][%2d][%4d][%4d]%!"
        i line.nspaces line.ntabs line.bol line.eol;
      let len = line.eol - line.bol - 1 in
      if len <> -1 then
        Printf.printf "%S\n%!"
        (String.sub s line.bol len)
      else
        Printf.printf "EOL\n%!"
    ) lines

