open IndentLexer

let debug = try
              Sys.getenv "INDENT_EFUNS" = "d"
  with Not_found -> false

let _ =
  if debug then
    Printf.eprintf "ocp-indent: debug efuns mode\n%!"

let token lexbuf =
  let tok = token lexbuf in
  if debug then
    Printf.eprintf "%s\n%!" (string_of_tok tok);
  tok

let indentation = ref 2

(*
let print_indentations list =
  print_string "Indentations :"; print_newline ();
  List.iter (fun (indent, list) ->
      List.iter (fun pos ->
          Printf.printf "Line at %d with %d" pos indent
      ) list
  ) list;
  print_newline ()
*)

(*
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
*)

(*
let rec pop_to_quote stack =
  match stack with
    [] -> ([],0)
  | (BEGINQ,indent) :: _ -> stack, indent+ !indentation
  | _ :: stack -> pop_to_quote stack
*)

type node_precedence =
| PRE_VALUE

(* For a given position in the stack (the path to the current expression),
 identify the kind of node that we have found before. *)
type node_kind =
  NSTRUCT
| NSIG
| NOBJECT
| NLET  (* used by LET, CLASS and METHOD *)
| NLETIN
| KSTRING
| NEXPR of node_precedence
| NTOP
| NPAREN
| NBRACE
| NBEGIN
| NINCLUDE
| NEXCEPTION
| NVAL
| NMATCH
| NTRY
| NWITH
| NBRACKET
| NOPEN
| NFUN
| NBIND (* the EQUAL after the LET, and INITIALIZER *)
| NFOR
| NWHILE
| NDO

type indenter_state = {
  path : (node_kind * int) list;
  prev : token;
  indent : int;
  line : int option;
  lines : (int * int) list;
}

let initial_state = {
  path = [];
  prev = SEMISEMI;
  indent = 0;
  line = None;
  lines = [];
}

let next_lines state =
  match state.line with
    None -> state.lines
  | Some line -> (line, state.indent) :: state.lines

let indent state more_indent =
  let indent = state.indent + more_indent in
  { state with
    indent;
    line = None;
    lines = match state.line with
      None -> state.lines
    | Some line -> (line, indent) :: state.lines
  }

let next_state state token =
  { state with
    prev = token;
    line = None;
    lines = next_lines state;
  }

let force_indent state indent =
  match state.line with
    None -> state
  | Some line ->
    { state with
      line = None;
      lines = (line, indent) :: state.lines
    }

let push state prev kind more_indent =
  { path = (kind, state.indent) :: state.path;
    prev;
    indent = state.indent + more_indent;
    line = None;
    lines = next_lines state;
  }

let replace state prev kind more_indent =
    match state.path with
      [] -> assert false
    | (_, indent) :: path ->
      let path = (kind, indent) :: path in
      { path;
        prev;
        indent = indent + more_indent;
        line = None;
        lines = next_lines state;
      }

let rec pop_to_top path =
  match path with
    [] -> ([], NTOP, 0)
  | ( (NSTRUCT
          | NSIG
          | NOBJECT) as kind, indent) :: _ ->
    path, kind, indent + !indentation
  | _ :: stack -> pop_to_top stack

let pop_to_top state =
  let (path, kind, indent) = pop_to_top state.path in
  { state with path; indent }, kind

let rec pop_to_kind state kind path =
  match path with
    [] -> (state.path, NTOP, state.indent)
  | (k, indent) :: path when k = kind ->
    path, kind, indent
  | _ :: path -> pop_to_kind state kind path

let pop_to_kind state kind =
  let (path, kind, indent) = pop_to_kind state kind state.path in
  { state with path; indent }, kind

let rec pop_to_fun state f path =
  match path with
    [] -> (state.path, NTOP, state.indent)
  | (kind, indent) :: path when f kind ->
    path, kind, indent
  | _ :: path -> pop_to_fun state f path

let pop_to_fun state f =
  let (path, kind, indent) = pop_to_fun state f state.path in
  { state with path; indent }, kind

let rec indenter lexbuf state =
  let tok = token lexbuf in
  let token = tok.token in
  match token with

  | EOF -> List.rev state.lines
  | EOL  ->
    let lines = match state.line with
        None -> state.lines
      | Some line -> (* empty lines are not indented ! *)
        (line, 0) :: state.lines
    in
    indenter lexbuf { state with line = Some (tok.tok_line+1); lines }

  | STRING_BEGIN ->
    indenter lexbuf (push state STRING_BEGIN KSTRING 0)
  | STRING_INSIDE ->
    indenter lexbuf (force_indent state 0)
  | STRING_END ->
    indenter lexbuf (replace state STRING_END (NEXPR PRE_VALUE) 0 )
  | STRING_INDENTED ->
    indenter lexbuf (next_state state STRING_BEGIN)

  | LINENUM ->
    indenter lexbuf (force_indent state 0)

  | INT64  | INT32  | INT  | LIDENT
  | FLOAT  | CHAR
  | TRUE | FALSE | NATIVEINT
  | UNDERSCORE | TILDE | QUESTION
  | QUOTE ->
    indenter lexbuf
      (match state.path with
        (NEXPR PRE_VALUE, _) :: _ ->
          next_state state token
      | _ ->
        push state token (NEXPR PRE_VALUE) !indentation
      )


  | SEMISEMI ->
    let state, _kind = pop_to_top state in
    indenter lexbuf (next_state state SEMISEMI)

  | OPEN ->
    begin
      match state.prev with
        LET ->
          indenter lexbuf (push state OPEN NOPEN 0)
      | _ ->
        let state, _kind = pop_to_top state in
        indenter lexbuf (push state OPEN NOPEN !indentation)
    end

  | INCLUDE ->
    let state, _kind = pop_to_top state in
    indenter lexbuf (push state INCLUDE NINCLUDE !indentation)

  | EXCEPTION ->
    let state, _kind = pop_to_top state in
    indenter lexbuf (push state EXCEPTION NEXCEPTION !indentation)

  | VAL ->
    let state, _kind = pop_to_top state in
    indenter lexbuf (push state VAL NVAL !indentation)

  | LET ->
    begin
      match state.prev with
        IN | THEN | COLONCOLON | INFIXOP0 | INFIXOP1
      | INFIXOP2 | INFIXOP3 | INFIXOP4
      | STAR | EQUAL | LESS | GREATER | OR | BARBAR | AMPERAMPER
      | AMPERSAND | COLONEQUAL
      | LESSMINUS | LPAREN | LBRACKET | LBRACKETBAR | MATCH | TRY
      | WHILE | DO | TO | DOWNTO | BEGIN | MINUSGREATER | WHEN | COMMA
      | SEMI | QUESTION | QUOTE | BAR | IF  ->
        (* A "let" ... "in" *)
        indenter lexbuf (push state LET NLETIN !indentation)

      | ELSE ->
        (* On reste dans le bloc precedent, mais avec une indentation plus
           petite car on est sorti du IF THEN ELSE *)
        indenter lexbuf (push (indent state (- !indentation) )
                           LET NLETIN !indentation)

      | _ ->
        (* On est dans un nouveau LET toplevel *)
        let state, _kind = pop_to_top state in
        indenter lexbuf (push state LET NLET !indentation)
    end

  | CLASS
  | METHOD ->
    let state, _kind = pop_to_top state in
    indenter lexbuf (push state token NLET !indentation)
  | INITIALIZER ->
    let state, _kind = pop_to_top state in
    indenter lexbuf (push state token NBIND !indentation)

  | LPAREN ->
    indenter lexbuf (push state LPAREN NPAREN !indentation)
  | RPAREN ->
    let state, _kind = pop_to_kind state NPAREN in
    indenter lexbuf (next_state state token)

  | LBRACE
  | LBRACELESS ->
    indenter lexbuf (push state LPAREN NPAREN !indentation)
  | RBRACE
  | GREATERRBRACE ->
    let state, _kind = pop_to_kind state NBRACE in
    indenter lexbuf (next_state state token)

  | LBRACKET
  | LBRACKETGREATER
  | LBRACKETLESS ->
    indenter lexbuf (push state LBRACKET NBRACKET !indentation)
  | BARRBRACKET
  | RBRACKET
  | GREATERRBRACKET ->
    let state, _kind = pop_to_kind state NBRACKET in
    indenter lexbuf (next_state state token)


  | SIG ->
    indenter lexbuf (push state SIG NSIG !indentation)
  | OBJECT ->
    indenter lexbuf (push state OBJECT NOBJECT !indentation)
  | STRUCT ->
    indenter lexbuf (push state STRUCT NSTRUCT !indentation)
  | BEGIN ->
    indenter lexbuf (push state BEGIN NBEGIN !indentation)
  | END ->
    let state, kind = pop_to_fun state
      (function
      | NBEGIN
      | NSTRUCT
      | NSIG
      | NOBJECT -> true
      | _ -> false
      ) in
    indenter lexbuf (
      match kind with
        NBEGIN ->
          push state END (NEXPR PRE_VALUE) !indentation
      | _ -> next_state state token)

  | COMMENT_BEGIN | COMMENT_INSIDE | COMMENT_END ->
    indenter lexbuf (force_indent state 0)

  | MATCH ->
    indenter lexbuf (push state MATCH NMATCH !indentation)
  | TRY ->
    indenter lexbuf (push state TRY NTRY !indentation)
  | WITH ->
    let state, _kind = pop_to_fun state
      (function
      | NMATCH
      | NTRY -> true
      | _ -> false
      ) in
    indenter lexbuf (
      push state WITH NWITH !indentation
    )
  | FUNCTION ->
    indenter lexbuf (push state FUNCTION NWITH !indentation)
  | FUN ->
    indenter lexbuf (push state FUN NFUN !indentation)

  | WHILE ->
    indenter lexbuf (push state WHILE NWHILE !indentation)
  | FOR ->
    indenter lexbuf (push state FOR NFOR !indentation)
  | DO ->
     let state, _kind = pop_to_fun state
      (function
      | NFOR
      | NWHILE -> true
      | _ -> false
      ) in
    indenter lexbuf (
      push state DO NDO !indentation
    )
  | DONE ->
    let state, _kind = pop_to_kind state NDO in
    indenter lexbuf (push state DONE (NEXPR PRE_VALUE) !indentation)

  | TO
  | DOWNTO ->
    let state, _kind = pop_to_kind state NFOR in
    indenter lexbuf (
      push state token NFOR !indentation
    )

  |VIRTUAL
  |REC
  |PRIVATE
  |FUNCTOR
  |DOTDOT
  |BACKQUOTE
  |ILLEGAL_CHAR ->
    indenter lexbuf (next_state state token)


  | AMPERAMPER|AMPERSAND|AND|AS
  | ASSERT|BANG|BAR|BARBAR
  | COLON|COLONCOLON|COLONEQUAL|COLONGREATER|COMMA|CONSTRAINT
  | DOT|ELSE|EQUAL|EXTERNAL
  |GREATER|IF|IN|INFIXOP0
  | INFIXOP1|INFIXOP2|INFIXOP3|INFIXOP4|INHERIT
  | LABEL|LAZY|LBRACKETBAR|LESS|LESSMINUS|MINUS|MINUSDOT
  | MINUSGREATER|MODULE|MUTABLE|NEW|OF|OPTLABEL|OR|PLUS|PLUSDOT
  | PREFIXOP|QUESTIONQUESTION|SEMI
  | SHARP|STAR|THEN|TYPE|UIDENT|WHEN
  |QUOTATION_BEGIN|QUOTATION_INSIDE|QUOTATION_END
   ->
    indenter lexbuf (next_state state token)

(*


let rec pop_to_top stack =
  match stack with
    [] -> ([],0)
  | (NSTRUCT,indent) :: _ -> stack, indent+ !indentation
(*  | (BEGINQ,indent) :: _ -> stack, indent+ !indentation *)
  | (NSIG,indent) :: _ -> stack, indent+ !indentation
  | (NLBRACE,indent) :: _ -> stack, indent+ !indentation
  | (NOBJECT,indent) :: _ -> stack, indent+ !indentation
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

(*
let rec pop_indentation indents =
  match indents with
    [] -> raise Not_found
  | (indent, eols) :: indents ->
      match eols with
        [] -> pop_indentation indents
      | eol :: eols ->
          (indent, eol, (indent,eols) :: indents)
*)

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

let rec pop_off_kwds kwds stack =
  match stack with
    [] -> ([], 0)
  | (kwd, indent) :: new_stack ->
    if List.mem kwd kwds then
      pop_off_kwds kwds new_stack
    else
      (stack, indent)

let rec parse lexbuf
    (prev_tok : token)
    (stack : (node_kind * int) list)
    (eols : int list) indent indents =
  let tok = token lexbuf in
(*  let pos = tok.tok_indent in *)
  let token = tok.token in
  match token with
    EOL  -> parse lexbuf prev_tok stack (
      (tok.tok_line+1) ::eols) indent indents
  | EOF  -> fix indent  eols indents
(*  | STRING_BEGIN -> (0,[0]) :: (fix indent eols indents)
  | COMMENT_BEGIN -> ( !indentation,[0]) :: (fix 0 eols indents)
  | COMMENT_END -> parse lexbuf prev_tok stack [] indent (fix 0 eols indents)
*)
  | LET ->
      (*
  indentation des LETs: Il faut savoir s'il s'agit d'un LET avec ou sans IN.
   Pour cela, on regarde simplement le token precedent.
   Voici ceux qui ne peuvent pas introduire un TOP-LET
*)
      begin
        match prev_tok with
          IN | THEN | COLONCOLON | INFIXOP0 | INFIXOP1 |
          INFIXOP2 | INFIXOP3 | INFIXOP4 (* | SUBTRACTIVE *)
        | STAR | EQUAL | LESS |
          GREATER | OR | BARBAR | AMPERAMPER | AMPERSAND | COLONEQUAL |
          LESSMINUS | LPAREN | LBRACKET | LBRACKETBAR | MATCH | TRY |
          WHILE | DO | TO | DOWNTO | BEGIN | MINUSGREATER | WHEN | COMMA |
          SEMI | QUESTION | QUOTE | BAR | IF (* | BEGINQ *) ->
            (* On reste dans le bloc precedent, donc avec la meme indentation *)
            parse lexbuf token
              ((NLET,indent) :: stack) [] (indent+ !indentation)
            (fix indent eols indents)

        | ELSE ->
            (* On reste dans le bloc precedent, mais avec une indentation plus
        petite car on est sorti du IF THEN ELSE *)
            parse lexbuf token ((NLET,indent- !indentation) :: stack) [] indent
              (fix (indent- !indentation) eols indents)

        | _ ->
            (* On est dans un nouveau LET toplevel *)
            let (stack, indent) = pop_to_top stack in
            parse lexbuf token ((NLET,indent) :: stack) [] (indent+ !indentation)
            (fix indent eols indents)

      end

  | VAL (* | VALUE *) | EXTERNAL | TYPE | EXCEPTION | OPEN
  | INCLUDE | CLASS (* | RULE *) | METHOD | INITIALIZER | VIRTUAL ->
      (* On est dans une pharse toplevel *)
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((token,indent) :: stack) [] (indent+ !indentation)
      (fix indent eols indents)

  | VAL -> parse_at_top lexbuf VAL NVAL stack eols indent indents

and parse_at_top lexbuf token kind stack eols indent indents =
      let (stack, indent) = pop_to_top stack in
      parse lexbuf token ((NVAL,indent) :: stack) [] (indent+ !indentation)
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

(* EQUAL can be used either as a binder after LET, or as an infix operator *)
  | EQUAL ->
(*    begin
      match stack with
        (LET, _) :: _ ->

    end
*)
      let (_stack',_kwd,_indent') = pop_to_kwds [ (*DEF; *) BAR] stack in
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
      let (stack,_kwd, indent) = pop_to_kwds kwds stack in
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

  | PLUS | MINUS ->
    let (stack, indent) = pop_off_kwds [STAR] stack in
    parse lexbuf token ( (PLUS, indent) :: stack) [] (indent + !indentation)
      (fix (indent+ !indentation) eols indents)

  | _ ->

      let offset = token_offset prev_tok in
      parse lexbuf token stack [] indent
        (fix (indent+offset) eols indents)

let get_indentations lexbuf =
  parse lexbuf SEMISEMI [] [] 0 []

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

let indent_string oc s =
  let lexbuf = Lexing.from_string s in

  IndentLexer.init ();
  let indentations = indenter lexbuf initial_state in

  let lines = IndentLexer.lines () in
  let nlines = Array.length lines in
  let indents = Array.create nlines 0 in

  let max_indent = ref 10 in

    if debug then begin
      Printf.eprintf "line:  spc tab bol   eol   content\n%!";
      Array.iteri (fun i line ->
        Printf.eprintf "%4d: [%2d][%2d][%4d][%4d]%!"
          i line.nspaces line.ntabs line.bol line.eol;
        let len = line.eol - line.bol - 1 in
        if len <> -1 then
          Printf.eprintf "%S\n%!"
            (String.sub s line.bol len)
        else
          Printf.eprintf "EOF\n%!"
      ) lines;
    end;

  List.iter (fun (line, nspaces) ->
    if debug then
      Printf.eprintf "%d: %d\n%!" line nspaces;
    if nspaces > !max_indent then max_indent := nspaces;
    indents.(line) <- nspaces;
  ) indentations;

  let max_indent = String.make !max_indent ' ' in

  for i = 0 to nlines - 2 do
    let line = lines.(i) in
    let len = line.eol - line.bol - 1 in
    Printf.fprintf oc "%s%s\n"
      (String.sub max_indent 0 indents.(i))
      (String.sub s line.bol len)
  done;
  let line = lines.(nlines-1) in
  let len = line.eol - line.bol - 1 in
  if len <> -1 then
    Printf.fprintf oc "%s%s"
      (String.sub max_indent 0 indents.(nlines-1))
      (String.sub s line.bol len);
  Printf.fprintf oc "%!"




let indent_channel ic oc =
  let s = string_of_channel ic in
  indent_string oc s
