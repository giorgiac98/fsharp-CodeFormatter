module FSharpCodeFormatter.Formatter

open Lib

// se non vuoi realizzare la versione avanzata, non modificarla
let split (w : int) (s : string) = //split_lines s
    add_newlines w s

/// Funzone ausiliaria che dati in input PI(programma indentato), n(tab), PnI(programma non indentato) ritorna il programma indentato
let rec indent_block (ind_p: (int*string) list, n: int, not_ind_p: string list) =
    match not_ind_p with 
    | [] -> ind_p                                                   // Input terminato
    | ""::ss -> indent_block(ind_p@[(0, "")], 0, ss)                // Riga vuota
    | s::ss when n = 0 -> indent_block(ind_p@[(0, s)], n+1, ss)     // n = 0 indica una funzione top-level
    | s::ss when n <> 0 ->
        match (tokenize_line s) with
        | "let"::k -> 
            if (last k = "=") then indent_block(ind_p@[(n, s)], n+1, ss)
            else indent_block(ind_p@[(n, s)], n, ss)
        | "if"::k -> 
            if (last k = "then") then indent_block(ind_p@[(n, s)], n+1, ss)
            else indent_block(ind_p@[(n, s)], n, ss)
        | "elif"::k -> 
            let t = (tab_key ind_p "if")
            if (last k = "then") then indent_block(ind_p@[(t, s)], t+1, ss)
            else indent_block(ind_p@[(t, s)], t, ss)
        | "else"::k -> 
            let t = (tab_key ind_p "if")
            if (k = []) then indent_block(ind_p@[(t, s)], t+1, ss)
            else indent_block(ind_p@[(t, s)], t-1, ss)
        | "fun"::k -> 
            if (last k = "->") then indent_block(ind_p@[(n, s)], n+1, ss)
            else indent_block(ind_p@[(n, s)], n, ss)
        | "match"::k -> indent_block(ind_p@[(n, s)], n, ss)
        | "|"::k -> 
            if (last k = "->") then indent_block(ind_p@[(n, s)], n+1, ss)
            else
                match ss with
                | [] -> ind_p@[(n,s)]
                | y::ys -> 
                    match (tokenize_line y) with
                    | "|"::k -> indent_block(ind_p@[(n, s)], n, ss)
                    | _ ->indent_block(ind_p@[(n, s)], (n-last_match (reverse ind_p)), ss)
        | "in"::k -> 
            let t = (tab_key ind_p "let")
            if (k = []) then indent_block(ind_p@[(t, s)], t+1, ss)
            else indent_block(ind_p@[(t, s)], t-1, ss)
        | _ -> indent_block(ind_p@[(n, s)], n-1, ss) 


// questa è la funzione principale da implementare correttamente sia per versione avanzata che per quella normale
let rec indent (lines : string list) =
    indent_block ([], 0, lines)