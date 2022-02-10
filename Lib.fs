/// Modulo contenente funzioni utili all'implementazione del progetto.
module Lib

// È FORTEMENTE CONSIGLIATO L'UTILIZZO DELLE FUNZIONI QUI CONTENUTE: LE FUNZIONI SEGUENTI SONO PENSATE
// PER AIUTARVI NELL'IMPLEMENTAZIONE E PER DARVI QUALCHE SPUNTO SULLE STRUTTURE DATI DA UTILIZZARE.

open System

let tab_size = 4
let tabulate n = new string (' ', n * tab_size)

/// Elimina spazi, tabulazioni e caratteri di end-of-line all'inizio e alla fine di una stringa.
let trim_line (s : string) = s.Trim [|' '; '\t'; '\r'; '\n'|]

/// Spezza una stringa in singole parole separate da white space (cioè spazi e/o tabulazioni).
let tokenize_line (s : string) = s.Split ([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

/// Spezza una stringa in linee utilizzando come separatore il carattere di end-of-line. Tale carattere è multi-piattaforma, cioè si adegua automaticamente
/// ai diversi sistemi operativi.
let split_lines (s : string) = s.Split ([|Environment.NewLine|], StringSplitOptions.None) |> Array.map trim_line |> List.ofArray

/// Funzione di debug che stampa una lista di coppie int * string. Ogni elemento della lista rappresenta una linea di codice tabulato: l'intero è il valore
/// della tabulazione e la stringa il testo.
let print_tabbed_lines tlines =
    printfn "\n------------DEBUG-----------\n"
    for tabn, line in tlines do
        printfn "%2d, \"%s\"" tabn line

/// Funzione di appoggio per le due successive.
let private split_line toklen (tok : string) (line : string) =
    let i = line.IndexOf tok + toklen
    in
        trim_line (line.Substring (0, i)), trim_line (line.Substring (i, line.Length - i))

/// Chiamare split_line_in tok line divide l'argomento line usando la prima occorrenza tok come pivot. Il risultato è una coppia di stringhe: la parte prima di tok e
/// la parte dopo tok. La stringa tok è inclusa nella prima parte.
let split_line_inc (tok : string) = split_line tok.Length tok

/// Chiamare split_line_in tok line divide l'argomento line usando la prima occorrenza tok come pivot. Il risultato è una coppia di stringhe: la parte prima di tok e
/// la parte dopo tok. La stringa tok è inclusa nella seconda parte.
let split_line_exc = split_line 0

/// Produce una stringa data una lista di coppie int * string che rappresentano una linea tabulata del sorgente di output.
let render tlines =
    #if DEBUG
    print_tabbed_lines tlines
    #endif
    let rec R l =
        match l with
        | [] -> ""
        | (n, line) :: xs ->
            let tab = tabulate n
            in
                tab + line + "\n" + R xs
    R tlines

///Ritorna l'ultimo elemento di una lista non vuota
let rec last l =
    match l with
    | [] -> failwith "ERRORE: lista vuota"
    | [x] -> x
    | x::y::xs -> last (y::xs)

///Ritorna la testa di una lista non vuota
let head l =
    match l with
    | [] -> failwith "ERRORE: lista vuota"
    | x::xs -> x

///Ritorna la coda di una lista
let tail l =
    match l with
    | [] -> []
    | x::xs -> xs

///Data una lista ritorna la stessa lista rovesciata
let rec reverse l =
    match l with 
    | [] -> []
    | x::xs -> reverse(xs)@[x]

/// Data una lista ritorna la stessa lista senza l'ultimo elemento
let rec remove_last l =
    match l with
    | [] -> failwith "ERRORE: lista vuota"
    | [x] -> []
    | x::y::xs -> x::(remove_last (y::xs))

/// Data una lista ritorna la sua lunghezza
let rec length l =
    match l with
    | [] -> 0
    | x::xs -> 1 + length xs

/// Data una stringa (il programma) ritorna la lista dei token senza spazi multipli e \n
let clean (s : string) = 
    let rec tokenize (t: string list) =
        match t with
        | [] -> []
        | ""::xs -> [""]@(tokenize xs)
        | x::xs -> (tokenize_line x)@(tokenize xs)
    let rec trim t =
        match t with
        | [] -> []
        | x::xs -> (trim_line x)::(trim xs)
    in trim(tokenize(split_lines s))

/// Funzione che data un lista di stringhe (parole), ritorna una stringa unica rappresentante la concatenazione
/// delle singole parole
let rec list_to_string (l : string list) =
    match l with 
    | [] -> ""
    | [x] -> x
    | x::xs -> x + " " + list_to_string xs

///Data una stringa k ritorna true se k è una keyword
let is_keyword k =
    match k with
    | "let" | "=" | "in" | "if" | "elif" | "then" 
    | "else" | "match" | "with" | "|" | "fun" | "->" -> true
    | _ -> false 


/// Funzione che data una keyword (in, elif, else) e una lista contentente PI, 
/// ritorna un intero indicante la tabulazione della keyword
let tab_key (l: (int*string) list) (key : string) : int =
    let rec find_keys key =
        match key with
        | "let" -> ("let", "in")
        | "if" | "elif" -> ("if", "else")
        | _ -> failwith "ERRORE: CASO IMPOSSIBILE"
    let rec found_key (l : (int*string) list) (tabs : int list) (keys: string * string): int =
        match l, keys with 
        | [], _ -> last tabs                                   //[] indica fine del programma. Ritorno la t dell'ultimo 
        | (t,s)::xs, _ when s = "" -> find_key xs keys         //Costrutto non chiuso
        | (t,s)::xs, (start_k, end_k) when s <> "" ->
            let token = head(tokenize_line s)
            if (token = start_k) then
                if (t = last tabs) then found_key xs tabs keys  //Aggiungo la t del let solo se diversa dall'ultima presente
                else found_key xs (tabs@[t]) keys
            elif (token = end_k) then 
                if ((length tabs) = 1) then find_key xs keys    //Se length è 1 allora ho chiuso tutti i costrutti
                else found_key xs (remove_last tabs) keys       //Ow rimuovo solo l'ultima t
            else found_key xs tabs keys
    and find_key (l: (int*string) list) (keys : string * string): int =
        match l, keys with
        | [], _ -> failwith "ERRORE: CASO IMPOSSIBILE"
        | (t,s)::xs, _ when s = "" -> find_key xs keys
        | (t,s)::xs, (start_k, end_k) when s <> "" ->
            let token = head(tokenize_line s)
            if (token = start_k) then found_key xs [t] keys 
            else find_key xs keys
    in find_key (tail l) (find_keys key)

/// Funzione che conta il numero di match innestati
let rec last_match (ind_p : (int * string) list) = 
    match ind_p with
    | [] -> 0
    | (n, s)::ss -> 
        match head(tokenize_line(s)) with
        | "match" -> 1 + last_match ss
        | "|" -> last_match ss
        | _ -> 0


/// Funzione ausiliaria che data in input una stringa contentente il programma
/// ritorna la stringa con aggiunti i \n dove è necessario splittare. Considera w.
let rec add_newlines (w : int) (s : string) = 
/// Funzione mutuamente ricorsiva che dato w(width) e strl(lista dei token che compongono il programma) ritorna 
/// una lista di stringhe dove ogni elemento è una riga di programma splittata secondo i criteri prestabiliti
    let rec get_start_token (w : int) (prog : string list) (strl : string list) : string list =
        match prog, strl with
        | [], [] -> []
        | _, [] -> prog
        | _, s::ss -> 
            match s with
            | "" -> get_start_token w (prog@[s]) ss
            | "let" -> find_end_token w "=" prog [] strl
            | "if" | "elif" -> find_end_token w "then" prog [] strl
            | "else" ->
                if is_keyword (head(ss)) then get_start_token w (prog@[s]) ss
                else find_end_token w "" (prog@[s]) [] ss
            | "match" -> find_end_token w "with" prog [] strl
            | "|" -> find_end_token w "->" prog [] strl
            | "fun" -> find_end_token w "->" prog [] strl
            | "in" -> 
                if is_keyword (head(ss)) then get_start_token w (prog@[s]) ss
                else find_end_token w "" (prog@[s]) [] ss
            | _ -> find_end_token w "" prog [] strl

/// Funzione mutuamente ricorsiva che dato w(width), un token finale da cercare e lista dei token che compongono il programma
/// finché non trova il token finale inserisce nella nuova linea i token, quando end_tok viene trovato, la nuova linea viene aggiunta
/// al programma in base a w
    and find_end_token (w : int) (end_tok : string) (prog : string list) (line : string list) (strl : string list) : string list = 
        let sline = list_to_string line
        match end_tok with
        | "" ->                 // Non c'è end token, ad esempio con else o in
            match strl with
            | [] ->
                if sline.Length < w then 
                    let new_line = (last prog) + " " + sline                    //concateno la mia linea con quella precedente
                    get_start_token w ((remove_last prog)@[new_line]) strl     
                else get_start_token w (prog@[sline]) strl
            | ""::ss ->
                if sline.Length < w then 
                    let new_line = (last prog) + " " + sline                    //concateno la mia linea con quella precedente
                    get_start_token w ((remove_last prog)@[new_line]) strl          
                else get_start_token w (prog@[sline]) strl
            | s::ss ->
                if is_keyword s then 
                    if sline.Length < w then 
                        let new_line = (last prog) + " " + sline                //concateno la mia linea con quella precedente
                        get_start_token w ((remove_last prog)@[new_line]) strl          
                    else get_start_token w (prog@[sline]) strl
                else find_end_token w end_tok prog (line@[s]) ss
        | _ ->                  //c'è un end token da cercare
            match strl with
            | [] -> get_start_token w (prog@[sline]) strl
            | s::ss -> 
                if s = end_tok then get_start_token w (prog@[sline + " " + s]) ss   //splitto dopo l'end token
                else find_end_token w end_tok prog (line@[s]) ss

    in get_start_token w [] (clean s)