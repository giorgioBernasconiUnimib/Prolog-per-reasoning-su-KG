% il codice presente in questo file serve a parsare
% un file in formato standard turtle e ad asserire le
% triple contenute nel file nella base di conoscenza di
% prolog nella forma triple(Subj, Pred, Obj).

% istruzione per informare prolog che triple verrà
% creata solo dinamicamente durante l'esecuzione

:- dynamic triple/3.

% istruzione per informare prolog che prefix verrà
% creata solo dinamicamente durante l'esecuzione

:- dynamic prefix_rdf/2.

% predicato parse_ttl, che prende una stringa in input,
% la splitta eliminando gli spazi e la passa all'analizzatore
% ricorsivo.

parse_ttl(Str) :-
    split_string(Str, "\n", "", L),
    rec_parse(L).

% predicato rec_parse, che controlla la lista in input a
% gruppi di 4 elementi e fa assert delle triple.

rec_parse([]).

rec_parse(["" | Other]) :-
    !, rec_parse(Other).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "",["@prefix", Prefisso, Significato, "."]), !,
    sub_string(Prefisso, 0, _, 1, Stri),
    string_to_atom(Stri, PrefAtom),
    string_to_atom(Significato, MeanAtom),
    assert(prefix_rdf(PrefAtom, MeanAtom)),
    rec_parse(Other).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "", Splitted),
    last_of(Splitted, ";"), !,
    skip_str_split(Splitted, [Subj, Pred, Obj, ";"]),
    sub_prefixes(Subj, AtomSubj),
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    is_subject(AtomSubj),
    is_pred(AtomPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)),
    handle_preds(Other, AtomSubj, Resto),
    rec_parse(Resto).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "", Splitted),
    skip_str_split(Splitted, [Subj, Pred, Obj | Linea]),
    string_chars(Obj, Chars),
    last_of(Chars, ','), !,
    sub_string(Obj, 0, _, 1, Stri),
    sub_prefixes(Subj, AtomSubj),
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Stri, AtomObj),
    is_subject(AtomSubj),
    is_pred(AtomPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)),
    handle_obj(Linea, AtomSubj, AtomPred),
    rec_parse(Other).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "", Splitted),
    last_of(Splitted, "."), !,
    skip_str_split(Splitted, [Subj, Pred, Obj, "."]),
    sub_prefixes(Subj, AtomSubj),
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    is_subject(AtomSubj),
    is_pred(AtomPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)),
    rec_parse(Other).

% predicato handle_preds/3, che si occupa di gestire i ;
% nel file turtle.

handle_preds([E | Others], AtomSubj, Others) :-
    split_string(E, " ", "\t", Splitted),
    skip_str_split(Splitted, [Pred, Obj, "."]), !,
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    is_subject(AtomSubj),
    is_pred(AtomPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)).

handle_preds([E | Others], AtomSubj, Altri) :-
    split_string(E, " ", "\t", Splitted),
    skip_str_split(Splitted, [Pred, Obj, ";"]), !,
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    is_subject(AtomSubj),
    is_pred(AtomPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)),
    handle_preds(Others, AtomSubj, Altri).

% predicato handle_obj/3, che si occupa di gestire le ,
% nel file turtle.

handle_obj([Obj, "."], AtomSubj, AtomPred) :-
    !, string_chars(Obj, Chars),
    not(last_of(Chars, ',')),
    sub_prefixes(Obj, AtomObj),
    is_subject(AtomSubj),
    is_pred(AtomPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)).

handle_obj([Obj | Altri], AtomSubj, AtomPred) :-
    string_chars(Obj, Chars),
    last_of(Chars, ","), !,
    sub_string(Obj, 0, _, 1, Stri),
    sub_prefixes(Stri, AtomObj),
    is_subject(AtomSubj),
    is_pred(AtomPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)),
    handle_obj(Altri, AtomSubj, AtomPred).

% predicato read_file/2, che permette di leggere un
% file il cui nome viene passato come argomento e successivamente
% lo passa al parser. il secondo argomento indica se
% il file path è assoluto o relativo

read_file(Name, abs) :-
    open(Name, read, File, []),
    read_file_to_string(File, Str, []),
    parse_ttl(Str).

read_file(Name, rel) :-
    absolute_file_name(Name, Abs, []),
    read_file_to_string(Abs, Str, []),
    parse_ttl(Str).

% predicato read_file/0, una specificazione di read_file/2
% che utilizza come file di input un file input standard
% chiamato input.ttl, che deve trovarsi nella stessa directory
% del parser.

read_file :-
    read_file('input.ttl', rel).

% predicati read_abs_file/1 e read_rel_file/1, che permettono
% di considerare il nome di un file passato come argomento o
% come assoluto (abs), o come relatico(rel) senza doverlo specificare

read_abs_file(Name) :-
    read_file(Name, abs).

read_rel_file(Name) :-
    read_file(Name, rel).

% predicato triples/0 per listare tutte le triple presenti

triples :-
    listing(triple(_, _, _)).

% predicato destroy_triples/0 per eliminare tutte le triple presenti

destroy_triples :-
    retract(triple(_, _, _)),
    fail.

% predicato prefixes/0 per listare tutti i prefissi presenti

prefixes :-
    listing(prefix_rdf(_, _)).

% predicato destroy_prefixes/0 per eliminare tutti i prefissi presenti

destroy_prefixes :-
    retract(prefix_rdf(_, _)),
    fail.

% predicato last_of/2, che data una lista come primo argomento ed un
% elemento come secondo argomento e ha successo se il secondo argomento
% è l'ultimo elemento della lista

last_of([E], E).

last_of([_ | Others], E) :-
    last_of(Others, E).

% predicato skip_str/3, che data una lista di stringhe come primo
% argomento il quale primo elemento inizia con il carattere ", raggruppa
% tutti gli elementi fino al primo che finisce con "

skip_str([], _, _) :- fail.

skip_str([E | Others], Str, Resto) :-
    string_chars(E, ['"' | _]), !,
    skip_str(Others, Res, Resto),
    append([[E], Res], StrLs),
    atomics_to_string(StrLs, " ", Str).

skip_str([E | Resto], [E], Resto) :-
    string_chars(E, Els),
    member('"', Els), !.

skip_str([E | Others], Str, Resto) :-
    skip_str(Others, Res, Resto),
    append([[E], Res], Str).

% funzione skip_str_split, che data una lista di stringhe riunisce
% le parti di stringhe che erano state divise con split_string

skip_str_split([], []).

skip_str_split([E | Others], LsFin) :-
    string_chars(E, ['"' | Altri]),
    member('"', Altri), !,
    skip_str_split(Others, Res),
    append([[E], Res], LsFin).

skip_str_split([E | Others], Lsfin) :-
    string_chars(E, ['"' | _]), !,
    skip_str([E | Others], Str, Resto),
    skip_str_split(Resto, Res),
    append([[Str], Res], Lsfin).

skip_str_split([E | Others], LsFin) :-
    skip_str_split(Others, Res),
    append([[E], Res], LsFin).

% predicato sub_prefixes, che sostituisce i prefissi nelle triple

sub_prefixes(Element, NewElement) :-
    split_string(Element, ":", "", [PreStr | Other]),
    string_to_atom(PreStr, Pre),
    prefix_rdf(Pre, Meaning), !,
    atom_string(Meaning, MeaningStr),
    sub_string(MeaningStr, 0, _, 1, SubMean),
    append([[SubMean], Other, [">"]], Lista),
    atomics_to_string(Lista, ResStr),
    atom_string(NewElement, ResStr).

sub_prefixes(Element, NewElement) :-
    atom_string(NewElement, Element).

is_subject(Subj) :-
    is_iri(Subj), !.

is_subject(Subj) :-
    is_blank(Subj), !.

is_pred(Pred) :-
    is_iri(Pred), !.

is_obj(Obj) :-
    is_iri(Obj), !.

is_obj(Obj) :-
    is_blank(Obj), !.

is_obj(Obj) :-
    atom(Obj), !.

is_iri(Iri) :-
    atom_string(Iri, Stri),
    string_chars(Stri, ['<' | Altri]),
    last_of(Altri, '>').

is_blank(Blank) :-
    atom_string(Blank, Stri),
    string_chars(Stri, ['_', ':' | _]).
