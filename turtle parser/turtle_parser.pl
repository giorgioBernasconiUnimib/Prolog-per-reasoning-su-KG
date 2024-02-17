% il codice presente in questo file serve a parsare
% un file in formato standard turtle e ad asserire le
% triple contenute nel file nella base di conoscenza di
% prolog nella forma triple(Subj, Pred, Obj).

% istruzione per informare prolog che triple verr�
% creata solo dinamicamente durante l'esecuzione

:- dynamic triple/3.

% predicato parse_ttl, che prende una stringa in input,
% la splitta eliminando gli spazi e la passa all'analizzatore
% ricorsivo.

parse_ttl(Str) :-
    split_string(Str, "\n", "", L),
    rec_parse(L).

% predicato rec_parse, che controlla la lista in input a
% gruppi di 4 elementi e fa assert delle triple.

rec_parse([]).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "", [Subj, Pred, Obj, "."]),
    string_to_atom(Subj, AtomSubj),
    string_to_atom(Pred, AtomPred),
    string_to_atom(Obj, AtomObj),
    assert(triple(AtomSubj, AtomPred, AtomObj)),
    rec_parse(Other).

% predicato read_file/2, che permette di leggere un
% file il cui nome viene passato come argomento e successivamente
% lo passa al parser. il secondo argomento indica se
% il file path � assoluto o relativo

read_file(Name, rel) :-
    open(Name, read, File, []),
    read_file_to_string(File, Str, []),
    parse_ttl(Str).

read_file(Name, abs) :-
    absolute_file_name(Name, Abs, []),
    read_file_to_string(Abs, Str, []),
    parse_ttl(Str).

% predicato read_file/0, una specificazione di read_file/2
% che utilizza come file di input un file input standard
% chiamato input.ttl, che deve trovarsi nella stessa directory
% del parser.

read_file() :-
    read_file('input.ttl', abs).

% predicati read_abs_file/1 e read_rel_file/1, che permettono
% di considerare il nome di un file passato come argomento o
% come assoluto (abs), o come relatico(rel) senza doverlo specificare

read_abs_file(Name) :-
    read_file(Name, abs).

read_rel_file(Name) :-
    read_file(Name, rel).

% predicato triples/0 per listare tutte le triple presenti

triples() :-
    listing(triple(_, _, _)).

% predicato destroy_triples/0 per eliminare tutte le triple presenti

destroy_triples() :-
    retract(triple(_, _, _)),
    fail.

