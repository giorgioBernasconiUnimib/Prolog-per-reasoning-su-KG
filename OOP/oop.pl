% predicato atomic_list(L), per controllare che la
% lista è composta da atomi

atomic_list([]).

atomic_list([L | Ls]) :-
    atom(L),
    atomic_list(Ls).

% predicato is_slot per controllare che l'argomento
% sia uno slot

is_slot(S) :-
    split_string(S, " ", "", [N, "=" | _]),
    atom_string(Nome, N),
    atom(Nome).

% predicato are_slot(L), che controlla se tutti i
% valori di una lista sono slot

are_slot([]).

are_slot([L | Ls]) :-
    is_slot(L),
    are_slot(Ls).

string_value(Str, AtomStr) :-
    sub_string(Str, 1, _, 1, AtomStr).


stringify([L], L, _) :- !.

stringify([L | Ls], Str, Separator) :-
    stringify(Ls, StrNew, Separator),
    string_concat(L, Separator, LNew),
    string_concat(LNew, StrNew, Str).

other_atomic(Other, Res) :-
    is_list(Other),
    stringify(Other, StrOther, " "),
    string_chars(StrOther, Chars),
    nth1(1, Chars, '\''),
    !,
    string_value(StrOther, Res).

other_atomic(Other, Res) :-
    is_list(Other),
    stringify(Other, StrOther, " "),
    string_chars(StrOther, Chars),
    nth1(1, Chars, '’'),
    !,
    string_value(StrOther, Res).

other_atomic(Other, Res) :-
    stringify(Other, StrOther, " "),
    atom_string(Res, StrOther).

string_compound(S, C) :-
    split_string(S, " ", "", [L, "=" | Other]),
    atom_string(N, L),
    other_atomic(Other, Rest),
    C =.. [=, N, Rest].

strings_compound([], []).

strings_compound([S | Ss], [C | Cs]) :-
    string_compound(S, C),
    strings_compound(Ss, Cs).

% predicato def_class(Nome, [Genitori], [Slots])

:- dynamic class/3.
:- dynamic istance/3.

def_class(Nome, Genitori, Slots) :-
    atom(Nome),
    class(Nome, _, _),
    !,
    is_list(Genitori),
    atomic_list(Genitori),
    is_list(Slots),
    are_slot(Slots),
    strings_compound(Slots, Slotsc),
    destroy_class(Nome),
    assert(class(Nome, Genitori, Slotsc)).

def_class(Nome, Genitori, Slots) :-
    atom(Nome),
    is_list(Genitori),
    atomic_list(Genitori),
    is_list(Slots),
    are_slot(Slots),
    strings_compound(Slots, Slotsc),
    assert(class(Nome, Genitori, Slotsc)).

% metodo is_class(C) per vedere se C è una classe

is_class(C) :-
    class(C, _, _).

are_classes([]).

are_classes([E|Es]) :-
    is_class(E),
    are_classes(Es).

classi :-
    listing(class(_, _, _)).

destroy_classes :-
    retract(class(_, _, _)),
    fail.

destroy_class(Name) :-
    retract(class(Name, _, _)).

istanze :-
    listing(istance(_, _, _)).

destroy_istances :-
    retract(istance(_, _, _)),
    fail.

destroy_istance(Name) :-
    retract(istance(Name, _, _)).

is_instance(Name) :-
    istance(Name, _, _).

is_istance(Name, Class) :-
    istance(Name, Class, _).

into(_, []) :-
    !, fail.

into(E1, [E2|_]) :-
    E1 =.. [=, Name, _],
    E2 =.. [=, Name, _],
    !.

into(E, [_|Es]) :-
    into(E, Es).

fill_not_in([], L, L).

fill_not_in([E|Es], L, [E|Fins]) :-
    not(into(E, L)), !,
    fill_not_in(Es, L, Fins).

fill_not_in([_|Es], L, Fins) :-
    fill_not_in(Es, L, Fins).

final_slots([], Classe, Fins, FinsTmp) :-
    class(Classe, _, FinsI),
    fill_not_in(FinsI, FinsTmp, Fins).

final_slots([S|Sls], Classe, Fins, FinsTmp) :-
    append(FinsTmp, S, FinsTmp1),
    final_slots(Sls, Classe, Fins, FinsTmp1).

% metodo create(Nome, Classe, Slots), che crea una
% istanza di Classe chiamata Nome e con i valori di Slots

create(_, Classe, _) :-
    not(is_class(Classe)),
    !, fail.

create(_, _, Slots) :-
    not(is_list(Slots)),
    !, fail.

create(_, _, Slots) :-
    not(are_slot(Slots)),
    !, fail.


create(Nome, Classe, Slots) :-
    atom(Nome),
    strings_compound(Slots, Slotsc),
    final_slots([Slotsc], Classe, SlotsF, []),
    assert(istance(Nome, Classe, SlotsF)).


% metodo create(Nome, Classe), che richiama create/3

create(Nome, Classe) :-
    create(Nome, Classe, []).

inst(Nome, Istanza) :-
    atom(Nome),
    istance(Nome, A, B),
    Istanza = istance(Nome, A, B).

slot(istance(_, _, [S | _]), Slot, Res) :-
    S =.. [=, Slot, Res], !.

slot(istance(Nome, Classe, [_ | Slots]), Slot, Res) :-
    !, slot(istance(Nome, Classe, Slots), Slot, Res).

slot(Nome, Slot, Res) :-
    inst(Nome, Istanza),
    slot(Istanza, Slot, Res).
