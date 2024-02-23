% --------------------------turtle parser--------------------------


% il codice presente in questo file serve a parsare
% un file in formato standard turtle e ad asserire le
% triple contenute nel file nella base di conoscenza di
% prolog nella forma triple(Subj, Pred, Obj).

% istruzione per informare prolog che triple verr‡
% creata solo dinamicamente durante l'esecuzione

:- dynamic triple/3.

% istruzione per informare prolog che prefix verr‡
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

rec_parse([Comment | Other]) :-
    string_chars(Comment, ['#' | _]), !,
    rec_parse(Other).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "",["@prefix", Prefisso, Significato, "."]), !,
    sub_string(Prefisso, 0, _, 1, Stri),
    sub_string(Significato, 0, _, 1, Sign),
    atom_string(Significato, Meaning),
    string_to_atom(Stri, PrefAtom),
    string_to_atom(Sign, MeanAtom),
    is_iri(Meaning),
    assert(prefix_rdf(PrefAtom, MeanAtom)),
    rec_parse(Other).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "", Splitted),
    last_of(Splitted, ";"), !,
    skip_str_split(Splitted, [Subj, Pred, Obj, ";"]),
    sub_prefixes(Subj, AtomSubj),
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    control_pred(AtomPred, NewPred),
    is_subject(AtomSubj),
    is_pred(NewPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, NewPred, AtomObj)),
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
    control_pred(AtomPred, NewPred),
    is_subject(AtomSubj),
    is_pred(NewPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, NewPred, AtomObj)),
    handle_obj(Linea, AtomSubj, NewPred),
    rec_parse(Other).

rec_parse([Line | Other]) :-
    split_string(Line, " ", "", Splitted),
    last_of(Splitted, "."), !,
    skip_str_split(Splitted, [Subj, Pred, Obj, "."]),
    sub_prefixes(Subj, AtomSubj),
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    control_pred(AtomPred, NewPred),
    is_subject(AtomSubj),
    is_pred(NewPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, NewPred, AtomObj)),
    rec_parse(Other).

% predicato handle_preds/3, che si occupa di gestire i ;
% nel file turtle.

handle_preds([E | Others], AtomSubj, Others) :-
    split_string(E, " ", "\t", Splitted),
    skip_str_split(Splitted, [Pred, Obj, "."]), !,
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    control_pred(AtomPred, NewPred),
    is_subject(AtomSubj),
    is_pred(NewPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, NewPred, AtomObj)).

handle_preds([E | Others], AtomSubj, Altri) :-
    split_string(E, " ", "\t", Splitted),
    skip_str_split(Splitted, [Pred, Obj, ";"]), !,
    sub_prefixes(Pred, AtomPred),
    sub_prefixes(Obj, AtomObj),
    control_pred(AtomPred, NewPred),
    is_subject(AtomSubj),
    is_pred(NewPred),
    is_obj(AtomObj),
    assert(triple(AtomSubj, NewPred, AtomObj)),
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
% il file path Ë assoluto o relativo

read_file(Name, abs) :-
    reset,
    read_file_to_string(Name, Str, []),
    parse_ttl(Str).

read_file(Name, rel) :-
    reset,
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
% Ë l'ultimo elemento della lista

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
    append([[MeaningStr], Other, [">"]], Lista),
    atomics_to_string(Lista, ResStr),
    atom_string(NewElement, ResStr).

sub_prefixes(Element, NewElement) :-
    atom_string(NewElement, Element), !.

% predicato per controllare se un elemento Ë un soggetto turtle

is_subject(Subj) :-
    is_iri(Subj), !.

is_subject(Subj) :-
    is_blank(Subj), !.

% predicato per controllare se un elemento Ë un predicato turtle

is_pred(Pred) :-
    is_iri(Pred), !.

% predicato per controllare se un elemento Ë un oggetto turtle

is_obj(Obj) :-
    is_iri(Obj), !.

is_obj(Obj) :-
    is_blank(Obj), !.

is_obj(Obj) :-
    atom(Obj), !.

% predicato per controllare se un elemento Ë un iri

is_iri(Iri) :-
    atom_string(Iri, Stri),
    string_chars(Stri, ['<' | Altri]),
    last_of(Altri, '>'), !.

% predicato per controllare se un elemento Ë un blank node

is_blank(Blank) :-
    atom_string(Blank, Stri),
    string_chars(Stri, ['_', ':' | _]), !.

% predicato per sostituire ad 'a' rdf:type

control_pred('a', '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>') :- !.

control_pred(X, X) :- !.


% ---------------------------reasoner--------------------------

% implementazione del reasoner che, data una base di conoscenza
% (standard come quella presente in coda al file, oppure caricata con
% read_file), effettua inferenze rdfs e owl per ricavare nuove triple


% Verifica se una classe Ë una sottoclasse
is_subClass(SubClass,Class) :-
    triple(SubClass, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', Class).

% Verifica se un'entit‡ Ë una propriet‡†
is_property(Entity) :-
    triple(Entity, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2002/07/owl#ObjectProperty>').

% Inferisce che soggetti e oggetti di rdfs:subClassOf sono classi
infer_class_membership :-
    forall(triple(X, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', Y),
           ((not(triple(X, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>')) ->
            assert(triple(X, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>')),
                format("Inferred and asserted that ~w is a class.\n", [X]); true),
            (not(triple(Y, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>')) ->
            assert(triple(Y, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>')),
                format("Inferred and asserted that ~w is a class.\n", [Y]); true))).

% Inferisce il tipo del soggetto basato sul rdfs:domain della propriet‡†
infer_property_domain :-
    forall((triple(S, P, _), is_property(P), triple(P, '<http://www.w3.org/2000/01/rdf-schema#domain>', C)),
           ((not(triple(S, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', C)) ->
            assert(triple(S, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', C)),
                format("Inferred ~w rdf:type ~w based on the domain of ~w.\n", [S, C, P])
            ; true)
           )).


% Assicura la bidirezionalit‡† di una equivalenza tra classi
infer_equivalent_bidirectional(ClassA, ClassB) :-
    (not(triple(ClassB, '<http://www.w3.org/2002/07/owl#equivalentClass>', ClassA)) ->
    assert(triple(ClassB, '<http://www.w3.org/2002/07/owl#equivalentClass>', ClassA)),
      format("Asserted bidirectional equivalence: ~w owl:equivalentClass ~w.\n", [ClassB, ClassA])
    ; true).

% Assicura la bidirezionalit‡† di una disgiunzione tra classi
infer_disjoint_bidirectional(ClassA, ClassB) :-
    (not(triple(ClassB, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassA)) ->
    assert(triple(ClassB, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassA)),
      format("Asserted bidirectional disjointness: ~w owl:disjointWith ~w.\n", [ClassB, ClassA])
    ; true).

% Chiamata per assicurare la bidirezionalit‡† delle relazioni tra classi
infer_bidirectional_relations :-
    forall(triple(ClassA, '<http://www.w3.org/2002/07/owl#equivalentClass>', ClassB), infer_equivalent_bidirectional(ClassA, ClassB)),
    forall(triple(ClassA, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassB), infer_disjoint_bidirectional(ClassA, ClassB)).

% Assicura la bidirezionalit‡† di una equivalenza (sameAs) tra individui
infer_sameAs_bidirectional(IndividualA, IndividualB) :-
    (not(triple(IndividualB, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualA)) ->
        assert(triple(IndividualB, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualA)),
        format("Asserted bidirectional sameAs: ~w owl:sameAs ~w.\n", [IndividualB, IndividualA])
    ; true).

% Assicura la bidirezionalit‡† di una disgiunzione (differentFrom) tra individui
infer_differentFrom_bidirectional(IndividualA, IndividualB) :-
    (not(triple(IndividualB, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualA)) ->
        assert(triple(IndividualB, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualA)),
        format("Asserted bidirectional differentFrom: ~w owl:differentFrom ~w.\n", [IndividualB, IndividualA])
    ; true).

% Chiamata per assicurare la bidirezionalit‡ delle relazioni tra individui
infer_individual_bidirectional_relations :-
    forall(triple(IndividualA, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualB),
           infer_sameAs_bidirectional(IndividualA, IndividualB)),
    forall(triple(IndividualA, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualB),
           infer_differentFrom_bidirectional(IndividualA, IndividualB)).

% Aggiunge transitivit‡ alla disgiunzione tra classi
infer_transitive_subClass :-
    forall((triple(ClassA, '<http://www.w3.org/2002/07/owl#equivalentClass>', ClassB), is_subClass(ClassA,ClassC)),
           (not(triple(ClassB, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassC)) ->
           assert(triple(ClassB, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassC)),
             format("Inferred and asserted subClassOf: ~w rdfs:subClassOf ~w.\n", [ClassB, ClassC])
           ; true)).

% Assicura che le triple rispettino le restrizioni di domain e range per le propriet‡†
apply_domain_and_range_restrictions :-
    forall((triple(S, P, O), is_property(P)), apply_domain_and_range_restrictions(S, P, O)).

% Applica le restrizioni di domain e range per una data propriet‡†
apply_domain_and_range_restrictions(S, P, O) :-
    % Gestione del domain
    (   triple(P, '<http://www.w3.org/2000/01/rdf-schema#domain>', Domain),
        not(triple(S, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', Domain)) ->
    assert(triple(S, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', Domain)),
        format("Inferred and asserted domain for ~w: ~w rdf:type ~w.\n", [P, S, Domain])
    ;   true),
    % Gestione del range
    (   triple(P, '<http://www.w3.org/2000/01/rdf-schema#range>', Range),
        not(triple(O, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', Range)) ->
    assert(triple(O, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', Range)),
        format("Inferred and asserted range for ~w: ~w rdf:type ~w.\n", [P, O, Range])
    ;   true).

% Inferenza basata sulle sottoclassi
infer_subclass_relations :-
    findall(_, (triple(Entity, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', SubClass),
                infer_subclass_relation(Entity, SubClass)), _).

%Viene ripetuta per tutte le triple grazie a infer_subclass_relations/0
infer_subclass_relation(Entity, SubClass) :-
    is_subClass(SubClass,Class),
    (   not(triple(Entity, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', Class)) ->
    assert(triple(Entity, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', Class))).

% Inferenza basata sulle sotto propriet√†
infer_subproperty_relations :-
    findall(_, (triple(S, SubP, O), infer_subproperty_relation(S, SubP, O)), _).

% Viene ripetuta per tutte le triple grazie a infer_subproperty_relations/0
infer_subproperty_relation(S, SubP, O) :-
    triple(SubP, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', P),
    (   \+ triple(S, P, O) -> assert(triple(S, P, O))).

% Gestione dell'equivalenza tra classi
infer_equivalent_classes :-
    forall(triple(ClassA, '<http://www.w3.org/2002/07/owl#equivalentClass>', ClassB),
                (forall(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassA),
                       (not(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassB)) ->
                       assert(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassB)) ;
                       true)),
                forall(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassB),
                       (not(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassA)) ->
                       assert(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassA)) ;
                       true)))).

% Aggiunge transitivit√† alla disgiunzione tra classi
infer_transitive_disjointness :-
    forall((triple(ClassA, '<http://www.w3.org/2002/07/owl#equivalentClass>', ClassB),
            triple(ClassA, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassC)),
           (not(triple(ClassB, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassC)) ->
           assert(triple(ClassB, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassC)),
             format("Inferred and asserted disjointness: ~w owl:disjointWith ~w.\n", [ClassB, ClassC])
           ; true)),
    forall((triple(ClassB, '<http://www.w3.org/2002/07/owl#equivalentClass>', ClassA),
            triple(ClassA, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassC)),
           (not(triple(ClassB, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassC)) ->
           assert(triple(ClassB, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassC)),
             format("Inferred and asserted disjointness: ~w owl:disjointWith ~w.\n", [ClassB, ClassC])
           ; true)).

% Individua le inferenze che, per via della disgunzione tra
% classi, causano una inconsistenza
infer_disjoint_classes :-
    findall(Instance-(ClassA-ClassB),
            (triple(ClassA, '<http://www.w3.org/2002/07/owl#disjointWith>', ClassB),
             triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassA),
             triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassB)),
            Inconsistencies),
    report_and_remove_inconsistencies(Inconsistencies).

% Gestisce la lista "Inconsistencies" rimuovendola dalle triple
% attualmente in considerazione e segnalando il problema
report_and_remove_inconsistencies([]).
report_and_remove_inconsistencies([Instance-(ClassA-ClassB) | Rest]) :-
    format('Inconsistency found: Instance ~w cannot belong to both disjoint classes ~w and ~w.\n', [Instance, ClassA, ClassB]),
    (triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassA) ->
    retract(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassA)); true),
    (triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassB) ->
    retract(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', ClassB)); true),
    report_and_remove_inconsistencies(Rest).

%Gestione di uguaglianza tra individui
infer_same_as_relations :-
    forall(triple(IndividualA, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualB),
                (forall(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', IndividualA),
                       (not(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', IndividualB)) ->
                       assert(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', IndividualB)) ;
                       true)),
                forall(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', IndividualB),
                       (not(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', IndividualA)) ->
                       assert(triple(Instance, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', IndividualA)) ;
                       true)))).

% Individua le coppie di individui che, per via della loro disuguaglianza dichiarata,
% causano una inconsistenza
infer_disjoint_individuals :-
    findall((IndividualA, IndividualB),
            (triple(IndividualA, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualB),
             triple(IndividualA, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualB)),
            Inconsistencies),
    report_and_remove_different_inconsistencies_on_individual(Inconsistencies).

% Gestisce la lista di coppie di individui dichiarati come diversi e
% uguali al contempo, rimuovendole dalle triple attualmente
% in considerazione e segnalando il problema
report_and_remove_different_inconsistencies_on_individual([]).

report_and_remove_different_inconsistencies_on_individual([(IndividualA, IndividualB) | Rest]) :-
    format('Inconsistency found: Individuals ~w and ~w cannot be declared as both different and the same.\n', [IndividualA, IndividualB]),
    (triple(IndividualA, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualB) ->
    retract(triple(IndividualA, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualB)); true),
    (triple(IndividualB, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualA) ->
    retract(triple(IndividualB, '<http://www.w3.org/2002/07/owl#differentFrom>', IndividualA)); true),
    (triple(IndividualA, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualB) ->
    retract(triple(IndividualA, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualB)); true),
    (triple(IndividualB, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualA) ->
    retract(triple(IndividualB, '<http://www.w3.org/2002/07/owl#sameAs>', IndividualA)); true),
    report_and_remove_different_inconsistencies_on_individual(Rest).

% per applicare la propriet‡ di simmetria alle propriet‡ che la hanno

infer_symmetric_property :-
    forall(triple(P, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2002/07/owl#SymmetricProperty>'),
           (forall(triple(A, P, B), (not(triple(B, P, A)) -> assert(triple(B, P, A)) ; true)))).

% per inferire relazioni inverse

infer_inverse_property :-
    forall(triple(P, '<http://www.w3.org/2002/07/owl#inverseOf>', Q),
           (forall(triple(A, P, B), (not(triple(B, Q, A)) -> assert(triple(B, Q, A)) ; true)))).

% per applicare la propriet‡ di transitivit‡ alle propriet‡ che la hanno

infer_transitive_property :-
    forall(triple(P, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2002/07/owl#TransitiveProperty>'),
           (forall(triple(A, P, B),
                   (forall(triple(B, P, C), (not(triple(A, P, C)) -> assert(triple(A, P, C)) ; true)))))).

% per applicare la propriet‡ di funzionalit‡ alle propriet‡ che la hanno

infer_functional_property :-
    forall(triple(P, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2002/07/owl#FunctionalProperty>'),
           (forall(triple(A, P, B),
                   (forall(triple(X, P, C), ((not(triple(B, '<http://www.w3.org/2002/07/owl#sameAs>', C)), not(B == C), A == X) ->
                                            assert(triple(B, '<http://www.w3.org/2002/07/owl#sameAs>', C)) ; true)))))).

% per inferire gli assiomi delle propriet‡

infer_property_axioms :-
    infer_symmetric_property,
    infer_inverse_property,
    infer_transitive_property,
    infer_functional_property.

% Applicazione delle nuove inferenze
apply_inferences :-
    infer_class_membership,
    infer_property_domain,    infer_property_axioms,
    infer_bidirectional_relations,
    infer_individual_bidirectional_relations,
    apply_domain_and_range_restrictions,
    infer_subclass_relations,
    infer_subproperty_relations,
    (infer_transitive_disjointness,infer_bidirectional_relations),
    infer_disjoint_classes,
    infer_equivalent_classes,
    infer_transitive_subClass,
    infer_same_as_relations,
    infer_disjoint_individuals,
    thing_hierarchy,
    nothing_hierarchy,
    topObjectProperty_hierarchy,
    report_loops,
    report_property_loops.

% Stampa tutte le triple, sia quelle del dataset sia quelle inferite
print_all_triples :-
    forall(triple(S, P, O), (add_prefixes(S, Sr),
                             add_prefixes(P, Pr),
                             add_prefixes(O, Or),
                             format("~w ~w ~w.\n", [Sr, Pr, Or]))).

% funzione per rendere tutte le classi subclass di thing

thing_hierarchy :-
    forall(triple(S, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>'),
           (\+ triple(S, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', '<http://www.w3.org/2002/07/owl#Thing>') ->
           assert(triple(S, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', '<http://www.w3.org/2002/07/owl#Thing>')))).

% funzione per rendere tutte le classi superclassi di nothing

nothing_hierarchy :-
    forall(triple(S, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>'),
           (\+ triple('<http://www.w3.org/2002/07/owl#Nothing>', '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', S) ->
           assert(triple('<http://www.w3.org/2002/07/owl#Nothing>', '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', S)))).

% funzione per rendere tutte le propriet‡ subproperty di topObjectProperty

topObjectProperty_hierarchy :-
    forall(triple(S, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2002/07/owl#ObjectProperty>'),
           (\+ triple(S, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', '<http://www.w3.org/2002/07/owl#TopObjectProperty>') ->
           assert(triple(S, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', '<http://www.w3.org/2002/07/owl#TopObjectProperty>')))).

% funzioni per controllare che non ci siano loop nelle subClass

report_loops :-
    findall(ClassA-ClassB,
            (triple(ClassA, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassB),
             triple(ClassB, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassA)),
            Inconsistencies),
    report_and_remove_class_inconsistencies(Inconsistencies).

report_and_remove_class_inconsistencies([]).
report_and_remove_class_inconsistencies([ClassA-ClassB | Rest]) :-
    format('Inconsistency found: subclass loop between classes ~w and ~w.\n', [ClassA, ClassB]),
    (triple(ClassA, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassB) ->
    retract(triple(ClassA, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassB)); true),
    (triple(ClassB, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassA) ->
    retract(triple(ClassB, '<http://www.w3.org/2000/01/rdf-schema#subClassOf>', ClassA)); true),
    report_and_remove_class_inconsistencies(Rest).

% funzioni per controllare che non ci siano loop nelle subProperties

report_property_loops :-
    findall(PropA-PropB,
            (triple(PropA, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', PropB),
             triple(PropB, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', PropA)),
            Inconsistencies),
    report_and_remove_prop_inconsistencies(Inconsistencies).

report_and_remove_prop_inconsistencies([]).
report_and_remove_prop_inconsistencies([PropA-PropB | Rest]) :-
    format('Inconsistency found: subproperty loop between properties ~w and ~w.\n', [PropA, PropB]),
    (triple(PropA, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', PropB) ->
    retract(triple(PropA, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', PropB)); true),
    (triple(PropB, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', PropA) ->
    retract(triple(PropB, '<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', PropA)); true),
    report_and_remove_prop_inconsistencies(Rest).

% predicato add_prefixes, che converte gli URI estesi nei loro corrispettivi prefissati

add_prefixes(ExtendedElement, PrefixedElement) :-
    prefix_rdf(Prefix, UriStart),
    atom_string(UriStart, UriStartStr),
    sub_string(ExtendedElement, 0, _, _, UriStartStr), !,
    string_length(UriStartStr, UriStartLen),
    sub_atom(ExtendedElement, UriStartLen, _, 1, Suffix),
    atom_concat(Prefix, ':', PrefixColon),
    atom_concat(PrefixColon, Suffix, PrefixedElement).

add_prefixes(Element, Element).

% Avvia l'esecuzione delle inferenze e stampa le triple risultanti
start_execution :-
    apply_inferences,
    print_all_triples.

% funzione select per effettuare query

select(Subj, Pred, Obj) :-
    var(Subj),
    var(Pred),
    var(Obj), !,
    print_all_triples.

select(Subj, Pred, Obj) :-
    not(var(Subj)),
    var(Pred),
    var(Obj), !,
    atom_string(Subj, StrSubj),
    sub_prefixes(StrSubj, SubSubj),
    forall(triple(SubSubj, P, O), (add_prefixes(SubSubj, Sr),
                             add_prefixes(P, Pr),
                             add_prefixes(O, Or),
                             format("~w ~w ~w.\n", [Sr, Pr, Or]))).

select(Subj, Pred, Obj) :-
    var(Subj),
    not(var(Pred)),
    var(Obj), !,
    control_pred(Pred, Pred1),
    atom_string(Pred1, StrPred),
    sub_prefixes(StrPred, SubPred),
    forall(triple(S, SubPred, O), (add_prefixes(S, Sr),
                             add_prefixes(SubPred, Pr),
                             add_prefixes(O, Or),
                             format("~w ~w ~w.\n", [Sr, Pr, Or]))).

select(Subj, Pred, Obj) :-
    var(Subj),
    var(Pred),
    not(var(Obj)), !,
    atom_string(Obj, StrObj),
    sub_prefixes(StrObj, SubObj),
    forall(triple(S, P, SubObj), (add_prefixes(S, Sr),
                             add_prefixes(P, Pr),
                             add_prefixes(SubObj, Or),
                             format("~w ~w ~w.\n", [Sr, Pr, Or]))).

select(Subj, Pred, Obj) :-
    not(var(Subj)),
    not(var(Pred)),
    var(Obj), !,
    control_pred(Pred, Pred1),
    atom_string(Pred1, StrPred),
    atom_string(Subj, StrSubj),
    sub_prefixes(StrSubj, SubSubj),
    sub_prefixes(StrPred, SubPred),
    forall(triple(SubSubj, SubPred, O), (add_prefixes(SubSubj, Sr),
                             add_prefixes(SubPred, Pr),
                             add_prefixes(O, Or),
                             format("~w ~w ~w.\n", [Sr, Pr, Or]))).

select(Subj, Pred, Obj) :-
    var(Subj),
    not(var(Pred)),
    not(var(Obj)), !,
    control_pred(Pred, Pred1),
    atom_string(Pred1, StrPred),
    sub_prefixes(StrPred, SubPred),
    atom_string(Obj, StrObj),
    sub_prefixes(StrObj, SubObj),
    forall(triple(S, SubPred, SubObj), (add_prefixes(S, Sr),
                             add_prefixes(SubPred, Pr),
                             add_prefixes(SubObj, Or),
                             format("~w ~w ~w.\n", [Sr, Pr, Or]))).

select(Subj, Pred, Obj) :-
    not(var(Subj)),
    var(Pred),
    not(var(Obj)), !,
    atom_string(Subj, StrSubj),
    sub_prefixes(StrSubj, SubSubj),
    atom_string(Obj, StrObj),
    sub_prefixes(StrObj, SubObj),
    forall(triple(SubSubj, P, SubObj), (add_prefixes(SubSubj, Sr),
                             add_prefixes(P, Pr),
                             add_prefixes(SubObj, Or),
                             format("~w ~w ~w.\n", [Sr, Pr, Or]))).

select(Subj, Pred, Obj) :-
    not(var(Subj)),
    not(var(Pred)),
    not(var(Obj)), !,
    control_pred(Pred, Pred1),
    atom_string(Pred1, StrPred),
    atom_string(Subj, StrSubj),
    sub_prefixes(StrSubj, SubSubj),
    sub_prefixes(StrPred, SubPred),
    atom_string(Obj, StrObj),
    sub_prefixes(StrObj, SubObj),
    triple(SubSubj, SubPred, SubObj).

% elimina tutte le triple e re-asserta quelle fondamentali

reset :-
    destroy_triples.

reset :-
    destroy_prefixes.

reset :-
    assert(triple('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#range>', '<http://www.w3.org/2000/01/rdf-schema#Class>')),
    assert(triple('<http://www.w3.org/2000/01/rdf-schema#subClassOf>', '<http://www.w3.org/2000/01/rdf-schema#range>', '<http://www.w3.org/2000/01/rdf-schema#Class>')),
    assert(triple('<http://www.w3.org/2000/01/rdf-schema#subClassOf>', '<http://www.w3.org/2000/01/rdf-schema#domain>', '<http://www.w3.org/2000/01/rdf-schema#Class>')),
    assert(triple('<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', '<http://www.w3.org/2000/01/rdf-schema#range>', '<http://www.w3.org/2002/07/owl#ObjectProperty>')),
    assert(triple('<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', '<http://www.w3.org/2000/01/rdf-schema#domain>', '<http://www.w3.org/2002/07/owl#ObjectProperty>')),
    assert(triple('<http://www.w3.org/2000/01/rdf-schema#Class>', '<http://www.w3.org/2002/07/owl#sameAs>', '<http://www.w3.org/2002/07/owl#Class>')), !.


% ---------------------------esempio di dataset-----------------------


% Definizioni di classi
triple(bird, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>').
triple(canary, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>').
triple(cat, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#Class>').

% Equivalenza di classi
triple(canary, '<http://www.w3.org/2002/07/owl#equivalentClass>', bird).

% Disgiunzione di classi
triple(bird, '<http://www.w3.org/2002/07/owl#disjointWith>', cat).

% Istanze
triple(tweety, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', canary).
triple(sylvester, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', cat).
% triple(tweety, rdf:type, bird). Questa triple √® coerente con
% l'equivalenza ma non √® presente perche sono scemo e l'avrei dovuta
% mettere
triple(felix, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', bird).
triple(felix, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', canary).
triple(felix, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', cat). % Questa triple crea un'inconsistenza con la disgiunzione



%Triple della base di conoscienza, da non toccare plz <3
triple('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>', '<http://www.w3.org/2000/01/rdf-schema#range>', '<http://www.w3.org/2000/01/rdf-schema#Class>').
triple('<http://www.w3.org/2000/01/rdf-schema#subClassOf>', '<http://www.w3.org/2000/01/rdf-schema#range>', '<http://www.w3.org/2000/01/rdf-schema#Class>').
triple('<http://www.w3.org/2000/01/rdf-schema#subClassOf>', '<http://www.w3.org/2000/01/rdf-schema#domain>', '<http://www.w3.org/2000/01/rdf-schema#Class>').
triple('<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', '<http://www.w3.org/2000/01/rdf-schema#range>', '<http://www.w3.org/2002/07/owl#ObjectProperty>').
triple('<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>', '<http://www.w3.org/2000/01/rdf-schema#domain>', '<http://www.w3.org/2002/07/owl#ObjectProperty>').
triple('<http://www.w3.org/2000/01/rdf-schema#Class>', '<http://www.w3.org/2002/07/owl#equivalentClass>', '<http://www.w3.org/2002/07/owl#Class>').
