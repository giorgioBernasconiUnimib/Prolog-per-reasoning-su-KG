:- dynamic triple/3.

% Verifica se un'entità è una classe
is_class(Entity) :-
    triple(Entity, rdf:type, owl:'Class').

% Verifica se un'entità è una proprietà
is_property(Entity) :-
    triple(Entity, rdf:type, owl:'ObjectProperty').

% Verifica se un'entità è un'istanza di una classe
is_instance(Entity, Class) :-
    triple(Entity, rdf:type, Class),
    is_class(Class).

% Inferenza di relazioni basate sulle proprietà definite
infer_relations :-
    findall(_, (triple(S, P, O), infer_relation(S, P, O)), _).

%Viene ripetuta per tutte le triple grazie a infer_relations/0
infer_relation(S, P, O) :-
    is_property(P),
    triple(P, rdfs:domain, Domain),
    triple(P, rdfs:range, Range),
    (   \+ triple(S, rdf:type, Domain) -> assert(triple(S, rdf:type, Domain)); true),
    (   \+ triple(O, rdf:type, Range) -> assert(triple(O, rdf:type, Range)); true).

% Inferenza basata sulle sottoclassi
infer_subclass_relations :-
    findall(_, (triple(Entity, rdf:type, SubClass), infer_subclass_relation(Entity, SubClass)), _).

%Viene ripetuta per tutte le triple grazie a infer_subclass_relations/0
infer_subclass_relation(Entity, SubClass) :-
    triple(SubClass, rdfs:subClassOf, Class),
    \+ triple(Entity, rdf:type, Class),
    assert(triple(Entity, rdf:type, Class)).

% Inferenza basata sulle sotto proprietà
infer_subproperty_relations :-
    findall(_, (triple(S, SubP, O), infer_subproperty_relation(S, SubP, O)), _).

% Viene ripetuta per tutte le triple grazie a infer_subproperty_relations/0
infer_subproperty_relation(S, SubP, O) :-
    triple(SubP, rdfs:subPropertyOf, P),
    \+ triple(S, P, O),
    assert(triple(S, P, O)).

% Applicazione generale delle inferenze
apply_inferences :-
    infer_relations,
    infer_subclass_relations,
    infer_subproperty_relations.

% Stampa tutte le triple per verifica
print_all_triples :-
    forall(triple(S, P, O), format("~w ~w ~w.\n", [S, P, O])).

% Avvia l'esecuzione delle inferenze e stampa le triple risultanti
start_execution :-
    apply_inferences,  % Applica le inferenze basate su classi, proprietà, sottoclassi, ecc.
    print_all_triples. % Stampa tutte le triple nella base di conoscenza




































% Classi
triple(creature, rdf:type, owl:'Class').
triple(animal, rdf:type, owl:'Class').
triple(animal, rdfs:subClassOf, creature).
triple(person, rdf:type, owl:'Class').
triple(person, rdfs:subClassOf, creature).
triple(employee, rdf:type, owl:'Class').
triple(employee, rdfs:subClassOf, person).

% Proprietà
triple(hasPet, rdf:type, owl:'ObjectProperty').
triple(hasPet, rdfs:domain, person).
triple(hasPet, rdfs:range, animal).
triple(worksFor, rdf:type, owl:'ObjectProperty').
triple(worksFor, rdfs:domain, employee).
triple(worksFor, rdfs:range, company).

% Sotto Proprietà
triple(manages, rdf:type, owl:'ObjectProperty').
triple(manages, rdfs:subPropertyOf, worksFor).
triple(manages, rdfs:domain, manager).
triple(manages, rdfs:range, company).

% Sottoclassi
triple(manager, rdf:type, owl:'Class').
triple(manager, rdfs:subClassOf, employee).
triple(company, rdf:type, owl:'Class').

% Istanze
triple(john, rdf:type, person).
triple(rosie, rdf:type, animal).
triple(john, hasPet, rosie).
triple(acmeCorp, rdf:type, company).
triple(jane, rdf:type, manager).
triple(jane, manages, acmeCorp).

% Ulteriori Istanze per Inferenze
triple(spot, rdf:type, animal).
triple(john, worksFor, acmeCorp).
triple(acmeCorp, rdfs:label, "ACME Corporation").

















































