:- dynamic triple/3.

%Triple della base di conoscienza, da non toccare plz <3
triple(rdf:type, rdfs:range, owl:class).
triple(rdfs:subClassOf, rdfs:range, owl:class).
triple(rdfs:subClassOf, rdfs:domain, owl:class).


% Verifica se un'entit� � una classe
is_class(Entity) :-
    triple(Entity, rdf:type, owl:class).

% Verifica se una classe � una sottoclasse
is_subClass(SubClass,Class) :-
    triple(SubClass, rdfs:subClassOf, Class).

% Verifica se un'entit� � una propriet�
is_property(Entity) :-
    triple(Entity, rdf:type, owl:'ObjectProperty').

% Assicura la bidirezionalit� di una equivalenza tra classi
infer_equivalent_bidirectional(ClassA, ClassB) :-
    ( \+ triple(ClassB, owl:equivalentClass, ClassA) -> assert(triple(ClassB, owl:equivalentClass, ClassA)),
      format("Asserted bidirectional equivalence: ~w owl:equivalentClass ~w.\n", [ClassB, ClassA])
    ; true).

% Assicura la bidirezionalit� di una disgiunzione tra classi
infer_disjoint_bidirectional(ClassA, ClassB) :-
    ( \+ triple(ClassB, owl:disjointWith, ClassA) -> assert(triple(ClassB, owl:disjointWith, ClassA)),
      format("Asserted bidirectional disjointness: ~w owl:disjointWith ~w.\n", [ClassB, ClassA])
    ; true).

% Chiamata per assicurare la bidirezionalit� delle relazioni tra classi
infer_bidirectional_relations :-
    forall(triple(ClassA, owl:equivalentClass, ClassB), infer_equivalent_bidirectional(ClassA, ClassB)),
    forall(triple(ClassA, owl:disjointWith, ClassB), infer_disjoint_bidirectional(ClassA, ClassB)).

% Assicura la bidirezionalit� di una equivalenza (sameAs) tra individui
infer_sameAs_bidirectional(IndividualA, IndividualB) :-
    ( \+ triple(IndividualB, owl:sameAs, IndividualA) ->
        assert(triple(IndividualB, owl:sameAs, IndividualA)),
        format("Asserted bidirectional sameAs: ~w owl:sameAs ~w.\n", [IndividualB, IndividualA])
    ; true).

% Assicura la bidirezionalit� di una disgiunzione (differentFrom) tra individui
infer_differentFrom_bidirectional(IndividualA, IndividualB) :-
    ( \+ triple(IndividualB, owl:differentFrom, IndividualA) ->
        assert(triple(IndividualB, owl:differentFrom, IndividualA)),
        format("Asserted bidirectional differentFrom: ~w owl:differentFrom ~w.\n", [IndividualB, IndividualA])
    ; true).

% Chiamata per assicurare la bidirezionalit� delle relazioni tra individui
infer_individual_bidirectional_relations :-
    forall(triple(IndividualA, owl:sameAs, IndividualB), infer_sameAs_bidirectional(IndividualA, IndividualB)),
    forall(triple(IndividualA, owl:differentFrom, IndividualB), infer_differentFrom_bidirectional(IndividualA, IndividualB)).

% Aggiunge transitivit� alla disgiunzione tra classi
infer_transitive_subClass :-
    forall((triple(ClassA, owl:equivalentClass, ClassB), is_subClass(ClassA,ClassC)),
           ( \+ triple(ClassB, rdfs:subClassOf, ClassC) -> assert(triple(ClassB, rdfs:subClassOf, ClassC)),
             format("Inferred and asserted subClassOf: ~w rdfs:subClassOf ~w.\n", [ClassB, ClassC])
           ; true)).

% Assicura che le triple rispettino le restrizioni di domain e range per le propriet�
apply_domain_and_range_restrictions :-
    forall((triple(S, P, O), is_property(P)), apply_domain_and_range_restrictions(S, P, O)).

% Applica le restrizioni di domain e range per una data propriet�
apply_domain_and_range_restrictions(S, P, O) :-
    % Gestione del domain
    (   triple(P, rdfs:domain, Domain),
        \+ triple(S, rdf:type, Domain) -> assert(triple(S, rdf:type, Domain)),
        format("Inferred and asserted domain for ~w: ~w rdf:type ~w.\n", [P, S, Domain])
    ;   true),
    % Gestione del range
    (   triple(P, rdfs:range, Range),
        \+ triple(O, rdf:type, Range) -> assert(triple(O, rdf:type, Range)),
        format("Inferred and asserted range for ~w: ~w rdf:type ~w.\n", [P, O, Range])
    ;   true).

% Inferenza basata sulle sottoclassi
infer_subclass_relations :-
    findall(_, (triple(Entity, rdf:type, SubClass), infer_subclass_relation(Entity, SubClass)), _).

%Viene ripetuta per tutte le triple grazie a infer_subclass_relations/0
infer_subclass_relation(Entity, SubClass) :-
    is_subClass(SubClass,Class),
    (   \+ triple(Entity, rdf:type, Class) -> assert(triple(Entity, rdf:type, Class))).

% Inferenza basata sulle sotto propriet�
infer_subproperty_relations :-
    findall(_, (triple(S, SubP, O), infer_subproperty_relation(S, SubP, O)), _).

% Viene ripetuta per tutte le triple grazie a infer_subproperty_relations/0
infer_subproperty_relation(S, SubP, O) :-
    triple(SubP, rdfs:subPropertyOf, P),
    (   \+ triple(S, P, O) -> assert(triple(S, P, O))).

% Gestione dell'equivalenza tra classi
infer_equivalent_classes :-
    forall(triple(ClassA, owl:equivalentClass, ClassB),
                (forall(triple(Instance, rdf:type, ClassA),
                       ( \+ triple(Instance, rdf:type, ClassB) -> assert(triple(Instance, rdf:type, ClassB)) ; true)),
                forall(triple(Instance, rdf:type, ClassB),
                       ( \+ triple(Instance, rdf:type, ClassA) -> assert(triple(Instance, rdf:type, ClassA)) ; true)))).

% Aggiunge transitivit� alla disgiunzione tra classi
infer_transitive_disjointness :-
    forall((triple(ClassA, owl:equivalentClass, ClassB), triple(ClassA, owl:disjointWith, ClassC)),
           ( \+ triple(ClassB, owl:disjointWith, ClassC) -> assert(triple(ClassB, owl:disjointWith, ClassC)),
             format("Inferred and asserted disjointness: ~w owl:disjointWith ~w.\n", [ClassB, ClassC])
           ; true)),
    forall((triple(ClassB, owl:equivalentClass, ClassA), triple(ClassA, owl:disjointWith, ClassC)),
           ( \+ triple(ClassB, owl:disjointWith, ClassC) -> assert(triple(ClassB, owl:disjointWith, ClassC)),
             format("Inferred and asserted disjointness: ~w owl:disjointWith ~w.\n", [ClassB, ClassC])
           ; true)).

% Individua le inferenze che, per via della disgunzione tra
% classi, causano una inconsistenza
infer_disjoint_classes :-
    findall(Instance-(ClassA-ClassB),
            (triple(ClassA, owl:disjointWith, ClassB),
             triple(Instance, rdf:type, ClassA),
             triple(Instance, rdf:type, ClassB)),
            Inconsistencies),
    report_and_remove_inconsistencies(Inconsistencies).

% Gestisce la lista "Inconsistencies" rimuovendola dalle triple
% attualmente in considerazione e segnalando il problema
report_and_remove_inconsistencies([]).
report_and_remove_inconsistencies([Instance-(ClassA-ClassB) | Rest]) :-
    format('Inconsistency found: Instance ~w cannot belong to both disjoint classes ~w and ~w.\n', [Instance, ClassA, ClassB]),
    (triple(Instance, rdf:type, ClassA) -> retract(triple(Instance, rdf:type, ClassA)); true),
    (triple(Instance, rdf:type, ClassB) -> retract(triple(Instance, rdf:type, ClassB)); true),
    report_and_remove_inconsistencies(Rest).

%Gestione di uguaglianza tra individui
infer_same_as_relations:-
    forall(triple(IndividualA, owl:sameAs, IndividualB),
                (forall(triple(Instance, rdf:type, IndividualA),
                       ( \+ triple(Instance, rdf:type, IndividualB) -> assert(triple(Instance, rdf:type, IndividualB)) ; true)),
                forall(triple(Instance, rdf:type, IndividualB),
                       ( \+ triple(Instance, rdf:type, IndividualA) -> assert(triple(Instance, rdf:type, IndividualA)) ; true)))).



% Applicazione delle nuove inferenze
apply_inferences :-
    infer_bidirectional_relations,
    apply_domain_and_range_restrictions,
    infer_subclass_relations,
    infer_subproperty_relations,
    (infer_transitive_disjointness,infer_bidirectional_relations),
    infer_disjoint_classes,
    infer_equivalent_classes,
    infer_transitive_subClass,
    infer_same_as_relations.

% Stampa tutte le triple, sia quelle del dataset sia quelle inferite
print_all_triples :-
    forall(triple(S, P, O), format("~w ~w ~w.\n", [S, P, O])).

% Avvia l'esecuzione delle inferenze e stampa le triple risultanti
start_execution :-
    apply_inferences,
    print_all_triples.






% Definizioni di classi
triple(bird, rdf:type, owl:'Class').
triple(canary, rdf:type, owl:'Class').
triple(cat, rdf:type, owl:'Class').

% Equivalenza di classi
triple(canary, owl:equivalentClass, bird).

% Disgiunzione di classi
triple(bird, owl:disjointWith, cat).

% Istanze
triple(tweety, rdf:type, canary).
triple(sylvester, rdf:type, cat).
% triple(tweety, rdf:type, bird). Questa triple � coerente con
% l'equivalenza ma non � presente perche sono scemo e l'avrei dovuta
% mettere
triple(felix, rdf:type, bird).
triple(felix, rdf:type, canary).
triple(felix, rdf:type, cat). % Questa triple crea un'inconsistenza con la disgiunzione











