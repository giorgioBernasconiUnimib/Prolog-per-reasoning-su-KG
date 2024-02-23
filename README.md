Progetto per "Introduzione all'intelligenza artificiale"
=======





### 1. Il progetto parte dalla lettura del dataset (di cui forniremo un esempio). Esso viene letto da un parser di turtle, che implementeremo in prolog, traducendolo in formato prolog.
1. Verrà verificata la corretta formattazione (es subject with IRI or Blank node ecc…). In caso contrario il programma segnalerà un Syntax error.

### 2. Analisi delle triple e successiva inferenza per distinguere classi e relazioni e proprietà.

### 3. Analisi delle subclass, subproperties, domain/range, quindi utilizzo di inferenza per la derivazione delle nuove triple.


## Per quanto riguarda l’utilizzo di OWL abbiamo strutturato alcune idee per altri punti da analizzare dopo aver finito i precedenti. 

### 1. Gestione della relazioni sulle classi (equivalenza e disgiunzione) per il consistency Check.

### 2. Gestione relazioni sugli individui (uguaglianza e differenza).

### 3. Gestione di almeno una parte delle relazioni tra proprietà.

### 4. Gestione della gerarchia di classi e proprietà.

Per far avviare il file
======

### 1. Aprire il file OwLogics.pl

### 2. Se si vuole ispezionare il dataset di esempio persente nel programma, scrivere il comando start_execution., altrimenti
 1. utilizzare read_file., read_abs_file(Filename). oppure read_rel_file(Filename) per leggere il file (nel caso di read_file sarà letto il file input.ttl presente)
 2. utilizzare start_execution. per effettuare inferenze.

### 3. Se si vuole, utilizzare select(Subj, Pred, Obj). per effettuare query.

Note
======

### Si noti che nel caso si desse il filename assoluto, sarà necessario cambiare i '\' nel path con '/'

### Nel file turtle, sarà necessario seguire le seguenti regole

#### 1. le triple devono essere nella forma Subj Pred Obj .
#### 2. i commenti devono iniziare all'inizio della riga, con un #
#### 3. se si hanno più predicati per uno stesso soggetto, separarli con ; nella forma Subj Pred1 Obj1 ;
#### Pred2 Obj2 . (Si noti che nella nuova riga si possono usare solo '\t')
#### 4. se si hanno più oggetti, separarli tramite una ',' nella forma Subj Pred Obj1, Obj2 .
