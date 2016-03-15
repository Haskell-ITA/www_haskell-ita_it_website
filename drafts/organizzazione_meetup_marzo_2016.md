## Haskell Day Firenze

Questa pagina serve per collaborare nella stesura del programma dettagliato. Siete tutti liberi di dire la vostra attraverso merge requests, interventi in mailing-list o IRC.

### Programma 

Questo è il terzo evento del gruppo dei programmatori Haskell Italiani. A differenza dei due precedenti, vogliamo concentrarci un po' più sull'aspetto pratico, quindi ci saranno un sacco di occasioni di sperimentare la programmazione funzionale pura direttamente sul proprio portatile. La proposta è di fare dei gruppetti di lavoro ristretti (2-6 persone, ideale 3-4) per lavorare su qualcosa di concreto, approfondire la conoscenza del linguaggio attraverso il confronto e divertirsi un po'. 

È importante avere già l'ambiente preparato seguendo questa guida [http://haskell-ita.it/2015/01/Installare_Haskell/](http://haskell-ita.it/2015/01/Installare_Haskell/).

#### Talk

TODO inserire dettagli 

#### Coding with Mentor 

Invece di avere un esercizio/progetto comune, si è pensato di fare gruppi con progetti differenti e un mentor che propone lui il tema e fa un pó da guida. 

Portate quindi il portatile, con un ambiente Haskell. Il fine è puramente didattico, quindi nessuna ansia da prestazione legata al codice finale prodotto.:-)

Di seguito ci sono le proposte, ma se ne possono avanzare altre il giorno stesso. Il giorno del meetup sceglieremo quali progetti fare (non e\` detto si facciano tutti), e i relativi gruppi di lavoro.

##### Capire il codice di Stack e provare a risolvere un issue su GitHub 

Mentor: Luca Molteni 

riporto il messaggio di Luca in mailing-list:

Penso che durante il prossimo incontro oltre a qualche talk potremmo organizzare un gruppo per contribuire a un progetto open source scritto in Haskell, in modo da dare effettivamente qualcosa indietro alla comunità e programmare su una code base matura.

Personalmente mi propongo per fare mentoring e seguire un gruppo che si occuperà di provare a contribuire qualche cosa su Stack, prendendo dalla lista di issue aperti per neofiti di GitHub.

https://github.com/commercialhaskell/stack/issues?q=is%3Aopen+is%3Aissue+label%3Anewcomer

Obiettivo: una pull request. Ce la faremo? Chissà.

##### Functional Reactive Programming applicato ad una arena di Robot

Mentor: Massimo Zaniboni

La proposta e` guardare insieme Functional Reactive Programming, in particolare NetWire https://wiki.haskell.org/Netwire, che è una FRP arrow-based, e completare degli esempi di Robot specificati usando https://github.com/massimo-zaniboni/hrobots

##### Libreria Haskell per la scambio di messaggi

Mentor: Titto

Sto lavorando ad un semplice sistema di scambio dati. E' una sorta di twitter ma in cui i messaggi sono valori di tipi ben definiti.

Molto brevemente, il sistema consente a qualsiasi utente di:

* definire uno o piu' tipi dati e registrarli presso il sistema.
* inviare valori di uno qualunque dei tipi registrati.
* ricevere tutti i valori di un certo tipo (opzionalmente anche filtrandoli sulla base del valore di alcuni campi)

E' una struttura semplice ma molto flessibile.

Supponiamo ad esempio che uno voglia implementare una chat intelligente, tipo Haskel lRC, da usare magari proprio durante il nostro incontro.

Un semplice modello dati potrebbe essere qualcosa cosa come:

    -- Protocol for a simple chat system
    data Chat = ChatEntry {
                       fromUser::String
                      ,subject::Subject
                      ,message::Message}

    -- Hierarchical subject
    -- Example: Subject ["Haskell","Meeting","Firenze","Marzo 2016","Stack Development"]
    data Subject = Subject [String] 

    -- Different kind of messages
    data Message = TextMessage String
                 | HaskellSearch String
                 | HaskellExpression String


L'esercizio consiste nel:

* creare un modello e registrarlo 
* creare una semplice interfaccia utente per creare messaggi e per visualizzarli
* creare qualche (ro)bot che quando vede un valore di un tipo particolare reagisce in maniera intelligente. Ad esempio se si tratta di una  HaskellExpression, la valuta e ritorna il risultato o quando vede una HaskellSearch fa la ricerca su Hoogle e ritorna il risultato.

Credo che un esercizio cosi' dovrebbe fornire materiale sufficiente tanto per utenti nuovi che per quelli più smaliziati.

Dopo aver creato il modello tutti insieme, dato che le varie funzionalita' di input/output ed elaborazione sono indipendenti una dall'altra, ognuno può scegliere il pezzo che preferisce, più o meno difficile, e lavorare in parallelo.
.

##### Opalaye - Libreria per l'accesso ai database relazionali

Mentor: Ruggero D'Alò (Arguggi)

Opalaye è una libreria Haskell per l'accesso ai database relazionali. Quello che lo rende interessante è che:

* obbliga il programmatore a scrivere query SQL corrette, tenendo conto anche dello schema del database, e non solo della sintassi
* utilizza numerose estensioni di Haskell, motivandone l'utilità

L'esercizio consiste nello studio della libreria e delle estensioni Haskell, seguendo http://ren.zone/articles/opaleye-sot e nella scrittura di alcuni programmi di DEMO.

##### Esercizi semplici per chi è all'inizio

Problema: ci saranno al meetup anche alcune persone che non conoscono Haskell o sono ai primissimi passi. In questo caso partire da progetti più o meno reali, rischia di essere troppo difficile. Ci vorrebbe qualcuno disposto a seguire chi è all'inizio e fare magari qualche esercizio canonico che si trova nel WEB

##### Proposte varie in cerca di un Mentor

###### Lenses

Studiare una libreria gia' esistente, magari una libreria di uso generale teoricamente ricca ma un po' ostica come "lens", sarebbe un ottimo esercizio.


##### Altre proposte

TODO Sentitevi liberi di proporre quello che preferite, qua o in Mailing-List

## TODO Vari

TODO preparare locandina evento, da dare Pasqualino Assini da distribuire nelle bacheche delle università e simili
