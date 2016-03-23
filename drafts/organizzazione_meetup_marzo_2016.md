## Haskell Day Firenze

Questa pagina serve per collaborare nella stesura del programma dettagliato. Siete tutti liberi di dire la vostra attraverso merge requests, interventi in mailing-list o IRC.

### Programma 

Questo è il terzo evento del gruppo dei programmatori Haskell Italiani. A differenza dei due precedenti, vogliamo concentrarci un po' più sull'aspetto pratico, quindi ci saranno un sacco di occasioni di sperimentare la programmazione funzionale pura direttamente sul proprio portatile. La proposta è di fare dei gruppetti di lavoro ristretti (2-6 persone, ideale 3-4) per lavorare su qualcosa di concreto, approfondire la conoscenza del linguaggio attraverso il confronto e divertirsi un po'. 

È importante avere già l'ambiente preparato seguendo questa guida [http://haskell-ita.it/2015/01/Installare_Haskell/](http://haskell-ita.it/2015/01/Installare_Haskell/).

#### Coding with Mentor 

Si formeranno gruppi con progetti differenti e un mentor che propone lui il tema e fa un pó da guida. Sono previsti anche gruppi per chi conosce ancora poco il linguaggio e intende fare esercizi didattici insieme ad altri.

Il fine è puramente didattico, quindi nessuna ansia legata al codice finale prodotto. Venite possibilmente con il portatile, e con un ambiente Haskell installato. 

L'idea è questa:

* il lavoro a gruppi permette sia di divertirsi e conoscersi, rompendo il ghiaccio rispetto ad un talk tradizionale, che di scoprire come lavorano altre persone e quindi confrontarsi
* le frequenti pause permettono a tutti di conoscere persone di altri gruppi e confrontare il lavoro fatto
* è possibile cambiare gruppo durante il corso della giornata, o dare "consulenze" nel caso ci siano problemi di cui si sa la soluzione

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

##### Semplici Sistemi distribuiti in Haskell.

Mentor: Pasqualino "Titto" Assini

Descrizione: Introduzione alla programmazione distribuita.

Livello richiesto: Da principiante ad esperto.

Il sistema che useremo è un sistema di messaggeria, in cui i messaggi sono valori di tipi definiti in Haskell.

Molto brevemente, il sistema consente di:

* definire uno o piu' tipi dati in Haskell
* inviare valori di uno qualunque dei tipi definiti
* ricevere tutti i valori di un certo tipo

E' una struttura minimalista ma molto flessibile, una sorta di grado zero dei sistemi distribuiti.

Ulteriori informazioni e le istruzioni di installazione del codice (da installare **prima** di arrivare al meeting) sono disponibili su [github](https://github.com/tittoassini/HaskellMeetingFirenze2016).

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

#### Talk

Per il momento non sono previsti talk classici, dato che per questo meetup si vuole puntare di più sul programmare insieme. La parte di "talk" è in teoria sostituita dalle frequenti pause in cui le varie persone possono commentare il loro lavoro e vedere il lavoro degli altri, scambiandosi informazioni più in stile "fiera", che talk con palco. 

Se però qualcuno vuole preparare ugualmente un talk su un argomento di cui ha esperienza e che vuole condividere con altri, può tranquillamente proporlo. Di fatto l'organizzazione è libera e se un'idea piace e viene votata da un pó di persone, non c'è problema.

