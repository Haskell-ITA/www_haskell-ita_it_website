---
title: Haskell-Day Autunnale a Bologna (2016-11-19)
date: 2016-11-19
author: community
tags: events
---

In un’uggiosa giornata autunnale, venti intrepidi Haskellers provenienti da varie città italiane si sono incontrati a Bologna, ospitati dall’associazione [Kilowatt](http://kilowatt.bo.it/), nella particolare e suggestiva ["Gabbia del Leone"](http://leserre.kilowatt.bo.it/gabbia-del-leone/), situata nel cuore dei [Giardini Margherita](http://leserre.kilowatt.bo.it/).

<a href="/images/photos/meetup_2016_11_19_gruppo.jpg"><img src="/images/photos/meetup_2016_11_19_gruppo.jpg" alt="photo" class="img-thumbnail"></a>

Seguendo un approccio ormai collaudato, abbiamo formato piccoli gruppi di lavoro, in modo che a ognuno fosse possibile partecipare, in maniera attiva e alla pari, ad uno o più progetti di proprio gradimento - il tutto in maniera estremamente libera e amichevole. 

### Nuovi Arrivati
Un gruppo di volenterosi newbie si è cimentato su esercizi, presi dall’ottimo [Haskell Programming from first principles](http://haskellbook.com/). Come spesso accade, gli esercizi si sono rivelati tutt’altro che semplici - e un confronto con persone più esperte è stato decisamente utile. Ecco un [resoconto di quanto imparato](http://another-ticket-in-the-wall.blogspot.it/2016/11/combinatorial-problems-in-haskell.html). 

<a href="/images/photos/meetup_2016_11_19_newbie.jpg"><img src="/images/photos/meetup_2016_11_19_newbie.jpg" alt="photo" class="img-thumbnail"></a>

### Finger Trees
Paol(in)o ha spiegato l'utilizzo dei [FingerTree](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html): una struttura dati utilizzata per modellare sequenze - che sono anche alla base del modulo `Data.Sequence`.
Il formato della presentazione è stato molto gradito: circa cinque ascoltatori si sono riuniti intorno ad una lavagna, per delineare differenti ipotesi risolutive. L’interattività della spiegazione ha permesso a tutti di capire meglio l'argomento trattato.

### Web Development in Haskell
Si è organizzato un piccolo simposio per discutere della situazione del web-development in Haskell. Abbiamo considerato principalmente le differenze tra l'approccio FRP (reflex) e quello basato su react (react-flux), volgendo particolare attenzione alle differenze di struttura generale e alla componibilità delle strutture create.

<a href="/images/photos/meetup_2016_11_19_experts.jpg"><img src="/images/photos/meetup_2016_11_19_experts.jpg" alt="photo" class="img-thumbnail"></a>

### Fuga dagli alieni dallo spazio profondo
Abbiamo esplorato i problemi di concorrenza che si possono presentare in un’applicazione di rete, prendendo a pretesto un prototipo di gioco multiplayer online che permettesse ai client di accedere ad una scacchiera condivisa applicando una semplice regola: due utenti non possono stare nella stessa cella.
Per fare ciò, abbiamo introdotto un’architettura client/server mediante [websocket](https://hackage.haskell.org/package/websockets) e uno stato condiviso sul server utilizzando [MVar](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Concurrent-MVar.html ). 
Dopodiché, abbiamo simulato una race condition - introducendo un tempo di attesa tra il primo controllo di disponibilità della cella e l’effettiva modifica: a questo punto, due client che entravano nella stessa cella contemporaneamente potevano farlo!
Sostituendo MVar con TVar - tratto da [STM] (https://hackage.haskell.org/package/stm) - abbiamo ottenuto il controllo della transazione e la possibilità di individuare modifiche esterne allo stato - che abbiamo deciso di risolvere mantenendo il secondo client fermo nella posizione precedente.

### Programmazione Message-Oriented
Un gruppo di persone ha svolto esercizi utilizzando la libreria [Quid2](https://github.com/tittoassini/quid2), sotto la guida del suo autore: Titto.

### Web client e monadi
Ponendosi come obiettivo la realizzazione di un semplice client per le web API di [Random.org](https://www.random.org/), un gruppetto di nuovi arrivati ha avuto modo di sperimentare l’I/O in Haskell e - coadiuvati da sviluppatori più esperti - di svolgere un primo approfondimento sulla teoria delle monadi.

### Community
Il pranzo insieme, i momenti di retrospective durante il meetup, gli scambi di consulenza fra i diversi gruppi e una gita serale/notturna nel centro di Bologna con degustazione di tigelle hanno permesso a tutti di conoscersi.

Ringraziamo gli autori di [Haskell Programming from first principles](http://haskellbook.com/) per averci messo a disposizione alcune copie del libro, regalate poi a cinque fortunati Haskellers, e l’associazione [Kilowatt](http://kilowatt.bo.it/), per averci ospitato.

