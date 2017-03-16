---
title: Haskell-Day Inverno a Milano (2017-02-25)
date: 2017-03-15
author: community
tags: meetups
---

Per questo meeting ci siamo ritrovati a Milano in
[Venini 42](http://venini42.it/), gentilmente ospitati da
[Mikamai](http://www.mikamai.com/) e [LinkMe](http://linkme.it/).

<a href="/images/photos/meetup_2017_02_25_gruppo.jpg"><img src="/images/photos/meetup_2017_02_25_gruppo.jpg" alt="photo" class="img-thumbnail"></a>

# I progetti

### Beginners

Il gruppo di chi ha deciso di avvicinarsi al linguaggio è stato seguito da
Luca, procedendo lungo la traccia della presentazione "[Introduzione ad
Haskell](https://www.slideshare.net/volothamp/introduction-to-haskell-54056240)".
Durante la giornata le nuove leve hanno svolto esercitazioni sulla definizione
di nuovi tipi, implementato Type Class e intuito così la definizione di Monade.

### Haskell Internals

Titto ha illustrato la problematica dell'ottimizzazione e dell'efficienza in
Haskell e gli strumenti per risolvere questo problema. Partendo da un esempio
concreto (una serializzazione di dati sfruttando i generics), il gruppo ha
visto come dare indicazioni efficaci al compilatore e come varia l'efficienza
all'aumentare della complessità dei tipi.
Gran parte del tempo è stata impiegata commentando e analizzando Core, il
linguaggio generato dal compilatore come passo intermedio fra il programma
Haskell e il codice binario.

### Reflex

Carlo ha presentato la libreria FRP Reflex, illustrandone i tipi di base,
il paradigma di funzionamento e le potenzialità. Il gruppo ha installato
reflex-dom attraverso nix e svolto un piccolo esempio per vedere Reflex
in azione.

### Web service

Un gruppo di livello intermedio si è focalizzato sulla realizzazione di un
webservice usando il framework [Servant](http://hackage.haskell.org/package/servant).
La API Rest proposta era molto semplice, affinché fosse possibile realizzarne
una implementazione completa nel limitato tempo a disposizione, potendosi
quindi rendere conto della maggior parte degli aspetti tecnici legati a un
lavoro di questo tipo:
definizione delle rotte e dei controller ad esse legati, mantenimento dello
stato del server (per semplicità si è optato per una MVar anziché usare un
database esterno), codifica dell’output del servizio. A metà pomeriggio si
aveva un prototipo funzionante, e i più avventurosi si sono addirittura
cimentati nella scrittura di un frontend con Elm. Il codice prodotto [è
disponibile su github](https://github.com/larsen/dadi).
