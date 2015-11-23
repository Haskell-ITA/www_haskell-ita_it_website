---
title: Meetup Bologna (Autunno 2015)
date: 2015-10-17
tags: meetups
---
Ci siamo trovati a Bologna, Sabato 17 Ottobre, presso [T3Lab](http://www.t3lab.it), e sponsorizzati da [BioDec](http://www.biodec.com/).

<img src="/images/photos/meetup_2015_autunno.jpg" alt="photo" class="img-thumbnail">

<!--more-->

## Talks

### Haskell Intro

Relatore: Luca Molteni

Argomento: introduzione alla programmazione funzionale e Haskell.

[Slides](https://www.slideshare.net/volothamp/introduction-to-haskell-54056240)

[Filmato](https://t.co/FF09fZx9mp)

### Monadi e superpoteri

Relatore: Carlo Nucera

Argomento:

Questo talk è pensato come un aperitivo per fare intuire alle persone che
conoscono poco il linguaggio quali sono motivazioni e vantaggi che derivano
dall'uso delle monadi in Haskell.

Pur non spiegando approfonditamente i concetti di monade o di monad transformer,
guarderemo una carrellata di esempi scelti (tra gli altri, Maybe, Memoizzazione,
e Stato+NonDeterminismo) facendo vedere come consentano di scrivere del codice
semplice che si prende cura in modo trasparente dei contesti computazionali.

TODO inserire link alle slides

TODO inserire link al filmato

### Effetti modulari ed estendibili

Relatore: Matteo Acerbi

Argomento:

Il linguaggio Haskell obbliga il programmatore ad esplicitare quali
sono i side-effect compiuti dai propri programmi. In particolare, la
presenza di effetti in una definizione di funzione influenza il suo
*tipo*.

Ad esempio, la funzione `readFile` che prende il percorso di un file e
ritorna la stringa contenuta da esso ha tipo

    readFile :: FilePath -> IO String

e non

    readFile :: FilePath -> String

Infatti, `readFile` deve interagire con il sistema operativo per
ottenere la stringa corrispondente al contenuto del file: in Haskell,
i programmi che richiedono queste interazioni sono "confinati" nella
monade `IO`.

Se le funzioni compiono molteplici effetti (e.g.: `IO`, `State Int`,
`Maybe`, `List`, ...), nasce il problema di gestirne la composizione.

Nella presentazione sono trattati diversi approcci, con un interesse
specifico relativo a proprietà di modularità ed estensibilità.

Slides: Presto!

## Pair Programming

Responsabile: Massimo Zaniboni

Esercizio: robots che si sfidano in un'arena virtuale. Codice scritto usando Functional Reactive Programming

[Project Website](https://github.com/massimo-zaniboni/hrobots)

[Commenti sul framework FRP](https://groups.google.com/forum/#!topic/haskell_ita/crQqWPZkkac)


