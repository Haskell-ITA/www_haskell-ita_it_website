---
title: Installazione ambiente Haskell
tags: knowledge-base, tools
date: 2015-01-01
---

## Installazione

Per agevolare eventuali problemi di installazione e di tooling nel nostro
prossimo incontro, abbiamo deciso di creare tutti un ambiente simile basato su
Stack, il tool recentemente sviluppato da FPComplete che si occupa
dell'installazione del compilatore (ghc) e delle librerie.
<!--more-->

Prima di tutto andate sulla
[pagina di download di stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)
e seguite l'installazione per la vostra piattaforma.

Ecco, finito :P. A questo punto imparate a creare il vostro primo progetto con
stack, e ad usare in generale il tool, leggendo il capitoletto
[hello world example](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md#hello-world-example)
della guida di stack; in questo modo imparerete ad usare i comandi basilari
`stack new`, `stack setup`, `stack build`, `stack exec` e `stack test`.

Per cominciare ad essere operativi con l'haskell, installate il vostro editor
preferito, e assicuratevi di avere attivato il syntax highlighting per l'haskell
(ad esempio il pacchetto `language-haskell` per atom, o `haskell-mode` per
emacs).

## Cosa è meglio sapere prima dell'incontro

È fortemente consigliato aver letto i primi 4-8 capitoli del libro
[Learn You A Haskell For Great Good](http://learnyouahaskell.com/chapters) (potete leggerlo
online), o qualunque altro testo che vi consenta una familiarità con la sintassi
e gli aspetti basilari del linguaggio. Questo vi consentirà di seguire i talk ed
i workshop con più confidenza, traendone quanto più beneficio possibile.

Nel leggere questi capitoli, vi sarà utile invocare l'interprete haskell,
`ghci`. Se avete seguito la precedente guida di installazione, potete farlo con
il comando `stack ghci`. Potete trovare tutte le informazioni che servono su
come usare i vari tool che l'installazione haskell mette a disposizione (ad
esempio `ghc` o `runhaskell`) leggendo il resto della
[guida di stack](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md).
