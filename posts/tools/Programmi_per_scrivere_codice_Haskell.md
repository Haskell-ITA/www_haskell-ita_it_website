---
title: Editors e IDE per Haskell
tags: knowledge-base, tools
date: 2015-01-01
---

## Overview

Al momento (2015-10-07) non esiste un editor/IDE per Haskell che sia semplice da installare, completa come features, ma esente da problemi nel passaggio da Cabal puro a Stack.

Le soluzioni semplici da installare, non hanno features più o meno indispensabili quando si inizia a fare sul serio, e le soluzioni avanzate non sono semplici da configurare, dato che usano diversi tools e al minimo problema, bisogna "aprire il cofano". Inoltre Stack è uscito da poco e le IDE non si sono ancora completamente aggiornate.

Quindi se stai imparando Haskell, il consiglio è quello di iniziare con un editor semplice, dato che eviterai di perdere più tempo a configurare l'ambiente di sviluppo che ad imparare il linguaggio, e per scrivere i primi programmi in Haskell serve veramente poco come editor, dato che sta tutto in un file. Nel mentre la situazione lato IDE migliorerà.
<!--more-->

## Soluzioni Semplici

### Atom

Può essere scaricato dalla [home page di Atom](https://atom.io)

Per installare le [funzionalità legate ad Haskell](https://atom.io/packages/ide-haskell):

    $ apm install language-haskell haskell-ghc-mod ide-haskell autocomplete-haskell

Il programma più usato per scrivere codice Haskell è Emacs. Per chi comincia con Haskell, Emacs può essere ostico.
In questo caso si consiglia di cominciare con Atom o SublimeText e passare ad Emacs in un secondo momento.

### Il tuo editor preferito + Haskell

Puoi usare un qualsiasi editor (`vi`, `gedit`, `nano`, `notepad++`, etc.) più
[ghcid](https://github.com/ndmitchell/ghcid#readme), un demone per fare il
typecheck del vostro progetto ad ogni salvataggio, e sarai avvisato se hai commesso errori.

Se avete seguito la guida di installazione di `stack` proposta, potete installare `ghcid` con
il comando `stack install ghcid`

### Vim

Vim non è un editor semplice, ma se già lo usi e conosci, allora usarlo anche per Haskell è sicuramente semplicissimo.

Vim contiene già un modulo per la *syntax highlight* dei file `.hs`. Per rendere
il lavoro con i `<tab>` più adatto al linguaggio Haskell, aggiungete questo al
vostro `.vimrc` (dall'utente #haskell merijin)

    " Tab specific option
    set tabstop=8                   "A tab is 8 spaces
    set expandtab                   "Always uses spaces instead of tabs
    set softtabstop=4               "Insert 4 spaces when tab is pressed
    set shiftwidth=4                "An indent is 4 spaces
    set shiftround                  "Round indent to nearest shiftwidth multiple

Se cercate una IDE Haskell che usa Vim come editor, ci sono [haskell-vim-now](https://github.com/begriffs/haskell-vim-now) e
[ghcmod-vim](https://github.com/eagletmt/ghcmod-vim).

## Soluzioni complete ma complesse

### Emacs

Ci sono numerosi plug-in per Emacs che lo fanno diventare una IDE completa per programmare in Haskell.

Emacs è difficile da configurare ed è difficile imparare ad usarlo. Quindi se sei interessato il consiglio è di consultare in giro per il web. Per esempio https://github.com/chrisdone/emacs-haskell-config 

### Spacemacs 

Se vuoi imparare ad usare Emacs, ti consigliamo vivamente di abbandonare l'idea :-), e di iniziare fin da subito con la sua variante (cugino) [Spacemacs](https://github.com/syl20bnr/spacemacs), dato che:
* mostra sempre un menu progressivo del comando che puoi attivare dopo, e permette di non dover usare solo la memoria, ma di essere guidati passo passo. Per esempio SPC (spazio) per attivare il menu dei comandi, poi "w" per attivare il menu delle finestre e "m" per ingigantire la finestra corrente. Ad ogni passo ti presenta i comandi disponibili
* le configurazioni di default di ogni layer (Haskell, org-mode, PHP, etc..) sono ad un livello di completezza e praticita\` quasi perfetto, mentre in Emacs uno deve raccogliere trucchi a destra e manca e migliorare il file di configurazione settimana dopo settimana
* usa i keybinding di Vim (ma può anche essere configurato in modo diverso) che alla lunga sono più comodi e potenti (parere personale/opinabile, ma questa è la mia esperienza dopo aver usato per tanti anni Emacs e stancato di fare accordi alla tastiera, puntando tutto sulla memoria da pianista)

#### Spacemacs con Stack

Stack è uscito da poco, quindi serve un minimo di impegno per farlo lavorare con Spacemacs:

1) Installate spacemacs, come scritto nella loro ottima documentazione.

2) Aggiungete al vostro `PATH` la cartella che contiene l'eseguibile `ghc`; io ho messo nel mio `.bashrc`:

    PATH=~/.local/bin:~/.stack/programs/x86_64-linux/ghc-7.10.2/bin:$PATH
    
3) Modificare il file `~/.stack/global/stack.yaml` facendolo diventare:

    flags: {}
    packages: []
    extra-deps:
    - cabal-helper-0.6.1.0
    resolver: lts-3.8

4) `stack install stylish-haskell hasktags hlint ghc-mod hindent`

5) Attivate il layer haskell nel vostro `~/.spacemacs` in maniera che sia come:

    (haskell :variables
             haskell-enable-hindent-style "gibiansky"
             haskell-process-type 'stack-ghci
             haskell-tags-on-save nil
             haskell-process-log t
             haskell-process-suggest-haskell-docs-imports nil)

(per capire cosa si intende con layer e come aggiungerli, leggete la [guida di spacemacs](https://github.com/syl20bnr/spacemacs)
