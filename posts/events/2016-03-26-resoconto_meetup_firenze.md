---
title: Meetup Firenze (Primavera 2016)
date: 2016-03-26
author: community
tags: meetups
---

Ci siamo trovati Sabato 26 Marzo, a Calenzano, in provincia di Firenze, ospitati da [Develer](https://www.develer.com/). 

<a href="/images/photos/meetup_2016-03-26_full.jpg"><img src="/images/photos/meetup_2016-03-26.jpg" alt="photo" class="img-thumbnail"></a>

Partecipanti, fila in piedi (a partire da sinistra): Luca Benci, Alberto, Daniele D'Orazio, Carlo Nucera, Nicola Bonelli, Francesco Ariis, Matteo Baglini, Salvatore Veneziano, Marco Santamaria, Emiliano Anichini.

In ginocchio: Pasqualino Assini (Titto), Massimo Zaniboni, Ruggero D’Alò, Vitalij Zadneprovskij, Nadir Sampaoli, Aurelien Rainone, Francesco Gazzetta. 

Dietro, a fare la foto: Luca Molteni :-)

Ritrovo alle 10:00, con la sede di Develer pronta a ospitare, nonostante sia il Sabato prima di Pasqua, il nostro terzo meetup: diciotto programmatori appassionati di Haskell, provenienti da tutta Italia, e pronti a condividere una giornata di programmazione funzionale e divertimento.

Dopo un breve sondaggio sulla preparazione tecnica dei presenti, sono stati proposti i seguenti progetti, su cui lavorare a gruppi:

* Twitter di Titto (meno "poeticamente" una libreria per lo scambio di messaggi tipizzati)
* Studio dell'articolo di Renzo Carbonara su Opaleye-SOT, che è una libreria Haskell che fa uso di numerose estensioni per supportare query relazionali usando le Arrows in Haskell 
* Contribuire alla soluzione di un issue per Stack
* Kata di Base
* HRobots

<!--more-->

## Twitter di Titto (mentore: Titto)

Questo è stato certamente il progetto più popolare, con nove persone che ci hanno lavorato sopra.

Sono stati scritti quattro bot che si sono interfacciati con l'infrastruttura di Titto, per scambiarsi messaggi in chat.

Grazie all'ottimo tutoraggio di Titto, anche le persone che non avevano un'ampia conoscenza di Haskell sono riusciti a creare qualcosa.

## Opaleye-SOT (mentore: Ruggero)

L'articolo è molto interessante, dato che descrive nei dettagli come Opaleye SOT estenda il type system di Haskell, usando equazioni sui tipi, al fine di verificare a compile-time, che le query, scritte usando le Arrow di Haskell, rispettino la struttura logica dello schema del database sottostante.

È stata un'ottima occasione per imparare diverse estensioni di GHC, e capire quale possa essere il loro utilizzo pratico.

## Contribuiamo a Stack (mentore: Luca Molteni)

In quattro proviamo a risolvere questo semplice issue [1369](https://github.com/commercialhaskell/stack/issues/1369) su GitHub, relativo al progetto OSS Stack. 

Riusciamo a creare una implementazione parziale per le 13:00. Dopo pranzo creiamo la pull request e ci rendiamo conto che abbiamo rotto il comportamento precedente, quindi lavoriamo tutto il pomeriggio nel cercare di esprimere in maniera corretta l'opzione da console con un valore di default, ma non ci riusciamo causa crisi da carboidrato.

Cosa abbiamo imparato:

* i tempi di compilazione di Haskell sono veramente lunghi soprattutto con progetti di queste dimensioni
* anche in Haskell si possono fare "pasticci" nel design del codice, come ad esempio ritrovarsi a dover modificare tre file, in cinque punti diversi, per aggiungere una semplice opzione da command line

## Lens (mentore: Carlo)

Carlo ha mostrato e spiegato un'anteprima dei suoi articoli sulle Lens che saranno pubblicati sul blog di Haskell-ITA.

## Considerazioni Finali

Intanto un doveroso plauso ai programmatori di Develer che oltre a fare gli onori di casa, si sono cimentati con il linguaggio Haskell, mostrando una curiosità e una passione veramente a tutto campo per quel che riguarda il loro lavoro.

In questo terzo meetup abbiamo deciso di privilegiare il lavoro in gruppo e la programmazione, rispetto ai talk. La maggior parte delle persone è riuscita a partecipare al lavoro di due gruppi durante il corso della giornata: uno principale e poi nella parte finale del pomeriggio un progetto minore come un corso sulle Lens, o degli esercizi Kata. Nelle pause fra una sessione di lavoro e un'altra c'è stato modo di chiaccherare e confrontarsi anche fra persone di gruppi differenti.

L'impressione è che ci sia divertiti e non poco, e che si sia creato un buon spirito di gruppo/community, complice anche l'accogliente sala con ampie vetrate e vista sulle montagne. 

