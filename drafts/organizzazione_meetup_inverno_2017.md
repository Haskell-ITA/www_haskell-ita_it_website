## Haskell Day - XXX, YYY 2017

### Idee Progetti

#### Migliorare Haddock (Massimo Zaniboni)

Miglioramenti "semplici":
* quando un modulo A esporta tutte le funzioni di un modulo B, rendere piu` chiara la cosa
* rendere piu` chiare quali sono le funzioni supportate da un tipo o da un modulo, guardando anche le class supportate. Per esempio List (o Data.List) supporta anche tutte  le funzioni su Traversable. Quindi far vedere sia le funzioni "dirette" di un modulo, che quelle "indirette" supportate grazie ad una "instance SomeClass ThisType"

Lo propongo perche` a me queste due cose mi hanno fatto perdere un po' di tempo e non ho sempre trovato quello che cercavo.

Interazione "community-oriented" (Haddock Overflow :-)
* poter annotare on-line la documentazione di Haskell sia come se fosse documentazione aggiuntiva che tramite domande e risposte, per poi trasformarla nel tempo in documentazione aggiuntiva direttamente nel modulo o come modulo `SomePackage.Tutorial`

Credo molto nella documentazione che si migliora in modo incrementale dato che i dubbi miei quando leggo una classe oscura sono anche di tanti altri, ma se non e\` facile "fare domande" e migliorare la documentazione, per inerzia nessuno lo fa. Inoltre ho visto che in PHP la sezione di domande e risposte sotto la pagina di documentazione ufficiale e\` spesso molto utile.

Discussione su IRC http://irc.fgaz.me/425


