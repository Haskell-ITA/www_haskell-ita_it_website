---
title: Applicative Fold
author: Massimo Zaniboni <massimo.zaniboni@gmail.com>
date: 2016-10-20
tags: coding
---

In questo post provo a ricostruire parte del lavoro di studio, fatto insieme a Francesco (Franciman su IRC), di una libreria MVC per Haskell, scritta da Gariel Gonzaleg, pubblicata su [http://hackage.haskell.org/package/mvc](http://hackage.haskell.org/package/mvc) e descritta in [http://www.haskellforall.com/2014/04/model-view-controller-haskell-style.html](http://www.haskellforall.com/2014/04/model-view-controller-haskell-style.html).

La libreria definisce una View come 

> import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>), First)
> import qualified Data.Monoid as M
>
> newtype View a = AsFold (FoldM IO a ())

<div></div><!--more-->

Quindi `AsFold` è un tag che identifica il data-type, mentre `FoldM` è un data-type definito in `Control.Foldl`.

Control.Foldl
-------------

Andando a recuperare la definizione di `FoldM`:

> -- | Like 'Fold', but monadic
> data FoldM m a b = forall x . FoldM (x -> a -> m x) (m x) (x -> m b)

Quindi siccome è "like Fold", questa è la `Fold`:
 
> {-| Efficient representation of a left fold that preserves the fold's step
>    function, initial accumulator, and extraction function
>
>    This allows the 'Applicative' instance to assemble derived folds that
>    traverse the container only once
> -}
> data Fold a b = forall x . Fold (x -> a -> x) x (x -> b)

Quindi la prima funzione `x -> a -> x` è da considerarsi come una funzione di fold, che accetta uno stato di elaborazione intermedio `x`, un elemento della "lista" `a` e torna il nuovo stato intermedio. Segue lo stato iniziale di elaborazione, come nella `fold`. La seconda funzione è invece una funzione finale che viene usata per convertire lo stato finale della `fold` nel risultato finale voluto. E infatti abbiamo questa funzione che esegue una `Fold`, come se fosse una `fold`:

> -- | Apply a strict left 'Fold' to a 'Foldable' container
> fold :: Foldable f => Fold a b -> f a -> b
> fold (Fold step begin done) as = F.foldr cons done as begin
>  where
>    cons a k x = k $! step x a

Quindi con `Data.Foldl.fold` possiamo passare ad una `Fold` un container foldable `f a` e avere il risultato finale del fold di tipo `b`. `Foldable` rappresenta un container che è foldable, quindi di fatto tutte le strutture dati più comuni: liste, vettori, trees, ecc... 

Control.Foldl e Applicative
---------------------------
 
Lo scopo di `Fold` è quello di scrivere codice che combina due o più funzioni di fold, come

> average :: Num b => Fold b b
> average = (/) <$> sum <*> genericLength
>
> sum :: Num a => Fold a a
> sum = Fold (+) 0 id
>
> genericLength :: Num b => Fold a b
> genericLength = Fold (\n _ -> n + 1) 0 id

La combinazione delle due `Fold` `sum` e `genericLength`, in una implementazione naive richiederebbe una scansione doppia della lista. Qualcosa tipo:

> slowAverage :: Num b => [b] -> b
> slowAverage fs
>   = let s = sum fs
>         l = length fs
>     in  s / l

Le regole di combinazione delle operazioni della `Fold` permettono invece di scansionare la lista una sola volta, creando un codice equivalente a questo:

> fastAverage :: Num b => [b] -> b
> fastAverage fs
>   = let (s,l ) = foldl (\(s1, l1) x -> (s1 + x, l1 + 1)) (0, 0) fs
>     in  s / l

Questo grazie all'implementazione di `Applicative` da parte della `Fold`:

> data Pair a b = Pair !a !b
>
> instance Applicative (Fold a) where
>    pure b    = Fold (\() _ -> ()) () (\() -> b)
>
>    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
>        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
>            begin = Pair beginL beginR
>            done (Pair xL xR) = (doneL xL) (doneR xR)
>        in  Fold step begin done

e di

> instance Functor (Fold a) where
>    fmap f (Fold step begin done) = Fold step begin (f . done)
 
Alla luce di questo riguardiamo il codice di esempio

    average :: Num b => Fold b b 
    average = (/) <$> sum <*> genericLength

Notiamo l'uso di operatori `<$>`, `<*>`. Questi operatori, insieme a `<>` usato per i `Monoid` identificano la combinazione di valori fra di loro. Tutte queste combinazioni hanno un elemento neutro, e rispettano la propietà associativa, e altre propietà. Operazioni di questo tipo sono facili da capire per le persone, e presentano poche brutte sorprese, dato che l'ordine di esecuzione non cambia il risultato finale.

In questo caso invece di combinare valori in senso stretto, si combinano in un certo senso funzioni (la `Fold` contiene al suo interno delle funzioni). Ma in Haskell le funzioni sono first-class-citzien e quindi la cosa è lecita, e nel caso di codice Haskell evoluto, è una prassi. Quindi con questo codice combiniamo tre funzioni, per creare la funzione `average`, espressa non direttamente come funzione, ma come `Fold`.

L'operatore `<$>` è un sinonimo della `fmap` dei `Functor`. La `Fold` è un `Functor`, quindi è sia in grado di memorizzare un valore di tipo `x`, che di applicare al valore una generica funzione `x -> y`. La definizione di `Functor` è:

    class Functor f where
      fmap, (<$>) :: (a -> b) -> f a -> f b
 
La particolarità matematica di un `Functor` è quella di non riuscire a variare la struttura del `Functor` `f a` quando calcola `f b` dato che la funzione `a -> b` passata come parametro, non ha informazioni sul `Functor` sorgente, e non ritorna informazioni sul `Functor` destinazione.

Un esempio di `fmap` è la `map` su una lista. La `map` applica una funzione a tutti gli elementi di una lista. Ma non c'è modo in cui la `map` possa cambiare il numero di elementi della lista, dato che la funzione `f` parametro della `map` non ha alcuna informazione riguardo la lista, ma processa solo un elemento alla volta della lista. Inoltre se `map` fosse implementata in modo da variare il numero di elementi, non rispetterebbe le propietà dell'elemento neutro e la propietà associativa. 
 
Da notare che `Applicative` e poi `Monad` hanno una struttura simile, ma permettono alla funzione usata come primo parametro di modificare progressivamente più cose nel risultato finale. Ma lo vedremo in seguito.

Torniamo al nostro esempio. La prima parte dice
 
    average = (/) <$> sum ...

e si noti come nella definizione di `Fold`

    instance Functor (Fold a) where
      fmap f (Fold step begin done) = Fold step begin (f . done)

si dica semplicemente che la funzione `(/)` debba essere usata come parametro finale della `Fold` che se ricordiamo bene è la funzione usata per trasformare lo stato finale della `Fold` nel risultato finale tornato. E la cosa torna: la divisione per calcolare la media, non va applicata per ogni elemento, ma solo al termine della scansione della lista.

Da notare che la definizione di `fmap` (che sarebbe poi il nome di `<$>`) non torni un valore in questo caso, ma torni una `Fold` (simil funzione) risulato della combinazione di operatori. Solo quando si applicherà la `Fold` ad un container foldable, con un initial state, si avrà il risultato voluto. Ma noi stiamo combinando dei valori `Fold` per ora, e non dei valori finali.

Se cerchiamo di fare type checking di `(/) <$> sum ...` con `(<$>) :: (a -> b) -> f a -> f b`, notiamo che effettivamete l'elemento a sinistra dell'operatore è una funzione e l'elemento a destra la nostra struttura `Fold` di tipo `Functor`.

Vediamo la seconda parte

     ( (/) <$> sum) <*> genericLength

In questo caso `<*>` è l'operatore usato per combinare fra di loro due valori (funzioni nel nostro caso) di tipo `Applicative`. Questa la definizione di `Applicative`:
 
     class (Functor f) => Applicative f where
      pure  :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

La definizione di `<*>` è molto simile a quella di `<$>` solo che invece di avere `a -> b`, abbiamo `f (a -> b)` come primo argomento. Cosa comporta questo in pratica? Comporta che quando si torna il risultato `f b`, il risultato oltre a dipendere da `f a`, può anche dipendere da `f (a -> b)` e quindi anche dalla struttura del functor `f` usato come primo argomento in modo esplicito. Quindi se si usa come `Functor` un vettore, e si implementa per esempio la moltiplicazione di matrici, allora il numero di colonne e linee del vettore risultato possono dipendere dal numero di linee e colonne dei due vettori sorgenti. Nel caso di un `Functor` e della funzione `fmap` non era mai possibile cambiare la struttura del `Functor`.

Sia la `Applicative` che la `Functor` combinano `Functor` fra di loro e tornano un altro `Functor`. Ma la `Applicative` può cambiare la struttura dei `Functor` combinati fra di loro, mentre la `<$>` del `Functor` no. Per questo `Applicative` è più potente di `Functor`. Ma la maggior potenza ha un prezzo. Nel caso di Haskell il prezzo è quello di avere codice meno prevedibile perchè potenzialmente più potente e libero. Banalizzando è un pó come usare una `foldl` quando basta e avanza una `map`, o usare codice in IO quando basta una funzione pura.

Ma come si comporta la `<*>` nel caso della `Fold`? Noi stiamo combinando queste due `Fold` fra di loro:

    sum = Fold (+) 0 id
    genericLength = Fold (\n _ -> n + 1) 0 id
    ((/) <$> sum) <*> genericLength
 
dove gli elementi della `Fold` sono al solito la funzione di `step`, il valore iniziale e la funzione che trasforma l'ultimo step nel risultato finale della `Fold`. La definizione di `Applicative` per `Fold` è:
 
 
    instance Applicative (Fold a) where
      pure b    = Fold (\() _ -> ()) () (\() -> b)

      (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            begin = Pair beginL beginR
            done (Pair xL xR) = (doneL xL) (doneR xR)
        in  Fold step begin done

Intanto importantissimo: la funzione `a -> b` viene iniettata in una `Fold` tramite `pure` e nel nostro caso viene inserita come risultato finale, ignorando ogni altra computazione della `Fold`. È quindi come una funzione che torna una costante, senza processare nessun elemento. Nel nostro caso la costante finale è una funzione.
 
Nel caso della combinazione `<*>`, viene tornata una `Fold` finale in cui lo stato iniziale è una `Pair` con gli stati iniziali delle due `Fold` usate come parametro, il passo di step processa un elemento della struttura `Foldable` alla volta e lo passa alle due funzioni step dei due `Fold` usati come parametro. Quindi usa lo stesso "trucco" che useremo se volessimo unire i due fold in una unica iterazione della lista, mantenendo una pair nello stato.

Lo stato finale della `Fold` ha la forma `(doneL xL) (doneR xR)` ed è in realtà una applicazione di funzione. Ovvero `(doneL xL)` è una funzione. E la cosa ha senso, dato che abbiamo visto che possiamo creare un `Fold (a -> b)` solo usando `pure` e `pure` inserisce la funzione nell'ultimo elemento della `Fold`. Tornando all'esempio, `doneL` è una funzione, nel nostro caso `((/) <$> sum)` torna nel risultato finale una funzione che accetta un valore `Num b` e esegue la divisione con la `sum` di tutta la lista. Alla funzione `(doneL xL)` passiamo `(doneR xR)` che nel nostro caso è `genericLength`.

Ecco a questo punto a me sta già scoppiando la testa, dato che non sono matematico di professione, non sono abile nel term-rewriting e non riesco veramente a raffigurarmi tutto. Se avessi dovuto scrivere io questo codice, probabilmente avrei prodotto una linea al giorno. Ovviamente usarlo invece che scriverlo è nettamente più semplice, a patto che ci si fidi dei commenti, e non si cerchi troppo di capirne il funzionamento dietro le quinte. Almeno questo vale per me. Ma che senso hanno tutte queste complicazioni? Nell'aproccio Haskell se uno definisce una `Fold` come una `Applicative` o `Monoid` o `Functor` e così via, allora eredita in automatico tutte le propietà delle classi e tutte le funzioni di supporto. Quindi può usare il linguaggio che usa per le `Applicative`, anche per la sua `Fold`, senza doversi inventare delle nuove funzioni o concetti. Come in matematica si cerca con l'algebra di riunire in leggi e tipi comuni diverse operazioni matematiche che hanno una struttura comune, così in Haskell si cerca di unificare strutture dati che hanno una struttura semantica simile. Il codice risultante è estremamente compatto, ma anche denso, e occorre rinfrescarsi spesso i concetti base, dato che ogni dettaglio conta. 
 
Se si studia [http://hackage.haskell.org/package/foldl-1.0.7/docs/src/Control-Foldl.html](http://hackage.haskell.org/package/foldl-1.0.7/docs/src/Control-Foldl.html) si possono aprezzare le ulteriori definizioni "matematiche" legate alla `Fold`, che si ereditano in automatico ogni volta che si definisce qualcosa in termini di `Fold`.

View e Monoid
-------------

Torniamo alla nostra libreria MVC. All'inizio del tutto stava questa definizione di `View`

    newtype View a = AsFold (FoldM IO a ())

Nel nostro caso una `View` è vista come una funzione che accetta dati di tipo `a` e può tornare una `View` finale risultato della composizione di tutti i dati `a`, lavorando nella Monad `IO`. In particolare, la `View` è anche un `Monoid`:

> instance Monoid (View a) where
>    mempty = AsFold mempty
>    mappend (AsFold fold1) (AsFold fold2) = AsFold (mappend fold1 fold2)

Un `Monoid` è una struttura definita come:

    class Monoid a where
      mempty  :: a
      mappend, (<>) :: a -> a -> a

e per cui valgono la propietà associativa e esiste un elemento neutro. I numeri Interi sono Monoid, e lo sono rispetto la somma o la moltiplicazione, tra le altre cose. Le liste sono Monoid e lo sono rispetto alla concatenazione.
 
Siccome una View è un Monoid, allora possiamo sempre combinare due View e ottenere una nuova View. Il che rende la View un tipo di dato molto comodo da usare e estremamente componibile. Come tutti i Monoid del resto: tutti abbiamo imparato a sommare e sottrarre mele o pere alle elementari.

Conclusioni
-----------

Lo studio della libreria MVC potrebbe continuare per pagine e pagine, dato che pur essendo molto corta, ha decine di queste definizioni, che poi si rifanno a classi e funzioni generiche. Per esempio ci sono funzioni per trasformare una Pipe in un Controller e quindi partendo da una astrazione ben sviluppata di Haskell (le Pipes) è possibile derivare Controllers o Views. Il codice trasforma funzioni in funzioni con una semantica simile, ma in un contesto diverso, in modo molto elegante. 

Le mie conclusioni: il codice Haskell d'alto livello (scritto da persone intelligenti e che sanno il fatto loro) è notevolmente riusabile, e con una granularità molto fine. Però allo stesso tempo è difficile da capire, dato che le definizioni di nuove funzioni e strutture dati, pur essendo molto compatte, sono anche dense, poichè si riferiscono a concetti descritti in altri package. L'aproccio assomiglia un pó alla costruzione di teorie matematiche in cui ogni definizione si rifà ai concetti e definizioni introdotte precedentenmente, e dove è facile perdersi dopo un pó.

Nel progetto [https://github.com/Haskell-ITA/lazy-gui-choice](https://github.com/Haskell-ITA/lazy-gui-choice) a cui siete tutti invitati a partecipare, io e Francesco stiamo provando a vedere se è possibile usare Haskell in modalità MVC, creando eventualmente quello che manca.
