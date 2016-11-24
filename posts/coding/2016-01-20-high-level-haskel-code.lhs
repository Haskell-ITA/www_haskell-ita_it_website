---
title: Studio di codice Haskell high-level 
author: Massimo Zaniboni <massimo.zaniboni@gmail.com>
date: 2016-01-20
tags: coding
---

La libreria [Haskell-MVC](http://hackage.haskell.org/package/mvc) scritta da Gabriel Gonzalez, è un ottimo esempio di codice Haskell d'alto livello, scritto da una persona sicuramente intelligente e che padroneggia il linguaggio. La libreria è usata in questo post solo come punto di partenza e pretesto per capire quanto sia facile o difficile studiare il codice Haskell scritto da altri, e non verrà trattata nella sua interezza, anzi quasi per niente.

La libreria definisce una View come 

> import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>), First)
> import qualified Data.Monoid as M
>
> newtype View a = AsFold (FoldM IO a ())

`FoldM` è un data-type definito in `Control.Foldl`. 

<div></div><!--more-->

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

Quindi la prima funzione `x -> a -> x` è da considerarsi come una funzione di fold, che accetta uno stato di elaborazione intermedio `x`, un elemento della "lista" `a` e torna il nuovo stato intermedio. Segue lo stato iniziale di elaborazione, come nella `fold`. La seconda funzione è invece una funzione finale che viene usata per convertire lo stato finale della `fold` nel risultato finale voluto.

E infatti abbiamo questa funzione che esegue una `Fold`, come se fosse una `fold`:

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

e per la quale valgono la propietà associativa ed esiste un elemento neutro. I numeri Interi sono Monoid, e lo sono rispetto la somma o la moltiplicazione, tra le altre cose. Le liste sono Monoid e lo sono rispetto alla concatenazione.
 
Siccome una View è un Monoid, allora possiamo sempre combinare due View e ottenere una nuova View. Il che rende la View un tipo di dato molto comodo da usare e estremamente componibile. Come tutti i Monoid del resto.

A cosa servono Functor e Applicative?
-------------------------------------

Definire una `Fold` come un `Applicative` o `Functor` o un `Monoid`, permettere di ereditare tutte le propietà delle classi e tutte le funzioni di supporto. Si possono riusare i concetti tipici di un `Functor` e di un `Applicative`, senza doversene inventare di nuovi e ad-hoc. Lo stesso accade in matematica, quando in algebra (per non parlare della category theory) si definiscono le operazioni matematiche  in base alla struttura che hanno, e si ereditano in automatico tutte le propietà e teoremi che valgono sulla struttura a cui sono state associate.

Se si studia [http://hackage.haskell.org/package/foldl-1.0.7/docs/src/Control-Foldl.html](http://hackage.haskell.org/package/foldl-1.0.7/docs/src/Control-Foldl.html) si possono aprezzare le ulteriori definizioni "matematiche" legate alla `Fold`, che si ereditano in automatico ogni volta che si definisce qualcosa in termini di `Fold`.

Inoltre `Functor`, `Applicative` e  `Monoid` rispettano la propietà associativa e hanno un elemento neutro. Questo fa si che abbiano un comportamento facile da prevedere per chi programma e che siano facilmente componibili fra di loro.

Conclusioni
-----------

Il codice Haskell d'alto livello (scritto da persone intelligenti e che sanno il fatto loro) è da una parte molto compatto, ma è anche purtroppo molto denso e difficile da capire, dato che le definizioni di nuove funzioni e strutture dati si riferiscono a concetti descritti in altri package, e che spesso non sono per niente banali. L'aproccio assomiglia alla costruzione di teorie matematiche in cui ogni definizione si rifà ai concetti e definizioni introdotte precedentenmente, e quindi è facile perdersi dopo un pó, se non si ripassano continuamente le definizioni precedenti.

Probabilmente le IDE dovrebbero aiutare di più permettendo di:

* espandere in modalità lettura/studio i tipi intermedi durante la combinazione di funzioni;
* navigare velocemente nelle definizioni di tipi;
* suggerire funzioni di trasformazione di tipi, in stile Hoogle, in modo da accedere in modo pratico all'ampio vocabolario di funzioni disponibili per ogni classe;
* istanziare codice generico, applicandolo a tipi e valori specifici usati come esempio, per "toccare con mano" il risultato finale, senza dover eseguire il term-rewriting mentalmente;
* altre ed eventuali: in pratica tutto quello che può aiutare nello studio di una teoria matematica / codice Haskell;

La buona notizia è che il riuso di codice Haskell è sicuramente superiore al riuso di codice object-oriented, dato che librerie Haskell che eseguono compiti complessi, hanno pochissime linee di codice, e riusano concetti definiti in altri packages, con una granularità di riuso molto più fine del tipico codice object-oriented.
