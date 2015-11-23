---
title: Integer Hypothenuses
author: Massimo Zaniboni <massimo.zaniboni@gmail.com>
date: 2015-10-23
tags: coding
---

Tutto inizio\` in chat #haskell.it. f-a chiede aiuto a meditans, il matematico del gruppo, su un esercizio tratto da [Progamming Praxis](http://programmingpraxis.com/2015/10/09/searching-for-hypotenuses/):

"Your task is to write a program that returns all the numbers less than some limit (think somewhere in the millions or tens of millions) that can be the hypotenuse of a right triangle with integer sides. Strive for the minimum possible run time for your program."

<div></div><!--more-->

Boring Code Prelude
-------------------

> {-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
>
> module Main where
>
> import Data.List
> import System.Environment
> import System.IO (stdout)
> import GHC.IO.Handle
> import Debug.Trace
> import qualified Data.Set as Set
> import qualified Data.IntSet as ISet 
> import Control.Monad
> import Control.Monad.ST
> import Data.Monoid
> import Data.Foldable  (foldMap)
> import qualified Data.ByteString as BS
> import qualified Data.ByteString.Lazy as LBS
> import qualified Data.ByteString.Builder as BS
> import qualified Data.IntPSQ as P
> import Data.Array.IArray (assocs)
> import Data.Array.MArray (newArray, writeArray)
> import Data.Array.ST     (STUArray, runSTUArray)
>
> main :: IO ()
> main
>   = do [algo, maxLimitS] <- getArgs
>        let maxLimit :: Int = read maxLimitS
>        case algo of
>          "v1" -> v1 maxLimit
>          "v2" -> v2 maxLimit
>          "v3" -> v3 maxLimit
>          "v4" -> v4 maxLimit
>          "v5" -> v5 maxLimit
>          "v7" -> v7 maxLimit
>          _ -> error "Unknown algo version"
>          
> v1 maxLimit = print $ solution maxLimit
> 
> v2 maxLimit = print $ triplesLessThan_v1 maxLimit
>
> v3 maxLimit = print $ multOnlyUniques maxLimit
>
> v4 maxLimit = print $ multOnlyUniques_v2 maxLimit
>
> v7 maxLimit = print $ triplesLessThan_v7 maxLimit


La specifica eseguibile
-----------------------

In tempo record larsen se ne esce con la sua implementazione che fa anche da specifica formale del problema, la chiama con orgoglio "the lower bound of smartness :-)" e ci saluta perche\` deve andare a pranzare.

> perfectSquares :: Int -> [Int]
> perfectSquares c = map (\x -> x * x) [1..c]
>
> solution :: Int -> [Int]
> solution c =
>   [ floor $ sqrt $ fromIntegral z
>   | z <- perfectSquares c,
>     x <- perfectSquares c,
>     y <- perfectSquares c,
>     x < z, y < x,
>     x + y == z ]

Da notare come Haskell sia elegante e conciso: il codice fa da specifica matematica e formale del problema, oltre che essere eseguibile. 

I tempi di esecuzione sono esponenziali: accettabili per c = 10^3 (4 secondi) e fuori controllo gia\` per c = 10^4

    time ./hypothenuse v1 1000 > t1.log
    real    0m4.596s
    user    0m4.593s
    sys     0m0.004s

Ne aprofittiamo per vedere alcune soluzioni

    [5,10,13,15,17,20,25,25,26,29,30,34,35,37,39,40,41,45,50,50,51,52,53,55,58,60,61,65,65,65,65,68,70,73,74,75,75,78,80,82,85,85,85,85,87,89,90,91,95,97,100,100,101,102,104,105,106,109,110,111,113,115,116,117,119,120,122,123, ...

e notiamo che ci sono sorprendentemente molti numeri che rispettano la condizione. Notiamo anche che ci sono dei duplicati, ma meglio non osare rallentare ulteriormente il tutto introducendo una `List.dup`.

La scorciatoia matematica
-------------------------

meditans ci segnala un articolo di Wikipedia in cui si parla delle [Pythagorean triples](https://en.wikipedia.org/wiki/Pythagorean_triple) che sono una soluzione al nostro problema.

Come spesso capita, grazie alla matematica e\` possibile risolvere un problema apparente complesso, applicando teoremi e sfruttando propieta\` dei numeri e riducendo il tutto ad una serie di calcoli decisamente meno onerosa. 

La soluzione codificata da meditans sotto dettatura di Pitagora, consiste nel generare un set iniziale di numeri che sono co-primi fra di loro:

> primitiveLessThan :: Int -> [Int]
> primitiveLessThan c = do
>   n <- [1   .. nLimit c]
>   m <- [n+1 .. mLimit c n]
>   guard (gcd n m == 1)
>   return $ m*m + n*n
>
> nLimit c   = floor . sqrt $ (fromIntegral c / 2)
> mLimit c n = floor . sqrt $ (fromIntegral c - (fromIntegral n)^2)

e nel usare questo set per generare tutte le soluzioni, moltiplicando ogni numero del set per 2, poi 3, poi 4, fino a quando non si raggiunge o supera il limite dei numeri da produrre.

> triplesLessThan_v1 :: Int -> Set.Set Int
> triplesLessThan_v1 c =
>   Set.fromList
>   . concatMap (\x -> [x,2*x .. c])
>   . primitiveLessThan $ c

Intanto da notare l'uso della funzione `guard` nella Monad `Applicative` che rende il codice di `primitiveLessThan_v1` estremamente matematico, e conciso. In C era necessario scrivere due cicli `for` innestati, mentre in Haskell i cicli diventano impliciti e tutte le combinazioni di `n` e `m` che rispettano la condizione sono tornate come result.

Ma quanto paga questa ottimizzazione, rispetto alla soluzione naive di larsen? Beh Pitagora non e\` entrato nella storia immeritatamente e per 10^3

    time ./hypothenuse v2 1000 > t2.log
    real    0m0.007s
    user    0m0.000s
    sys     0m0.007s

e i risultati sono anche ordinati e senza duplicati grazie all'uso di `Set.fromList`.

    [5,10,13,15,17,20,25,26,29,30,34,35,37,39,40,41,45,50,51,52,53,55,58,60,61,65,68,70,73,74,75,78,80,82,85,87,89,90,91,95,97,100,101,102,104,105,106,109,110,111,113,115,116,117,119,120,122,123,125,130,135,136,137,140,143,145,146,148,149,150,153,155,156,157, ...

Possiamo arrischiarci a calcolare i risultati per il nostro primo milione: 10^6

    time ./hypothenuse v2 1000000 > t2.log
    real    0m10.032s
    user    0m9.945s
    sys     0m0.083s

E osare ancora di piu\` a quota 10^7 (10M)

    time ./hypothenuse v2 10000000 > t2.log
    real    3m10.876s
    user    3m9.786s
    sys     0m0.862s

Piccola ottimizzazione matematica
---------------------------------

Eseguendo un profiling si vede che la stragrande maggioraza del tempo e\` usato da `triplesLessThan_v1`. Ma alcune (forse molte) delle moltiplicazione eseguite possono essere evitate.

Se si ha `x*a = y*b` dove `a` e `b` sono i fattori co-primi, e `x` e `y`  i fattori 2, 3, ..n, con `x,a,y,b in Int` e `a < b => x > y`, allora `b = a * x/y` e `x/y in Int`, allora l'elemento successivo `(y+1)*b` e` `x*a + x/y*x` che e\` sicuramente intero ed e\` sicuramente generato da `a` senza dover iterare su `b`.

Quindi e\` possibile iterare solo su `a`, ed evitare di iterare su `b`.

massimo_zaniboni implementa la cosa, come una pulce sulla spalla di meditans che era sulle spalle dei giganti ottenendo:

> addMults :: Int -> Set.Set Int -> Int -> Set.Set Int
> addMults c set1 b
>   = foldl' (\set2 x -> Set.insert x set2) set1 [b, 2*b .. c]
> 
> 
> multOnlyUniques :: Int -> Set.Set Int
> multOnlyUniques c
>   = let uniquePrimitives = Set.toAscList $ Set.fromList $ primitiveLessThan c
> 
>         addMults1 set1 b
>           = case Set.member b set1 of
>               True
>                 -> set1
>                    -- b and its mults are already generated from previous factors
>               False
>                 -> addMults2 set1 b
> 
>         addMults2 set1 b
>           = foldl' (\set2 x -> Set.insert x set2) set1 [b, 2*b .. c]
>             
>     in foldl' addMults1 Set.empty uniquePrimitives 

I tempi passano da 10s per 10^6 a

    time ./hypothenuse v3 1000000 > t3.log
    real    0m3.690s
    user    0m3.583s
    sys     0m0.100s

e per 10^7 da 3m11s a

    time ./hypothenuse v3 10000000 > t3.log
    real    0m54.082s
    user    0m53.156s
    sys     0m0.779s

Ok la matematica paga ancora. Inoltre da un `diff t2.log t3.log` si vede che il risultato e\` lo stesso, e non abbiamo tagliato delle soluzioni.

Low Lewel Optimization: IntSet
------------------------------

Veniamo alla prima low-level optimization, in cui invece di migliorare l'algoritmo matematico sfruttando propieta\` del nostro problema, iniziamo a convertire il codice in codice equivalente ma piu\` efficiente da eseguire. Quindi passiamo dal dominio dei numeri a quello delle CPU, cache, RAM e quant'altro.

I Set in Haskell sono una struttura dati generica che accetta come chiave di ordinamento ogni type di tipo `Ord`. La chiave e\` memorizzata come un puntatore ad un Int. Ma un Int e\` compatto quanto un puntatore, quindi gia\` che ci siamo possiamo memorizzare direttamente l'Int nel Set, invece che il suo puntatore, risparmiando una operazione di deferenza ad ogni accesso.

Il `IntSet` e\` appunto una struttura dati che memorizza la chiave come un Unboxed Int, ovvero direttamente come Int.

Sempre massimo_zaniboni nel tentativo di ergersi a pulce bipede, sulla spalla di meditans, a sua volta sulle spalle di Pitagora, se ne esce con:

> multOnlyUniques_v2 :: Int -> ISet.IntSet 
> multOnlyUniques_v2 c
>   = let uniquePrimitives = ISet.toAscList $ ISet.fromList $ primitiveLessThan c
> 
>         addMults1 set1 b
>           = case ISet.member b set1 of
>               True
>                 -> set1
>                    -- b and its mults are already generated from previous factors
>               False
>                 -> addMults2 set1 b
> 
>         addMults2 set1 b
>           = ISet.union set1 (ISet.fromDistinctAscList $ [b, 2*b .. c])
>             -- NOTE: this code take advantage of order in list, instead of performing generic insert operations.
>             
>     in foldl' addMults1 ISet.empty uniquePrimitives 

Notiamo che si costruisce il Set usando `fromDistinctAscList` che sfrutta la propieta\` che gli elementi della lista sono crescenti, e usa un algoritmo piu\` veloce. Si usa inoltre `union` fra Set che sfrutta propieta\` del Set per velocizzare l'operazione.

I tempi passano da 3s per 10^6 a

    time ./hypothenuse v4 1000000 > t2.log
    real    0m0.863s
    user    0m0.826s
    sys     0m0.036s

Per 10^7 si passa da 54s a

    time ./hypothenuse v4 10000000 > t2.log
    real    0m16.551s
    user    0m16.302s
    sys     0m0.172s

Quindi sicuramente l'ottimizzazione ha pagato e non poco.

Un `diff` dei file prodotti dalla v3 e v4 dimostra che le soluzioni trovate sono esattamente le stesse e quindi non si sono introdotti errori nella trasformazione del codice.

Low Level Optimization: ByteString Builder
------------------------------------------

Invece di usare la funzione `print` che converte verso una potenzialmente lenta `String`, e confortati dal fatto che il profiler ci dice che una buona parte del tempo e\` speso per generare la stringa finale con il risultato, riscriviamo l'algo che scrive la string con i ByteString Builder:

> v5 maxLimit = do
>   let result = multOnlyUniques_v2 maxLimit
>   hSetBinaryMode stdout True
>   hSetBuffering stdout (BlockBuffering Nothing)
>   BS.hPutBuilder stdout (renderIntSet result)
>
> renderIntSet :: ISet.IntSet -> BS.Builder
> renderIntSet set1
>   = foldMap renderInt (ISet.toAscList set1)
>  where
>    renderInt i
>      = mappend (BS.intDec i) (BS.charUtf8 '\n')
>

I tempi per 10^7 passano da 16.5s a 14.9s, con l'unico vantaggio che l'output e\` personalizzabile rispetto alla built-in function.

    time ./hypothenuse v5 10000000 > t5.log
    real    0m14.899s
    user    0m14.737s
    sys     0m0.152s

In questo caso l'ottimizzazione non ha pagato e il profiler continua a dirci che la funzione di stampa si porta via un bella fetta di tempo.


Haskell e Codice Modulare
-------------------------

Molte funzioni accettano o emettono delle liste come result. In un linguaggio con semantica strict, questo sarebbe inefficiente dato che si materializza il risultato in una forma non ottimale, per poi trasformarlo in altro in fase di stampa o processing. Invece in Haskell la lista e\` un ottimo mezzo per trasferire i dati dato che di fatto si comporta come una coda di dati da elaborare, e il compilatore spesso non la genera nemmeno parzialmente (un pezzo alla volta, in modo incrementale), ma applica direttamente una list fusion fra le funzioni. Ci guadagna il codice che e\` leggibile, modulare e grazie al compilatore efficiente. Ovvero in C uno deve decidere fin dall'inizio la struttura dati da usare per far comunicare fra loro le funzioni ed e\` difficile scrivere codice che si comporta come se fosse un generator/iterator e produce un chunk alla volta. Si puo\` fare ma il codice risultante e\` un blocco unico, poco modulare e poco riusabile.

Quindi in Haskell si scrivono tante funzioni riusabili in diversi contesti, e ci pensa il compilatore a compilarle come se fossero un blocco di codice unico e coeso. Sembra poco, ma in realta\` e\` molto dato che viceversa uno deve riscrivere e rifattorizzare piu\` volte lo stesso codice, per tenere conto di piccole differenze di formato dati.

Low Level Optimization: Bit Array
---------------------------------

I risultati sono densi rispetto ai numeri interi: ovvero dato un insieme di numeri [1 .. n], sono molti i numeri che stanno nell'insieme. In questo caso un Bit Vector in cui viene usato un Bit 0/1 per indicare se un certo Int e\` parte della soluzione o no risulta essere efficiente, dato che usa meno RAM rispetto alla memorizzazione esplicita di tutti i numeri. Per dire, 1M di Int a 64 bit, occupa 4MB di RAM, mentre usando i Bit Vector 122KB di RAM.

Haskell supporta Array unboxed (i valori sono memorizzati nella cella di memoria senza passare per un puntatore e relativa deference) e aggiornabili in modo Mutable (con modifica in-place, senza salvare il vecchio valore), come si ha con gli array C. Le operazioni mutable devono essere eseguite all'interno di una Monad ad-hoc. La monad permette di esprimere algoritmi di tipo mutable/imperative, come si farebbe in C. Al termine dei calcoli, il valore tornato dalla Monad torna ad essere un value puro gestibile da Haskell e dalle funzioni e quindi si "snatura" il tutto solo all'interno della Monad o delle funzioni chiamate all'interno della Monad, perche\` ricordiamo che una Monad puo\` chiamare altre Monad dello stesso tipo.

Da notare come questo comportamento e\` in linea con la filosofia Haskell che e\` piu\` quella di usare il Domain Specific Language giusto per risolvere un problema, che quella di usare sempre e comunque funzioni pure. E alcuni algoritmi sui grafi o altro, risultano piu\` chiari usando mutable references. In questo caso usiamo una Mutable monad principalmente per motivi di efficienza.

meditans meditando su tutto questo se ne esce con questa versione

> triplesLessThan_v7 :: Int -> [Int]
> triplesLessThan_v7 c = map fst . filter snd . assocs
>                   $ runSTUArray (newArray (1,c) False >>= truifyHypoUntil c)
> 
> truifyHypoUntil :: Int -> STUArray s Int Bool -> ST s (STUArray s Int Bool)
> truifyHypoUntil c = foldl1' (>=>) $ map (truifyMultiples c) (primitiveLessThan c)
> 
> truifyMultiples :: Int -> Int -> STUArray s Int Bool -> ST s (STUArray s Int Bool)
> truifyMultiples c n = foldl1' (>=>) . map truify $ [n,2*n .. c]
>   where
>     truify :: Int -> STUArray s Int Bool -> ST s (STUArray s Int Bool)
>     truify i a = do writeArray a i True; return a

Rispetto alla sua prima versione naive di Pitagora fatta da meditans, i tempi si riducono da 3m10s a meno di 11s per 10^7.  

    time ./hypothenuse v7 10000000 > t7.log
    real    0m10.824s
    user    0m10.401s
    sys     0m0.412s

Su 10^6 si sta sotto il secondo

    time ./hypothenuse v7 1000000 > t7.log
    real    0m0.875s
    user    0m0.815s
    sys     0m0.060s

TODO applicare ottimizzazione dei fattori ripetuti come fatto nei passi prima e vedere la differenza di velocita\`.

Librerie a Disposizione
-----------------------

Haskell e\` un linguaggio poco usato, ma nonostante cio\` vi e\` un alto numero di librerie che si possono usare, ed e\` difficile non trovare quello di cui si ha bisogno. Nel nostro caso:

* Set
* IntSet
* Mutable unboxed arrays
* Priority Queues
* libreria di formattazione stringhe avanzata


