---
title: Haskellism
author: Massimo Zaniboni <massimo.zaniboni@gmail.com>
date: 2017-06-22
tags: coding
---

Un mio rant su #haskell-it sull'illegibilità di certo codice Haskell avanzato, termina con una sfida: "c'è un qualche esempio di codice Haskell complesso ma chiaro?". La risposta è [https://github.com/aurapm/aura](https://github.com/aurapm/aura)

Inizia la sfida: vediamo se un programmatore Object-Oriented come me, senza una laurea in matematica e categoria delle teorie,
e che parte prevenuto, lo riesce a capire! :-)

<!--more-->

Intanto esteticamente il codice si presenta molto bene.

Il 99% delle funzioni sono nella `Aura` monad, che è così definita:

```
  {- The Aura Monad. Functions of note:
  pure    : yields a successful value.
  failure : yields an error and bypasses all other operations.
  catch   : catches an error.
  wrap    : If given an Either, rewraps it into an Aura Monad.
  (>>=)   : fails on the first error.
  liftIO  : Perform intermittent IO using `liftIO`.
  ask     : Obtain run-time settings.
  runAura : Unwraps an Aura action. Must be passed `Settings` as well.
  -}
  newtype Aura a = A { runA :: ExceptT String (ReaderT Settings IO) a }
    deriving ( Monad, MonadError String, MonadReader Settings, MonadIO,
               Functor, Applicative)
```

Da notare i commenti sintetici e dritti al punto (ode al programmatore), 
e come non ci sia boiler-plate code (ode a `{-# LANGUAGE GeneralizedNewtypeDeriving #-}`).

Quasi tutte le funzioni o usano la `do` notation,  oppure usano uno stile applicative, idiomatico e prevedibile, come:

```
  getPacmanHelpMsg = lines <$> pacmanOutput ["-h"]
```

È vero che l'abito non fa il monaco, ma gli import sono molto ordinati: 

```
  import System.Posix.Files (fileExist)
  import System.FilePath    ((</>))
  import Text.Regex.PCRE    ((=~))
  import Control.Monad      (unless)
  import Data.List          ((\\), sort, groupBy)
  import Data.Foldable      (traverse_, fold)
  import Data.Char          (isDigit)
  import Data.Monoid        ((<>))
```

Quindi di primo impatto, il codice sembra vincere la sfida: ordinato e con una struttura prevedibile.

Ora la seconda parte della sfida: proverò a studiare due funzioni collegate fra loro, per verificare se la capisco anche nel dettaglio. 
Sarò iper-critico nell'analisi, ma è ovvio che se avessi scritto io il codice, o non sarebbe mai nato, o sarebbe stato molto peggiore.

Questa la forma originale. Senza una IDE che permetta di navigare nel codice e mostri i tipi, è ovviamente impossibile
capire al 100%, ma comunque un'idea se la si riesce a fare:

```
  -- | Interactive. Gives the user a choice as to exactly what versions
  -- they want to downgrade to.
  downgradePackages :: [String] -> [String] -> Aura ()
  downgradePackages _ []         = pure ()
  downgradePackages pacOpts pkgs = asks cachePathOf >>= \cachePath -> do
    reals <- pkgsInCache pkgs
    reportBadDowngradePkgs (pkgs \\ reals)
    unless (null reals) $ do
      cache   <- cacheContents cachePath
      choices <- traverse (getDowngradeChoice cache) reals
      pacman $ "-U" : pacOpts <> ((cachePath </>) <$> choices)
 
  getDowngradeChoice :: Cache -> String -> Aura String
  getDowngradeChoice cache pkg = do
    let choices = getChoicesFromCache cache pkg
    notify $ getDowngradeChoice_1 pkg
    liftIO $ getSelection choices
```

Iniziamo la disamina:


```
  -- | Interactive. Gives the user a choice as to exactly what versions
  -- they want to downgrade to.
  downgradePackages :: [String] -> [String] -> Aura ()
  downgradePackages _ []         = pure ()
  downgradePackages pacOpts pkgs = asks cachePathOf >>= \cachePath -> do
```

Il commento della funzione è ottimo.

Il codice idem, dato che estrae l'opzione di configurazione `cachePathOf`, leggendola dai parametri di configurazione, recuperati dall'environmet della `MonadReader`, e esegue una lista di comandi nella `Aura` monad. Tra l'altro tutte le funzioni hanno questa struttura in comune.

Il codice segue le best-practices, dato che estende `Aura` con tutte le features utili, riducendo gli argomenti delle funzioni,
e rendendole componibili fra di loro di default.

Continuiamo...

```
    reals <- pkgsInCache pkgs
    reportBadDowngradePkgs (pkgs \\ reals)
    unless (null reals) $ do
```

finezza: unless + negation nei test si potrebbe fraintentedere, più chiaro IMHO qualcosa tipo
`when (not $ null reals) $ do ...`. 

Il codice continua con

```
    cache   <- cacheContents cachePath
    choices <- traverse (getDowngradeChoice cache) reals
```

`getDowngradeChoice` è una action ma essendo in `Aura String`, 
può essere passata a `traverse` in stile simil "funzionale", quindi molto leggibile. 
Quello che fa è chiedere all'utente (quindi nella IO monad) se va fatto un downgrade di un package. 
Quindi trasforma la lista `reals` in una lista di scelte dell'utente, 
passando dalla IO monad. 

Questo è il codice della funzione

```
  getDowngradeChoice :: Cache -> String -> Aura String
  getDowngradeChoice cache pkg = do
    let choices = getChoicesFromCache cache pkg
    notify $ getDowngradeChoice_1 pkg
    liftIO $ getSelection choices
```

La funzione è chiara, e navigando nelle funzioni chiamate lo diventa ancora di più. Quindi ottimo.

Torniamo alla funzione originale. La linea dopo:

```
   pacman $ "-U" : pacOpts <> ((cachePath </>) <$> choices)
```

esegue il comando `pacman` passandogli una riga di comando, sotto forma di `[ShellArg]`.

Il codice è però criptico dato che usa `<>` e `<$>` che sono si operatori standard, 
ma troppo generici e vanno istanziati al nostro caso/type per capire cosa facciano realmente.
Nel nostro caso lavorano su una lista di `[ShellArg]`, quindi `<>` è la concatenazione di liste `++` 
e `<$>` è la `fmap`, ovvero la `map` di liste. 

Quindi il codice equivale a `(map (\f -> cachePath </> f) choices)` che semplicemente trasforma una lista di `FilePath` 
relative, in `FilePath` assolute. Infatti `</>` è una funzione che costruisce una path usando il directory separator 
di Unix (`/`) o di Windows (`\`).

`"-U" : pacOpts <> ((cachePath </>) <$> choices)` è un esempio di codice che soffre di Haskellism: 
codice elegante che appaga l'orgoglio intelettuale dell'autore, ma nello stesso tempo frustra il lettore,
perchè troppo criptico da interpretare. 
Frustrazione che rischia di trasformarsi in rabbia, quando il lettore realizza al 
termine di una (troppo) lunga sessione di studio, che il codice in realtà non
sta facendo tutto questo gran che.

Un modo per rendere il codice più chiaro, ma ugualmente elegante, è riscriverlo, rendendo espliciti
i tipi coinvolti, e quindi la semantica precisa degli operatori generici coinvolti:

```
  let params::[ShellArg] 
        = ["-U"] 
            <> pacOpts 
            <> ((\filePath -> cachePath </> filePath) <$> choices)
  pacman params
```

In questa forma si capisce subito che stiamo lavorando con delle liste, quindi `<>` si associa immediatamente alla concatenazione di liste,
abbiamo tolto il `:` che confondeva solo, e `<$>` si riconduce a `fmap` che per le liste è `map`.

L'operatore `</>` usato in forma infix è più chiaro, anche se meno elegante.

Inoltre l'uso di ben cinque operatori infix diversi (`$`, `:`, `<>`, `</>`, `<$>`) rende Haskell un linguaggio orientato più agli ideogrammi
che alle funzioni, quindi ne ho scelti pochi ma buoni.

Oppure possiamo riscriverlo usando funzioni esplicite, invece della loro variante generica: 

```
pacman $ ["-U"] ++ pacOpts ++ (map (\filePath -> cachePath </> filePath) choices)
```

In ambedue i casi, il criterio è quello di ridurre il tempo necessario a capire il codice, rispetto a massimizzare l'eleganza
fine a se stessa. 

La programmazione generica (`Monoid`, `Functor`, `Applicative`) è utile per aumentare il riuso delle funzioni/concetti, 
ma non è utile quando è più quello che offusca che quello che esprime. In questi casi o si rende più esplicito il codice, 
oppure le IDE del futuro devono aiutare nella comprensione, dato che diventa più uno studio che una lettura.

Riassunto: il codice di Aura è molto chiaro 
e relativamente facile da capire, e questo si estende anche ad Haskell, 
ma non è esente da un pó di Haskellism. Parte della cripticità di molto codice
Haskell potrebbe essere ridotta, mettendosi maggiormente nei panni di chi lo deve
leggere e capire, sopratutto quando tutto sommato il codice deve fare cose molto
semplici. Molta complessità di codice Haskell è semplicemente "gratuita" e andrebbe evitata.

