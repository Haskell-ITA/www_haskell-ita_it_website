---
title: Un semplice script con Turtle per gestire i file RAW+Jpeg della macchina fotografica.
author: Luca Molteni <volothamp@gmail.com>
date: 2015-12-30
tags: coding turtle
---

Ecco una semplice soluzione a un problema risolta utilizzando una libreria per fare degli script in Haskell.

In fotografia, spesso si decide di memorizzare le proprie foto digitali in un formato chiamato RAW, che possiamo pensare come un equivalente di un rullino di un po' di tempo fa. I file RAW sono a tutti gli effetti i dati registrati dal sensore della macchina fotografica digitale.

Questi non rappresentano una vera e propria immagine perché comunque vanno interpretati da un motore che ne fa il rendering, si preferisce quindi sempre affiancare i file Jpeg già processati. Questi ci danno la certezza di essere visti nello stesso modo su tutti i dispositivi.

Le macchine fotografiche memorizzano la coppia Raw + Jpeg come due file separati con lo stesso nome nello stesso percorso del filesystem.
Ad esempio nel caso di un file RAW di una macchina fotografica Fuji con estensione .RAF, i due file

    DSC1234.RAF
    DSC1234.JPG

sono a tutti gli effetti la stessa immagine, ma in due formati separati, e molto probabilmente risiedono nella stessa directory.

Avevo un po' di file sparsi Raw + Jpeg nel disco rigido, e avevo bisogno un semplice script che mi permettesse di cercarli e copiarli in una directory.
Un'ottima occasione per usare Haskell.

Questo file è scritto in Literate Haskell, questo vuol dire che è un sorgente eseguibile quindi potete scaricare il codice sorgente qui e compilarlo utilizzando stack con il comando:

    stack --resolver lts-3.15 --install-ghc runghc --package turtle 2015-10-30-Turtle-Raw.lhs

Questo ci permette di installare automaticamente GHC sulla macchina.

Ogni file Haskell scritto nel 2015 inizia almeno con qualche estensione di GHC da importare.

> {-# LANGUAGE OverloadedStrings #-}

Turtle utilizza questa estensione di GHC per poter specificare
le directory del filesystem come semplici stringhe.

> module Main where

Turtle espone una proprio versione di FilePath che ha alcune funzioni differenti.

> import Turtle
> import Filesystem.Path (addExtension)
> import Prelude hiding (FilePath)

Un file RAW nel mio caso ha una estensione RAF, quindi è una funziona che,
presa una directory di origine, cerca tutti i file all'interno

> rafFiles :: FilePath -> Shell FilePath
> rafFiles source = find (suffix "RAF") source

Questo è equivalente a scrivere

    find . -iname '*.raf'

In una shell Unix, e produce uno stream di file, chiamato in Turtle Shell. Una volta che abbiamo il nostro file RAF possiamo assumere che ci sia una Jpeg equivalente con lo stesso nome, definiamo quindi una nuova funzione pura che rimuove l'estensione Raf e aggiunge l'estensione Jpeg in fondo.

> toJpeg :: FilePath -> FilePath
> toJpeg fp = addExtension baseName "JPG"
>      where baseName = dropExtension fp

Shell è un funtore, quindi possiamo trasformarne i file interni usando fmap.

> equivalentJpegFiles :: FilePath -> Shell FilePath
> equivalentJpegFiles source = fmap toJpeg rafs
>             where rafs = rafFiles source

A questo punto possiamo concatenare i nostri due stream di file per ottenere tutti i file che vogliamo spostare.

> allFiles :: FilePath -> Shell FilePath
> allFiles source = rafs <|> jpegs
>      where jpegs = equivalentJpegFiles source
>            rafs  = rafFiles source

Per copiare i file, usiamo la primitiva di Turtle che prende il path di origine e di destinazione. Questa si chiama guardacaso cp e prende il path di origine e il path di destinazione, esattamente come

    cp sourceFile destinationFile

Il filepath di destinazione ha lo stesso nome file concatenando la directory di destinazione utilizzando </>

> copy :: FilePath -> FilePath -> IO ()
> copy file destinationDir = cp file (destinationDir </> filename file)

E applichiamo questa funzion copy a tutto il nostro stream utilizzando liftIO

> copyAll :: FilePath -> FilePath -> Shell ()
> copyAll source target = do files <- allFiles source
>                            liftIO $ copy files target

A questo punto si tratta solo di ottenere la directory da cui partire per cercare le immagini e la directory di destinazione da linea di comando.
Turtle ha un semplice DSL per definire i parametri di ingresso dello script che usa la sintassi dei funtori applicativi.

> args :: Parser (FilePath, FilePath)
> args = (,) <$> argPath "sourceDir" "The source directory where to find"
>            <*> argPath "targetDirectory" "The directory to put the files"

A questo punto il nostro programma consiste nell'ottenere le due directory di origine e destinazione dal comando ed eseguire la funzione copyAll

> main :: IO ()
> main = do (source, target)
>             <- options "find all raw + jpeg pairs and copy to target" args
>           sh $ copyAll source target
