# Meetup Organizations

## Shared Projects

### Shared Git Repo

Nei meetup fa comodo:
* condividere uno stesso repo GIT
* fare commit frequenti per sincronizzarsi, anche se il codice non è in bella

I problemi sono:
* i commit frequenti "sporcano" il repo 

La soluzione proposta è:
* creare un branch di lavoro
* annotare con commenti TAGGATI le parti che richiedono una review insieme
* eseguire un rebase nel branch ufficiale
* cancellare il branch di lavoro

L'unico svantaggio del metodo è che compare alla fine come autore del commit solo chi ha eseguito il rebase.

Per comodità si può lavorare in questo modo usando GitHub come repo condiviso.

####  Git Commands 

Create a develop branch, locally and on GitHub

    git checkout -b some-feature-branch
    git push --set-upstream origin some-feature-branch
    git push some-feature-branch

Develop. Then commit, and send to remote branch, for interacting with other developers.

    git commit -a -m "..."
    git push

When it is ok the feature can be merged to the master branch, deleting intermediate changes.

    git checkout master
    git merge --squash some-feature-branch
    git commit

Delete locally and remotely the develop branch

    git branch -D some-feature-branch
    git push origin :some-feature-branch

#### Tagged Comments for Discussions

Avere possibilmente un file DEVELOP.org o DEVELOP.md con le note di sviluppo del progetto. 

Inserire commenti TAGGATI sia nel file DEVELOP che nei files con il codice.

Alcuni TAG che si possono usare nei commenti, per renderne esplicito il senso:
* TODO 
* MAYBE
* DONE 
* FACT (observation) 
* PROBLEM (problem discovered in the code / design) 
* POSSIBLE-SOLUTION (possible solution for the problem) 

