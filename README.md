# blog.haskell-ita.it

## How to Update the Website

### Initial Setup

Clone this repo locally.

Compile the application with Nix or Stack.

#### Nix Instructions

Execute

    ./generate-nix-project.sh
    nix-shell ./shell.nix
    cabal build
    ./dist/build/site/site clean
    ./dist/build/site/site build

#### Stack Instructions

Execute

    stack build
    stack exec site clean
    stack exec site build
    stack exec site watch

### Website Configuration

The file `site.hs` contains the rules used from Hakyll for generating the website. Every time you change them:

* recomplie the application
* clean the website with `site clean`

### Website Preview

Execute

    site watch

for a live preview of the website. In case of change of files, the preview is updated.

### Adding a Post

Add posts inside `posts/` directory, in the proper subdirector, playing the role of category.

Use already defined posts as example. Hakyll uses Pandoc that is very powerful and many different type of files are accepted.

#### Teaser

It is convenient to have an excerpt of a post displayed on the index page along with a “Read more…” link to encourage your readers to read the rest of the post. For doing this put the teaser symbol where the excerpt end.

    <!--more-->

In case of Literate Haskell Code use instead

    <div></div><!--more-->

for bypassing the byrding style `>` character.

#### Images

Use something like

    <img src="/images/photos/meetup_2015_estate.jpg" alt="photo" class="img-thumbnail">

on the first column of a Markdown file. Pandoc will insert directly the HTML code. The class is used from Bootstrap CSS template for scaling and decorating the image.

#### Change Top Menu

Change the function `renderTagListForTopMenu` inside `site.hs`

#### Change Page Format

Change all files in templates directory. Same format is repeated for:

* blog-list
* blog post
* page

#### Change "About" Page

Change `about.md` file.

### Website Update

Execute

    site build

for updating the `_site/` directory. In case of big changes it is better a

    site clean
    site build

Then make a Git commit, and push to the remote repo.

    git status
    git add <missing-files>
    git commit -a -m "<some-message>"
    git push

The hosting server will make a pull, and publish the content of `_site/` directory. All other content will be not be published.

### Known Problems

If you miss the tag `date`, and `author` Hakyll generates a not clear error message like:

     [ERROR] Missing field $posts$ in context for item 3/index.html

In case of community pages uses `community` as author.

## Project Roadmap (TODO)

### Initial Release

TODO provare a fare subscribe ai FEEDS quando ho installato

TODO chiedere se puo` diventare la pagina standard di Haskell invece che il blog:
* cancello la knowldge-base su GitHub
* metto su un branch a parte il sito vecchio
* faccio puntare blog.haskell-ita.it a www.haskell-ita.it
* sostituisco il sito attuale www.haskell-ita.it con questo

### Automatic Update of the WebSite

TODO create a web-hook on GitHub

TODO the web server receive the hook, make git pull of the repo, and then make a site build

TODO daily the web server execute also a site clean and regeneration, in order to manage the big changes in the structure of the code of the site

