./dist/build/site/site clean
./dist/build/site/site build
rsync --chown=www-data:www-data --cvs-exclude -avzhe ssh ./_site/ root@asterisell.com:/var/www/blog_haskell-ita_it

