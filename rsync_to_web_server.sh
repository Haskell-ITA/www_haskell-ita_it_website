.stack-work/install/x86_64-linux/lts-5.14/7.10.3/bin/site clean
.stack-work/install/x86_64-linux/lts-5.14/7.10.3/bin/site build
rsync --chown=www-data:www-data --cvs-exclude -avzhe ssh ./_site/ root@asterisell.com:/var/www/www_haskell-ita_it

