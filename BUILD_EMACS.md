# Emacs Build Instructions

## Build Environment

### After a previous build
There is not much to do since dependencies were installed during a
previous build. Simply update the dependency packages and remove
packages that are no longer dependencies.

#### Packaged Dependencies
Update existing packages
    $ sudo apt update
    $ sudo apt upgrade

Remove packages that are no longer required.

##### libjansson (Emacs 30.1 and newer)
Emacs 30.1 has native JSON support and no longer requires libjansson.
Remove it from the system with:
    $ sudo apt remove libjansson-dev

    $ sudo apt autoremove

#### Treesitter
Update treesitter to the newest release. That was 0.25 at this writing
    $ cd tree-sitter
    $ git pull --all
    $ git branch -a
This shows a list of branches that are available. Checkout the latest
release branch.
    $ git checkout release-0.25
    $ make clean
    $ make
    $ sudo make install
    
### From Scratch
NOTE: This was originially written for Emacs 29.4. The text has been
updated for newer Emacs, but not tested.

#### Packaged Dependencies

Emacs prior to 30.1 required libjansson. Skip this for Emacs 30.1 or
newer.
* libjansson-dev: 
    $ sudo apt install libjansson-dev

* HarfBuzz
* libxpm
* libpng
* libjpeg
* libtiff
* libgif
* librsvg2o
* libwebp
* libattr1-dev
* libacl1-dev

    $ sudo apt install libharfbuzz-dev libxpm-dev libpng-dev libjpeg-dev \
        libtiff-dev libgif-dev librsvg2-dev libwebp-dev imagemagick \
        libmagickwand-dev libcairo2-dev libsqlite3-dev libgtk-3-dev \
        libwebkit2gtk-4.1-dev libgnutls28-dev libattr1-dev libacl1-dev

### Build tools

* make
* clang
* libgccjit0
* libgccjit-14-dev

    $ sudo apt install make gcc libgccjit0 libgccjit-14-dev autotools-dev 

### Treesitter
    $ git clone https://github.com/tree-sitter/tree-sitter.git
    $ cd tree-sitter
    $ git branch -a
    $ git checkout release-0.24
    $ make
    $ sudo make install

## Emacs
Build and install Emacs with these recipes.

### Emacs 30.1

    $ export LD_LIBRARY_PATH=/usr/local/lib
    $ ./configure \
        --prefix=/usr/local \
        --disable-gc-mark-trace \
        --with-mailutils \
        --with-cairo \
        --with-imagemagick \
        --with-sound=yes \
        --with-x-toolkit=gtk3 \
        --with-file-notification=yes \
        --without-compress-install \
        --with-native-compilation=aot \
        --with-tree-sitter \
        --with-pgtk
    $ make -j 4
    $ sudo LD_LIBRARY_PATH=/usr/local/lib make install

#### Notes:
1. I added the --disable-gc-mark-trace command line option here.
According to the release notes and (https://www.masteringemacs.org/article/whats-new-in-emacs-301)[Mikey P]: "This disables the GC mark trace buffer for about 5% better garbage collection performance. Doing so may make it more difficult for Emacs developers to help finding [sic] GC-related bugs that you run into" [Release Notes] ... "GC bugs do happen, but I'd say they're rare enough that you can turn it off and claim your 5% speed-up voucher." [Mikey P]

2. Emacs 30.1 enabled native compilation by default using the
just-in-time strategy where it compiles lisp files as they are
encountered at run time. I prefer to move the pain upfront to build
time, even though with just-in-time compilation it will only ever
occur once and I'll probably never notice.

3. I decided to not use link time optimization based on the release notes
description of "Link time optimization is not the default as it tends to
cause crashes and to make Emacs slower." I will need to research exactly
what it does before turning it back on.

4. Emacs does not work well with new versions of WebKit2GTK. See
(https://www.reddit.com/r/emacs/comments/1hdjgfg/webkit2gtk_on_recent_linux_distributions/)[Reddit] and (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66068#71)[Bug Report]. There has been some progress using WPE WebKit (https://yhetil.org/emacs-devel/8821.76377797208$1723267401@news.gmane.org/)[Emacs-Devel] and (https://yhetil.org/emacs-devel/87mso3ni0a.fsf@xn--no-cja.eu/)[Emacs-Devel], but that does not seem to have been pulled into Emacs 30. I am just disabling xwidgets since I do not actually use the feature right now.

### Emacs 29.4

    $ ./configure \
        --prefix=/usr/local \
        --with-mailutils \
        --with-cairo \
        --with-imagemagick \
        --with-sound=yes \
        --with-x-toolkit=gtk3 \
        --with-file-notification=yes \
        --with-xwidgets \
        --without-compress-install \
        --with-native-compilation=aot \
        --with-json \
        --with-tree-sitter \
        --with-pgtk \
        --enable-link-time-optimization 
    $ make -j 4
    $ sudo make install

