Quick Tour to Yesod
===================

These are the slides I used in my presentation to the Wellington FP Users group.  After each slide I showed the code for the slide and discussed how it works.

The talk covered the parts of Yesod that were of particular interest to me.

I have now extended the slides to include an example of using GHCJS to run Haskell in the web browser.

Building GHC with JavaScript output
-----------------------------------
This version of GHC makes a .js file whenever it outputs an object file.  It also "links" the javascript by copying all the files .js files into a .jsexe directory.
It has its own version of cabal so that .js files and .jsexe directories are installed properly.

<pre>
git clone https://github.com/the-real-blackh/ghc.git
cd ghc
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
./sync-all -r https://github.com/ghc get
</pre>

(You may need to rerun sync-all a few times if like me you get network errors part way through)

<pre>
rm -rf libraries/Cabal
git clone https://github.com/the-real-blackh/packages-Cabal.git libraries/Cabal
cp mk/build.mk.sample mk/build.mk
perl boot
./configure --prefix=/home/hamish/ghcjs
make
make install
</pre>

To use this compiler add /home/hamish/ghcjs to your path ahead of any other ghc.

<pre>
export PATH=/home/hamish/ghcjs/bin:$PATH
</pre>

You should be able to switch back to your main compiler at any point by simply not including this in you path.

The global packages (including JavaScript) will be installed to something like

 * ~/ghcjs/lib/ghc-7.1.20110508

User cabal packages are installed to something like

 * ~/.ghc/i386-darwin-7.1.20110508
 * ~/.cabal/lib/*/ghc-7.1.20110508

Installing cabal-install with GHCJS
-----------------------------------
You need to make a version of cabal-install that uses the new Cabal package.  So that which you run "cabal install" it will copy .js files and .jsexe directories to the install location.

<pre>
darcs get --lazy http://darcs.haskell.org/cabal-install
cd cabal-install
</pre>

Change build-depends in cabal-install.cabal so it has Cabal >= 1.10.1 && < 1.12

<pre>
cabal install --ghc-options='-XFlexibleInstances'
</pre>

Installing GHCJS RTS
--------------------
The runtime system js files need by GHCJS are in a package.  To install this do

<pre>
git clone https://github.com/hamishmack/ghcjs-rts.git
cd ghcjs-rts
cabal install
</pre>

Installing Yesod Slides
-----------------------
Almost there now

<pre>
git clone https://github.com/hamishmack/yesod-slides.git
cd yesod-slides
cabal install --constraint='tagged==0.2' --ghc-options='-XFlexibleInstances'
yesod-slides
</pre>

You need the -XFlixibleInstances because some of the packages on which we depend do not build with this dev version of ghc without it. 
  
Point your web browser at http://127.0.0.1:3000

Click to advance from one slide to the next.  The GHCJS slde is here the last one http://127.0.0.1:3000/ghcjs

