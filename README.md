WEB2DAY 2013
============

This is the website of the Web2Day 2013.

## Build the site

### Hakyll

This site is built with Hakyll, a most excellent static site generator
generator.

You need to have the haskell platform installed:

<http://www.haskell.org/platform/>

Then, install hakyll:

    cabal update
    cabal install hakyll

### Less

The styles are built with less. The build process uses `lessc`.

You need Node.js to install less.

<http://nodejs.org/download/>

Then, install less:

    npm install -g less

Make sure `lessc` is in your `$PATH` before building the site

### Build and preview

To generate the static website (generated files are stored in `_site`):

    make

To preview the site on (the website will be available at `localhost: 8000`):

    make preview

