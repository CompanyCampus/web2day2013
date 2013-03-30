#!/bin/sh

if [ ! -e .git/refs/heads/publish ]; then
    git checkout --orphan publish
    git rm -fr .
fi

