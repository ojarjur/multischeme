#!/bin/bash
if [ ! -d 'bin' ]; then mkdir bin; fi
csc -o bin/multischemec src/main.scm