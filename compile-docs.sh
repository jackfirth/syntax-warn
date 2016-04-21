#!/bin/bash
set -e # exit with nonzero exit code if anything fails

sribble --dest out --dest-name index syntax/warn.scrbl
