#! /bin/sh
find . -name \*.pcat -print|sed s,^\./,, > pcat.list
