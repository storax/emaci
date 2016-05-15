#!/usr/bin/env bash
cd $EMACI_TESTGITDIR2
git init
cd $EMACI_TESTGITDIR
git init
echo "file1" > file1.txt
echo "file2" > file2.txt
git add .
git commit -m "commit 1"
git checkout -b branch1
echo "coolio" > file2.txt
echo "ciao" > file3.txt
git add file2.txt
git add file3.txt
git commit -m "commit 2"
echo "stash1" > file4.txt
git stash save -a "stash 1"
git checkout master
