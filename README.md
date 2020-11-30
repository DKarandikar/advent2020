# advent2020

A loosely tied together bundle of scripts for [advent of code 2020](adventofcode.com)

## Workflow

This has hopefully been set up to be easy to just write code and see if it works

Idea is to the do the following to work on e.g. day1part1

> stack ghci

> day1part1

Modify some files

> :r

> day1part1

## Aside: Setting up all the Day files

> for i in {4..25}; do cp Day3.hs "Day$i.hs"; sed -i "s/3/$i/g" Day$i.hs; done