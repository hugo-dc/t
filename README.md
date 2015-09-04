t
=

I've been using Steve Losh's `t` [program](https://github.com/sjl/t/) for a long time. `t` is a command line
todo list manager created in Python. 

This repository is an attempt to re-write the `t` program using Haskell, just
because I'm a Haskell Beginner and wanted to do something useful (to me) using
the language. 

Installing t
------------

`t` Uses the [Haskell Platform 2014.2](https://www.haskell.org/platform/). 

Required libraries:

- sha (Install: `cabal install sha`)
- split (Install using `cabal install split`) 

Clone the Github Repository:

    $ git clone https://github.com/hugo-dc/t.git

Compile the t.hs source code:

    $ cd t/
    $ ghc --make t.hs

Decide where you want to keep your todo lists, create your directory:

    $ mkdir /path/to/lists

Create an empty file that will store your tasks:

    $ cd /path/to/lists 
    touch tasks

Set up an alias to run `t`:

    alias t='~/path_to_t/t --task-dir /path/to/list --list tasks'



Using t
-------

### Add a Task 

To create a new task type `t [The title of the task]`

Examples:

    $ t Write t command using Haskell 
    $ t Write a README.md file

### List Your Tasks 

To print a list of your current task just run the command `t`

    $ t
    1 - Write t command using Haskeell
    c - Write a README.md file

### Finish a Task 

To finish a task use the following format `t -f [task_id]`

    $ t -f c
    $ t
    1 - Write t command using Haskell

### Edit a task 

Use `t -e [task_id] [New task title]`:

    $ t -e 1 Write t command using Visual Basic 6
    $ t 
    1 - Write t command using Visual Basic 6


