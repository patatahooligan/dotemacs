# FakeDrake's Dot Emacs #

This is just my dot emacs. You are free to use it. It is based on el-get. I know I am not really using the package initializations correctly but we will get there. The nice thing about el-get is that it collects the packages I need from the internet so i dont have to fill this repo with submodules and emacswiki files that I do not maintain. Note that this (in particular the theme) will only work with emacs >= 24

## Special Key Bindings ##

* `C-=`: expand region
* `C-+`: contract region
* `C-c e s`: start or bring forth ERC
* `C-c e k`: kill all ERC buffers
* `M-+`: larger fonts
* `M--`: smaller fonts
* `C-c c`: orgmode-capture. Jot in notes you thought of just now quickly
* `C-c a`: open orgmode agenda
* `C-c d c`: Query and kill all buffers (desktop-clear)

## What you need to change ##

Some of the things in dotemacs files are personal so I will be gathering them into a file called personal.el. There will be a dummy fallback but if you serously consider using this emacs configuration i suggest you use the dummy-personal.el as a template to fill in your own preferences.

Also to use orgmode you need to setup in personal.el the agenda and orgmode folders. They help A LOT. As GTD suggest try to have *nothing* on your mind and als have *nothing* on your agenda todo. This feature needs a bit  more configuration to reach it's maximum efficincy but I am working on it.

## Features ##

In a nutshell the features this dotemacs brings to the table:

* Dark low contrast theme

* Space optimization

* Line numbers everywhere

* Hilight lines

* Some x clipboard  integration

* Nice IDO mode configuration

* Full screen on F11

* Uniquify buffers with directory names

* Show current file's full path in the title

* Unclutter bacups

* Some yasnippet

* Match parens

* Some autocomplete configuration

* Org mode configuration

* Remember session

* Undo tree

* Recent files on M-F11

* ERC configuration

* Bookmark navigation

## Features I would like to have ##

Here are some stuff I would like to put in here at some point

* auto-complete with dictionaries

* Python documentation

* Better yasnippet configuration

* python community mode(el-get managed)

* Auctex
