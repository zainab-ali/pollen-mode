# Pollen mode
An Emacs major mode for editing [Pollen markup files](https://docs.racket-lang.org/pollen/).

This contains:
 - syntax highlighting for `.pm` files
 - basic editing commands
 - commands to work with the pollen project server

## Editing commands

You can edit pollen markup tags using:
 - `pollen-change-surrounding-tag-name`
 - `pollen-split`
 - `pollen-join`
 - `pollen-delete-surrounding-tag`
 - `pollen-up-tag`
 - `pollen-mark-surrounding-content`

Find what each of these do by calling `M-x describe-function`, or just call them and see.

## The project server

You can start a project server by calling `pollen-start-server` and view a processed file by calling `pollen-browse-url`. Use `M-x describe-function` to find out how to use these.
