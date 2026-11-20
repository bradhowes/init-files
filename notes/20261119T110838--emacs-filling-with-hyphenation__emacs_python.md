---
title:      Emacs Filling with Hyphenation
date:       2026-11-19T11:08:38+01:00
tags:       emacs, python
identifier: 
signature:  
---

Sources:

* [Emacs fill.el][fill] -- defines the fill-paragraph plumbing
* [my-hyphenate.el][mine] -- implements custom filling with hyphenation


# Text Text

The following paragraphs were taken from [Syillabification article][syll] on [Wikipedia][wiki]. Move cursor to a
paragraph and type `M-q` to invoke [fill-paragraph][fill]. For playing with the hyphenation variation,
do `M-x my/hyphenate-fill-paragraph`, or better, bind to a key like
`(keymap-global-set "C-M-q" #'my/hyphenate-fill-paragraph)` and then type `C-M-q`.

In some languages, the spoken syllables are also the basis of syllabification in writing. However, possibly due to the
weak correspondence between sounds and letters in the spelling of modern English, written syllabification in English is
based mostly on etymological or morphological, instead of phonetic, principles. For example, it is not possible to
syllabify "learning" as lear-ning according to the correct syllabification of the living language. Seeing only lear- at
the end of a line might mislead the reader into pronouncing the word incorrectly, as the digraph ea can hold many
different values. The history of English orthography accounts for such phenomena.

A hyphenation algorithm is a set of rules, especially one codified for implementation in a computer program, that
decides at which points a word can be broken over two lines with a hyphen. For example, a hyphenation algorithm might
decide that impeachment can be broken as impeach-ment or im-peachment but not impe-achment.

One of the reasons for the complexity of the rules of word-breaking is that different dialects of English tend to differ
on hyphenation: American English tends to work on sound, but British English tends to look to the origins of the word
and then to sound.[3] There are also a large number of exceptions, which further complicates matters.[3] Some guidelines
can be found in Major Keary's article "On Hyphenation – Anarchy of Pedantry."[3]

# Examples

With fill-column


[fill]: /Applications/Emacs.app/Contents/Resources/lisp/textmodes/fill.el
[mine]: ../emacs.d/user-lisp/my-hyphenate.el
[syll]: https://en.wikipedia.org/wiki/Syllabification
[wiki]: https://en.wikipedia.org
