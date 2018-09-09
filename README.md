# elm-text-editor

A flexible text editor written in Elm

[Check out the demo](https://sidneynemzer.github.io/elm-text-editor/)

> Note:
> * This project is not published yet on package.elm-lang.org
> * I would not consider this project ready for use in production, since it's missing major features like scrolling and line wrap

## Features / Architecture

This library implements an editor (duh) and a buffer. The buffer is separate from the editor to allow multiple editors to use the same buffer, such as in a multi-panel text editor.

> Note: There's a checkmark next to implemented features

### Buffer

- [x] file content
- [ ] save status
- [ ] syntax highlighting (cached)
- [ ] decorations (eg underlines, tooltips, gutter icons)

The buffer implementation has helper functions for manipulating its content, like finding the end of a word.

### Editor

- [x] cursor location
- [x] selection
- [x] rendering to the DOM
- [x] UI interaction (mouse and keyboard)
- [x] undo history
- [ ] scroll position
- [ ] auto-complete dialog
- [ ] open decorations (in other words, decorations exist in the buffer but each editor tracks open decorations)
- [ ] line wrap

I hope that one day this project will be comparable to Ace and CodeMirror, but Ace and CodeMirror have had quite a head start (about 7 years!).

## Previous work and Inspiration

The editor behavior and keyboard shortcuts are based on [Atom][] (because that's the editor I'm most familiar with).

[Ace][] and [CodeMirror][] are text editors designed to work in a web browser. They're both written in JavaScript, so integration with Elm is pretty meh.

[Janiczek][] recently demonstrated [a text editor in pure elm][Janiczek-editor-discourse], which implements work-arounds for several issues I had faced in the past when creating a pure elm editor.

> I hope this inspires somebody to try some stuff in Elm they’ve been needing but seemed too big / hard for them! You might, like me with this project, find out it’s in your reach - no doubt thanks to Elm  
> *- Janiczek*

[Atom]: https://atom.io
[Ace]: https://ace.c9.io
[CodeMirror]: https://codemirror.net
[Janiczek]: https://github.com/Janiczek
[Janiczek-editor-discourse]: https://discourse.elm-lang.org/t/text-editor-done-in-pure-elm/1365
