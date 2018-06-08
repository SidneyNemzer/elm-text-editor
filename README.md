# elm-text-editor
A flexible text editor written in Elm

## Anatomy of a multi-panel text editor

elm-text-editor is designed to be an "editor" in the following tree:

* workspace
  * file tree
  * buffers (for a file path -- shared by multiple editors)
    * undo history
    * current file state
    * tracks save status
    * syntax highlighting
    * decorations (underlines, tooltips)
  * panels
    * tabs
    * editor
      * buffer (one of the buffers)
      * scroll
      * cursor
