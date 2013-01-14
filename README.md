[![Build Status](https://travis-ci.org/lewang/ws-butler.png)](http://travis-ci.org/lewang/ws-butler)

## ws-butler -- an unobtrusive way to trim spaces from end of line

- Only lines touched get trimmed.  If the white space at end of buffer is
  changed, then blank lines at the end of buffer are truncated respecting
  `require-final-newline`

- Trimming only happens when saving.

## What does unobtrusive mean?

The user is not made explicitly aware when trimming happens.  He keep working
and the butler takes care of white-space for you.

This means if point is at a location that was trimmed, point is not moved, but
the data on disk has been cleaned up (revert the buffer to confirm).

## history

1. I started by to trim all spaces at EOL in source code in a
   "write-file-hook", when I started programming.  It seemed like a great idea.

2. Then I got a job working on a code base where no one else trimmed spaces,
   so my commits became super noisy.  I wanted to stop being the "white space"
   police, so switched to [ws-trim][].
    * ws-trim works in a `post-command-hook` and trims white space while you
      edit.
    * This was too eager for me, for example I would stop scroll away to look
      at some code and when I get back to continue, my indentation is gone.
    * It caused some problems with other customizations which also rely on
      `post-command-hook`.

3. I started experimenting with using `highlight-changes-mode` to trim only
   lines I touch on save.


## This is the result

[ws-trim]: ftp://ftp.lysator.liu.se/pub/emacs/ws-trim.el
