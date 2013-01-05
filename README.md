## ws-butler -- an unobtrusive way to trim spaces from end of line

Only lines touched get trimmed.  

Trimming only happens when saving.

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
