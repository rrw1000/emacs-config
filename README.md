# rrw's emacs configuration

I like emacs. VSCode nearly cuts it, but the way it deals with multiple files and file navigation is pretty awful and reduces my productivity.

So, this repo is my dump of my personal emacs configuration.

It was forked from rksm/emacs-rust-config . I've customised it for my environment.

Use this as:

```
emacs -q --load /path/to/this/standalone.el
```

from the root of your project. I have an alias:

```
alias emacsme="emacs -q --load ~/work/emacs/emacs-config/standalone.el"
```

## Install stuff

```
pip3 install python-lsp-server
```


## Notes

`neotree-find` (`M-d`) will browse neotree to the directory of the current buffer.

`M-r` will recompile

`M-g` goto line

`C-1` inserts a literal tab



 
 

