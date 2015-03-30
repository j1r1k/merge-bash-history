# merge_bash_history

Utility to be used within unison (http://www.cis.upenn.edu/~bcpierce/unison/) to merge .bash_history files accross multiple machines. Written in haskell.

# Dependencies

regex-posix >= 0.95.1 

(It may be possible to relax version constraint)

# Example configuration in unison preferences file

Assuming you have root set to your home directory and merge_bash_history is in your $PATH

Add following to your unsion .prf file

```
merge = Name .bash_history -> merge_bash_history CURRENT1 CURRENT2 NEW
backupcurrent = Name .bash_history
```

# Manual usage

```
merge_bash_history <INPUT-FILE1> <INPUT-FILE2> <MERGED-FILE>
```

# Limitations
- Does not support line deletion (e.g. manual removal of lines from .bash_history file)
- No validation of parameters and presence of files

# Warnings
- Sorts .bash_history file by timestamp if not sorted

# Recommended Bash settings 

```
shopt -s histappend
PROMPT_COMMAND="${PROMPT_COMMAND};history -a"
```

From Pawel Hajdan (http://phajdan-jr.blogspot.de/2015/03/more-reliable-handling-of-bash-history.html)
