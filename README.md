A tool to format Racket code.

Features:

- Stand-alone command-line program.

- Decides line breaks as well as indentation.

- Sorts definitions where this will not change the meaning of the program.

- Optimized for simplicity and clarity, making it easy to extend and customize.

If no arguments are specified, it formats the code from standard input and writes the result to standard output.

If files are given, it formats the files. If -i is specified together with files, the files are edited in place. Otherwise, the result is written to standard output.

```
Usage: racket-format [options] files

-h  Show help
-i  Inplace edit
-v  Show version
```
