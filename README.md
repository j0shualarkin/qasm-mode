# qasm-mode
Emacs Mode for OPENQASM

# Set-Up
Clone this repo wherever you like, just remember the path. 
Add the following two lines to your emacs config file:
```
(add-to-list 'load-path "path/to/qasm-mode")
(require 'qasm-mode)
```

# Features
```Ctrl+c Ctrl+o```
This command generates a template. Open a blank .qasm file and enter the above command; you'll be prompted for a number.
If you enter 1 you'll get 

```
OPENQASM 2.0;
include "qelib1.inc"

qreg in[1];
creg out[1];

barrier in;

measure in -> out;
```

Enter a different number and you'll get that many registers. 
E.g., 3 would give you ```qreg in[3]; creg out[3];```

