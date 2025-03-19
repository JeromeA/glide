
# Using NASM

The reason we are using NASM is because gas is trying very hard to do everything in one pass, and doesn't even
try to replace symbols with their values. Even in simple cases, it assumes that some relocation will be needed:

```asm
    .text
    .org 0x40000
file_start:
    .quad 1
    .quad 2
myaddr:
    .quad 3
    .quad myaddr
```

In this example, the last quad is set to value zero instead of 0x40018.

