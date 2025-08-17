#!/usr/bin/perl -p -0777

# Delete all the directives that NASM doesn't understand.
s/^\s*\.(loc|cfi|file|size|type|text|section|p2align|globl|intel_syntax|data|bss).*\n//gm;

# Delete the .LFB and .LFE labels.
s/^\.LF[BE].*\n//gm;

# Delete everything after the .ident directive.
s/^\s*\.ident.*//ms;

# Delete all lines starting with #.
s/^#.*\n//gm;

# Replace indentation tabs with 4 spaces.
s/^\t/    /gm;

# Replace all other tabs with spaces.
s/\t/ /g;

# Delete align directives before strings.
s/^\s*\.align\s+\d+\n(\.LC\d+:\n\s*\.string\s".*"\n)/$1/gm;

# Change the format for litterals.
s/\.align/align/g;
s/\.string\s"(.*)"/db `$1`, 0/g;
s/\.ascii\s"(.*)"/db `$1`/g;
s/\.byte\s(-?\d+)/db $1/g;
s/\.value\s(-?\d+)/dw $1/g;
s/\.long\s(.+)/dd $1/g;
s/\.quad\s(-?\d+)/dq $1/g;
s/\.quad\s(\S+)/dq BASE_ADDR + $1/g;

# Replace local labels with global ones.
s/\.LC(\d+)/LC$1/g;
s/\.L(\d+)/L$1/g;

# NOTRACK is not supported by NASM, but is encoded the same as DS.
# NASM emits a warning, but it works.
s/notrack/ds/g;

# Change call/jmp to external symbols.
s/ *(call|jmp)\s+(\S+)\@PLT/$labels{$2} = 1; "    $1 [$2]"/ge;

# Change the relative addressing syntax.
# In particular:
#   call [QWORD PTR f[rip+16]] -> call [f + 16]
#   call [QWORD PTR [r12]] -> call [r12]
#   lea rbp, LISP_NAMES[rip+16] -> lea rbp, [LISP_NAMES + 16]
#   movups XMMWORD 24[rdi], xmm0 -> movups XMMWORD [rdi+24], xmm0
s/PTR //g;
s/\[(.*\[.*\])\]/$1/g;
s/(\S+)\[rip(\+\d+)?\]/[$1$2]/g;
s/-(\d+)\[([A-Za-z0-9_]+)\]/[$2-$1]/g;
s/(\d+)\[([A-Za-z0-9_]+)\]/[$2+$1]/g;
s/XMMWORD/OWORD/g;

# moveabs is just a mov.
s/movabs\s+(.*)$/mov $1/mg;

# Save bss entries.
s/(\S+):\n\s+\.zero\s+(\d+)\n/push @bss, "$1: resb $2"; ""/ge;

# Save common bss labels.
s/\s+\.comm\s+(\S+),(\d+),(\d+)/push @bss, "$1: resb $2"; ""/ge;

# A mov from a GOTPCREL address is just a mov.
s/([A-Za-z0-9_]+)\@GOTPCREL/$labels{$1} = 1; "$1"/ge;

# Add a dynsym section.
@labels = keys %labels;
$_ .= "\ndynsym:\n    dq 0, 0, 0\n";
for my $label (@labels) {
  $_ .= "    dd str_$label - dynstr, 0x12, 0, 0, 0, 0\n";
}

# Add a dynstr section.
$_ .= "\ndynstr:\n    db 0\n";
for my $label (@labels) {
  $_ .= "str_$label: db \"$label\", 0\n";
}
$_ .= "    libraries_strings\n";
$_ .= "dynstr_end:\n";

# Add a rela.text section.
$_ .= "\nrela_text:\n";
my $counter = 1;
for my $label (@labels) {
  $_ .= "    dq (BASE_ADDR + $label), ($counter << 32) | 1, 0\n";
  $counter++;
}
$_ .= "rela_text_end:\n";

# Add a bss section.
$_ .= "\neof:\n    section .bss\nbss:\n";
for my $label (@labels) {
  $_ .= "$label: resq 1\n";
}
for my $bss (@bss) {
  $_ .= "$bss\n";
}
$_ .= "bss_end:\n";
$_ .= "bss_size equ bss_end - bss\n";

