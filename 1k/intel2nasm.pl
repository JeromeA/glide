#!/usr/bin/perl -p -0777

# Delete all the directives that NASM doesn't understand.
s/.*\.(loc|cfi|file|size|type|text|section|p2align|globl|intel_syntax).*\n//g;

# Delete the .LFB and .LFE labels.
s/^\.LF[BE].*\n//gm;

# Delete everything after the .ident directive.
s/^\s*\.ident.*//ms;

# Replace indentation tabs with 4 spaces.
s/^\t/    /mg;

# Replace all other tabs with spaces.
s/\t/ /g;

# Change the format for strings.
s/\.string\s(".*")/db $1, 0/g;

# Replace local labels with global ones.
s/\.LC(\d+)/LC$1/g;

# Change the relative addressing syntax.
s/(\S+)\[rip\]/[$1]/g;
s/(?:PTR )?(\d+)\[rsp\]/[rsp + $1]/g;
s/PTR \[/[/g;

# Change call/jmp to external symbols.
$labels{"exit"} = 1;
s/ *(call|jmp)\s+(\S+)\@PLT/$labels{$2} = 1; "    $1 [$2]"/ge;

# A mov from a GOTPCREL address is just a mov.
s/\[(\S+)\@GOTPCREL\]/$labels{$1} = 1; "[$1]"/ge;

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
$_ .= "str_libc:  db \"libc.so.6\", 0\n";
$_ .= "str_libgtk:  db \"libgtk-3.so.0\", 0\n";
$_ .= "str_libgobject:  db \"libgobject-2.0.so.0\", 0\n";
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
$_ .= "bss_end:\n";
$_ .= "bss_size equ bss_end - bss\n";

