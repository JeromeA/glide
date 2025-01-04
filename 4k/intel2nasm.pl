#!/usr/bin/perl -p -0777

# Delete all the directives that NASM doesn't understand.
s/.*\.(loc|cfi|file|size|type|text|section|p2align|align|globl|intel_syntax|data).*\n//g;

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
s/\.byte\s(\d+)/db $1/g;
# s/\.zero\s(\d+)/times $1 db 0/g;

# Replace local labels with global ones.
s/\.LC(\d+)/LC$1/g;

# Change call/jmp to external symbols.
s/ *(call|jmp)\s+(\S+)\@PLT/$labels{$2} = 1; "    $1 [$2]"/ge;

# Change the relative addressing syntax.
# In particular:
#   call [QWORD PTR f[rip+16]] -> call [f + 16]
#   call [QWORD PTR [r12]] -> call [r12]
s/\[?QWORD (?:PTR )?(\S+)\[rip(\+\d+)?\]\]?/QWORD [$1$2]/g;
s/\[QWORD PTR \[(r12)\]\]?/QWORD [$1]/g;
s/(\S+)\[rip\]/[$1]/g;
s/(?:PTR )?(\d+)\[rsp\]/[rsp + $1]/g;
s/PTR //g;

# Save bss entries.
if (s/\.bss\n((?:\S+:\n\s+\.zero\s+\d+\n)+)/$bss=$1; ""/e) {
  while ($bss =~ /(\S+):\n\s+\.zero\s+(\d+)\n/g) {
    push @bss, "$1: resb $2";
  }
}

# Save common bss labels.
s/\s+\.comm\s+(\S+),(\d+),(\d+)/push @bss, "$1: resb $2"; ""/ge;

# A mov from a GOTPCREL address is just a mov.
s/([a-z0-9_]+)\@GOTPCREL/$labels{$1} = 1; "$1"/ge;

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
$_ .= "str_libgtksource: db \"libgtksourceview-4.so.0\", 0\n";
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

