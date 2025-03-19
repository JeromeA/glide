#!/usr/bin/perl -p -0777

BEGIN {
  local $/ = "\n";
  open my $fh, "<", "libraries.txt" or die $!;
  chomp(@libraries = <$fh>);
}

# Replace indentation tabs with 4 spaces.
s/^\t/    /mg;

# Replace all other tabs with spaces.
s/\t/ /g;

# Change call/jmp to external symbols.
s/ *(call|jmp)\s+(\S+)\@PLT/$labels{$2} = 1; "    $1 *$2(%rip)"/ge;
# A mov from a GOTPCREL address is just a mov.
s/([A-Za-z0-9_]+)\@GOTPCREL/$labels{$1} = 1; "$1"/ge;

# # Save bss entries.
# s/(\S+):\n\s+\.zero\s+(\d+)\n/push @bss, "$1: resb $2"; ""/ge;
# 
# # Save common bss labels.
# s/\s+\.comm\s+(\S+),(\d+),(\d+)/push @bss, "$1: resb $2"; ""/ge;
# 

########################################################################
#
# Delete sections
#
s/^ *\.section \.rodata.*$//m;
s/^ *\.section \.text.*$//gm;
# Notes are the last sections, and are removed completely.
s/^ *\.section \.note.*//ms;

########################################################################
#
# Add a hash section.
#

$nchain = $#libraries + 2;
$hash_section = "
    .long 1               ; nbucket
    .long $nchain         ; nchain
    .long 1               ; bucket[0]
    .long 0               ; chain[0]
";
for my $i (2..@libraries) {
  $hash_section .= "    .long $i\n";
}
$hash_section .= "    .long 0 ; last bucket\n";

s/hash:\n/$&$hash_section/;

########################################################################
#
# Add libraries_needed entries.
#

$libraries_needed = "";
for my $i (0..$#libraries) {
  my $library = $libraries[$i];
  my $short = $library;
  $short =~ s/[.-].*//;
  $libraries_needed .= "    .quad $i, str_$short - dynstr\n";
}
s/# Here: DT_NEEDED\n/$&$libraries_needed/;

########################################################################
# Add a dynsym section.
@labels = keys %labels;
$_ .= "\ndynsym:\n    .quad 0, 0, 0\n";
for my $label (@labels) {
  $_ .= "    .long str_$label - dynstr, 0x12, 0, 0, 0, 0\n";
}

########################################################################
# Add a dynstr section.
$_ .= "\ndynstr:\n    .byte 0\n";
for my $label (@labels) {
  $_ .= "str_$label: .asciz \"$label\"\n";
}
for my $library (@libraries) {
  my $short = $library;
  $short =~ s/[.-].*//;
  $_ .= "str_$short: .asciz \"$library\"\n";
}
$_ .= "dynstr_end:\n";

# Add a rela.text section.
$_ .= "\nrela_text:\n";
my $counter = 1;
for my $label (@labels) {
  $_ .= "    .quad (BASE_ADDR + $label), ($counter << 32) | 1, 0\n";
  $counter++;
}
$_ .= "rela_text_end:\n";

# Add a bss section.
$_ .= "\neof:\n    .section .bss\nbss:\n";
$_ .= "bss_start:\n";
for my $label (@labels) {
  $_ .= "$label: .skip 8\n";
}
for my $bss (@bss) {
  $_ .= "$bss\n";
}
$_ .= "bss_end:\n";
$_ .= "    .set bss_size, bss_end - bss_start\n";

