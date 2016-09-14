#!/usr/bin/perl 
#
# This script bootstraps the script-kiddie-protection encoding of an assembled 
# 'original' version of CCGMS Term 5.5 or 5.5+ (built with 'make history'
# target). Alternatively, you could load the assembled ccgmsterm5.5 prg
# on a C64, execute SYS15636 (SYS15631 for 5.5+), and resave it.
#
use strict;
use File::stat;
		              
my $file = shift @ARGV;
my $expectedFileLength = 19279;
my $startEncode = 0x843; # original range of encoded bytes
my $endEncode = 0x3507;
my $decodeSubLength = 74;
my $buf;

die if (!-f $file);

my $sb = stat($file);
my $filesize = $sb->size;

if ($filesize == $expectedFileLength - 5) { # account for change in 5.5+
	$expectedFileLength -= 5;
	$endEncode -= 5;
}

open (F, "<$file") || die;
binmode(F);

die if (read(F,$buf, $filesize) != $filesize);
my $cnt = 0;
my $dbg = 0;
my @bytes = split(//, $buf);
my $noEncode =  ($filesize != $expectedFileLength || ord($bytes[$startEncode]) != 0x78 || ord($bytes[$endEncode-1])!=0xfe);

print STDERR "Binary doesn't match expected form; skipping encoding.\n" if ($noEncode);
foreach(@bytes) {
	if ($dbg) {
		printf ("%02x\n",ord($_))if ($cnt == $startEncode || $cnt == $endEncode-1);
	} elsif (!$noEncode && ($cnt >= $startEncode && $cnt < $endEncode 
		|| $cnt >= $endEncode + $decodeSubLength && $cnt < $filesize)) {
		print(chr(ord($_) ^ 0xaa));
	} else {
		print;
	}
	++$cnt;
}
close(F);
