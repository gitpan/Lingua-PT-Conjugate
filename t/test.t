#!/usr/bin/perl -w 

BEGIN {print "1..1\n";}

$incstr = join(" ", map("-I$_", @INC));
@a = `$^X $incstr ckconj all`;

$ok=1;
foreach (@a){
	unless( /^(THESE VERBS ARE OK|NOT IN REFERENCE FILE)/){
		print "ERROR $_\n";
		$ok=0;
	}
}
unless(@a){$ok=0}
print "not " unless $ok;
print "ok 1";
