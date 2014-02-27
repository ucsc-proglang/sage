#!/usr/bin/perl -w

use strict;

my ($infile, $expected, $type, $description);
my ($outfile, $nsfile, @instat, @outstat);
my ($run, $failed, $total);
my $interp = "../sage";
my $flags = "-maxeval 100 -prover  \"../Simplify\" -phisto -noeval";
my $nsflags = "-maxeval 100 -phisto -noeval -nosimplify";

$run = 0;
$failed = 0;
$total = 0;
open(TESTS, "<testlist") or die "Can't open test list.\n";
for(<TESTS>) {
    $total++;
    ($infile, $expected, $type, $description) = split;
    $outfile = $infile;
    $outfile =~ s/\.sage/\.out/;
    $nsfile = $infile;
    $nsfile =~ s/\.sage/\.nsout/;
    @instat = stat($infile);
    if((scalar @instat) != 13) { die "$infile not found\n"; }
    @outstat = stat($outfile);
    if(((scalar @outstat) != 13) or ($instat[9] > $outstat[9])) {
        print "Testing $infile ...\n";
        `$interp $flags $infile 2>&1 > $outfile`;
        $run++;
        if(($? == 0 and $expected =~ /FAIL/i) or
           ($? != 0 and $expected =~ /PASS/i)) {
           print "******** Test $infile failed, expected $expected\n";
           $failed++;
        }
        if($type eq "B") {
            `$interp $nsflags $infile 2>&1 > $nsfile`;
        }
    }
}
close(TESTS);

if($run > 0) {
    if($failed == 0) {
        print "All tests successful ($run/$total)\n";
    } else {
        print "FAILED $failed tests ($failed/$total)\n";
    }
}
