#!/usr/bin/perl

my (@fields, $filename, @totals, $i);

while(<>) {
    @fields = split;
    $filename = shift @fields;
    for($i = 0; $i < scalar (@fields); $i++) {
        $totals[$i] += $fields[$i];
    }
}

for($i = 0; $i < scalar (@totals); $i++) {
    print $i; print ".5"; print " $totals[$i] \n";
}

