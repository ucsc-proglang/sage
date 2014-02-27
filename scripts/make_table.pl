#!/usr/bin/perl -w

$tlines = $tnsyes = $tnsmaybe = $tnsno = $tsyes = $tsmaybe = $tsno = 0;
foreach $base (@ARGV) {
    $file = $base . ".sage";
    $nslog = $base . ".nsout";
    $log = $base . ".out";
    @nsline = split(/\s+/, `grep MAYBE $nslog`);
    @nsstats = split(/\//, $nsline[2]);
    @sline = split(/\s+/, `grep MAYBE $log`);
    @stats = split(/\//, $sline[2]);
    ($_,$lines, $file) = split(/\s+/, " " . `wc -l $file`);

    $tlines += $lines;
    $tnsyes += $nsstats[0];
    $tnsmaybe += $nsstats[2];
    $tnsno += $nsstats[3];
    $tsyes += $stats[0];
    $tsmaybe += $stats[2];
    $tsno += $stats[3];
    print "\\texttt{$file} & $lines & $nsstats[0] & $nsstats[2] & ";
    print "$nsstats[3] & $stats[0] & $stats[2] & $stats[3] \\\\\n";
}

print "\\hline\n";
print "Total & $tlines & $tnsyes & $tnsmaybe & $tnsno & ";
print "$tsyes & $tsmaybe & $tsno \\\\\n";
