#!/usr/bin/env perl


while(<>) {
    if (/^benchmarking (.*)$/) {
        $name = $1;
    }
    if (/^mean:\s*([\d.]*) (\w*),/) {
        $mean = 1 * $1;
        $units = $2;
        
        printf "%-30s %10.2f %2s\n", $name, $mean, $units;
    }
}
