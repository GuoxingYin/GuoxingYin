#!/usr/bin/perl -w
open IN,"DF2.txt";
open OUT,">up.txt";
open OUT2,">down.txt";
while(<IN>){
chomp;
next if (/^UniProtID/);
my @info = split /\t/,$_;
if ($info[-2] <= -1){
print OUT2 "$_\n";
}
if ($info[-2] >= 1){
print OUT "$_\n";
}
}
close IN;
close OUT;
