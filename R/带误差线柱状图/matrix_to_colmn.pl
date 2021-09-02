#!/usr/bin/perl -w
my ($in,$out)=@ARGV;
open IN,$in;
open OUT,">$out";
my @info;
while(<IN>){
chomp;
if (/^sample/){
@info = split /\t/,$_;
}else{
next if (/^zwy/);
my @infoo = split /\t/,$_;
my $len = @infoo;
for (my $i =1;$i < $len;$i++){
print OUT "$infoo[0]\t$info[$i]\t$infoo[$i]\n";
}
}
}
close IN;
close OUT;
