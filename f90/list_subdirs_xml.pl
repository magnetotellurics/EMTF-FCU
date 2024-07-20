#!/usr/bin/perl -w
use strict;
use warnings;
use 5.010;
 
my $dir = shift // '.';
my $input_fname;
my $output_fname;
my $input_dir;
my @take_files=();

opendir my $dh, $dir or die "Could not open '$dir' for reading '$!'\n";
my @things = grep {$_ ne '.' and $_ ne '..'} readdir $dh;
foreach my $thing (@things) {
    if ( -d $thing ){
        $input_dir = $thing;
        opendir(LS, "$input_dir/") || die "Could not open '$input_dir' for reading '$!'\n";
        while($input_fname = readdir(LS)){
	        if($input_fname =~ /^([.\w]+)\.xml$/){
		        push @take_files, "$input_dir/$input_fname";
                print "$input_dir/$input_fname\n";
	        }
        }
        closedir(LS);
    }
}
closedir $dh;