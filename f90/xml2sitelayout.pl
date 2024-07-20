#!/usr/bin/perl -w
use strict;
use warnings;

# functionality: for all XML files in immediate subdirectories 
# (such as those obtained from IRIS EMTF)
# create a file names as *.sitelayout.xml oriented to site layout.
# A. Kelbert, Nov 28, 2023

my $dir = shift // '.';
my $input_fname;
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

if (@take_files==0) {
  print "No XML-files in subdirectories. Exiting...\n";
  exit;
}

my $output_fname;
foreach $input_fname(@take_files){
	$_ = $input_fname;
	s/\.xml$/\.sitelayout\.xml/;
	$output_fname = $_;
	print "$output_fname\n";
	print "xml2xml $input_fname $output_fname silent sitelayout\n";
	system("xml2xml $input_fname $output_fname silent sitelayout\n");
}
