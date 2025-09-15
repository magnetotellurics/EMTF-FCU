#!/usr/bin/perl -w
use strict;

my @take_files=();
my $input_dir;
my $output_dir;
my $dry_run = 0;

if (@ARGV>0){
  $input_dir = $ARGV[0];
} else {
  $input_dir = ".";
}

if (@ARGV>1){
  $output_dir = $ARGV[1];
} else {
  $output_dir = $input_dir;
}

if (@ARGV>2){
    $dry_run = 1;
}

system("mkdir $output_dir\n") unless -d $output_dir;

my $input_fname;
opendir(LS, "$input_dir/") || die "Unable to open the requested directory";
print "Reading directory $input_dir/\n";
while($input_fname = readdir(LS)){
	if($input_fname =~ /^(\S+)\.edi$/){
		push @take_files, $input_fname;
	}
}
closedir(LS);

if (@take_files==0) {
  print "No EDI-files in input directory. Exiting...\n";
  exit;
}

my $output_fname;
foreach $input_fname(@take_files){
	$_ = $input_fname;
	s/^(\S+)\.(\w+)$/$1\.xml/;
	$output_fname = $_;
    if ($dry_run){
        system("edi2xml $input_dir/$input_fname $output_dir/$output_fname silent 0.0d0 dry\n");
    } else {
        print "edi2xml $input_dir/$input_fname $output_dir/$output_fname silent 0.0d0\n";
        system("edi2xml $input_dir/$input_fname $output_dir/$output_fname silent 0.0d0\n");
    }
}
