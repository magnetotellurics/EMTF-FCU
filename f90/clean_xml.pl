#!/usr/bin/perl -w
use strict;

my @take_files=();
my $input_dir;
my $output_dir;

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

system("mkdir $output_dir\n") unless -d $output_dir;

my $input_fname;
opendir(LS, "$input_dir/") || die "Unable to open the requested directory";
print "Reading directory $input_dir/\n";
while($input_fname = readdir(LS)){
	if($input_fname =~ /^([^.]+)\.(xml)$/){
		push @take_files, $input_fname;
	}
}
closedir(LS);

if (@take_files==0) {
  print "No Z-files in input directory. Exiting...\n";
  exit;
}

my $output_fname;
foreach $input_fname(@take_files){
	$output_fname = $input_fname;
	# special chars corrected...
	print "Run sed -i '' 's/\&#195;\&#188;/ü/g' to fix special characters in XML file $output_dir/$output_fname...\n";
	system("cp $input_dir/$input_fname $output_dir/$output_fname; sed -i '' 's/\&#195;\&#188;/ü/g' $output_dir/$output_fname\n");
	print "Run sed -i '' 's/\&#195;\&#164;/ä/g' to fix special characters in XML file $output_dir/$output_fname...\n";
	system("cp $input_dir/$input_fname $output_dir/$output_fname; sed -i '' 's/\&#195;\&#164;/ä/g' $output_dir/$output_fname\n");
	print "Run sed -i '' 's/\&#195;\&#161;/á/g' to fix special characters in XML file $output_dir/$output_fname...\n";
	system("cp $input_dir/$input_fname $output_dir/$output_fname; sed -i '' 's/\&#195;\&#161;/á/g' $output_dir/$output_fname\n");
	print "Run sed -i '' 's/\&#195;\&#173;/í/g' to fix special characters in XML file $output_dir/$output_fname...\n";
	system("cp $input_dir/$input_fname $output_dir/$output_fname; sed -i '' 's/\&#195;\&#173;/í/g' $output_dir/$output_fname\n");
	print "Run sed -i '' 's/\&#195;\&#177;/ñ/g' to fix special characters in XML file $output_dir/$output_fname...\n";
	system("cp $input_dir/$input_fname $output_dir/$output_fname; sed -i '' 's/\&#195;\&#177;/ñ/g' $output_dir/$output_fname\n");
	# other special chars removed...
	print "Run sed -i '' 's/\&#[0-9]*;//g' to remove other special characters from XML file $output_dir/$output_fname...\n";
	system("cp $input_dir/$input_fname $output_dir/$output_fname; sed -i '' 's/\&#[0-9]*;//g' $output_dir/$output_fname\n");
}
