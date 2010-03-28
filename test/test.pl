#!/usr/bin/env -S perl -w

use strict;

use Getopt::Long;
use Carp;
use File::Slurp;

$| = 1;

my $language;

croak if !GetOptions("language=s" => \$language);

opendir(D, '.') || croak "Can't open .: $!";
my @tests = sort grep { /\.stupid$/ } readdir(D);
closedir(D);

my $failed = 0;
my $skipped = 0;
my $ran = 0;
for my $test (@tests) {
    ++$ran;
    my($base) = $test =~ /^(.*)\.stupid$/;
    print "$base...";
    my $code = read_file("$test");
    if($code =~ /^"EXPECT([^:]*):([^"]*)"/m) {
	my $expect_status = $1;
	my $expect_output = $2;
	my $status;
	my $output;
	if(system("./build-$language.sh $base")) {
	    $status = '-BUILD-FAIL';
	    $output = '';
	} else {
	    open(my $f, "generated/$language/$base |");
	    $status = '';
	    $output = read_file($f);
	    close $f;
	}
	if($expect_status ne $status) {
	    print "FAIL (expected status $expect_status, got $status)";
	    ++$failed;
	} elsif($expect_output ne $output) {
	    print "FAIL (expected '$expect_output', got '$output')";
	    ++$failed;
	} else {
	    print 'OK';
	}
    } else {
	print 'skip';
	++$skipped;
    }
    print "\n";
}

print "Ran $ran tests, $failed failed, $skipped skipped.\n";
