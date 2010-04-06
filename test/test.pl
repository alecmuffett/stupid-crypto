#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;
use Carp;
use File::Slurp;
use IPC::Run qw(run);

$| = 1;

my $language;
my $quietbuild;

croak if !GetOptions("language=s" => \$language,
                     "quietbuild" => \$quietbuild);

opendir(D, '.') || croak "Can't open .: $!";
my @tests = sort grep { /\.stupid$/ } readdir(D);
closedir(D);

my $failed = 0;
my $skipped = 0;
my $ran = 0;
my $passed = 0;
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
	my $buildredirect="";
	if($quietbuild) {
		$buildredirect=">/dev/null 2>/dev/null";
	}
	if(system("./build-$language.sh $base $buildredirect")) {
	    $status = '-BUILD-FAIL';
	    $output = '';
	} else {
	    my @cmd;
	    $cmd[0] = "generated/$language/$base";
	    my $err;
	    my $ok = run \@cmd, \undef, \$output, \$err;
	    if($ok) {
		$status = '';
	    } else {
		$status = '-RUN-FAIL';
		$output = $err;
	    }
	}
	if($expect_status ne $status) {
	    print "FAIL (expected status $expect_status, got $status with output $output)";
	    ++$failed;
	} elsif($expect_output ne $output) {
	    print "FAIL (expected '$expect_output', got '$output')";
	    ++$failed;
	} else {
	    print 'OK';
	    ++$passed;
	}
    } else {
	print 'skip';
	++$skipped;
    }
    print "\n";
}

print "Ran $ran tests, $passed passed, $failed failed, $skipped skipped.\n";
