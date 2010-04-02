#!/usr/bin/env -S perl -w

use strict;

use Getopt::Long;
use Carp;
use File::Slurp;
use IPC::Run qw(run);

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
