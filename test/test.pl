#!/usr/bin/perl -w

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

for my $test (@tests) {
    my($base) = $test =~ /^(.*)\.stupid$/;
    print "$base...";
    my $code = read_file("$test");
    if($code =~ /^"EXPECT([^:]*):([^"]*)"/m) {
	my $expect_status = $1;
	my $expect_output = $2;
	if(system("./build-$language.sh $base")) {
	    if($expect_status eq '-BUILD-FAIL') {
		print 'OK';
	    } else {
		print "BUILD FAIL";
	    }
	} else {
	    if($expect_status eq '') {
		open(my $f, "generated/$base |");
		my $got = read_file($f);
		close $f;
		if($expect_output eq $got) {
		    print 'OK';
		} else {
		    print "FAIL (expected '$expect_output', got '$got')";
		}
	    } else {
		print "FAIL (expected status $expect_status, got build success)";
	    }
	}
    } else {
	print 'skip';
    }
    print "\n";
}


