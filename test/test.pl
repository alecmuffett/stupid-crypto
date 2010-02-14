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
    if($code =~ /^"EXPECT:([^"]*)"/m) {
	my $expect = $1;
	system "./build-$language.sh $base|";
	open(my $f, "generated/$base |");
	my $got = read_file($f);
	close $f;
	if($expect eq $got) {
	    print 'OK';
	} else {
	    print "FAIL (expected '$expect', got '$got')";
	}
    } else {
	print 'skip';
    }
    print "\n";
}


