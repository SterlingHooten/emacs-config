#!/usr/bin/env perl

## Author: Emilio C. Lopes

use strict;
use warnings;

use Carp;
use Carp::Assert;

use Getopt::Long;
use Pod::Usage;

use Data::Dumper;

use Text::Wrap;

use File::Basename;

use constant PROGNAME => basename $0;
use constant VERSION  => '1.0';

sub get_options {
  my $config_ref = shift;

  local $SIG{__WARN__} =
    sub {
      my $warning = shift;

      chomp $warning;
      $warning = lcfirst $warning;

      if ($warning =~ m|^unknown option:\s*(.*)$|) {
        my $option = $1;
        $option = "--$option" if length $option >= 2;
        $option = "-$option" if length $option == 1;
        $warning = "unknown option: '$option'";
      }
      fatal_error("$warning\n");
    };

  Getopt::Long::Configure('no_ignore_case');
  GetOptions('h|help' => \&show_help,
             'version' => \&show_version,
             'v|verbose' => \$config_ref->{verbose});
  $SIG{__WARN__} = 'DEFAULT';
}

sub format_message {
  local $Text::Wrap::columns = 70;

  return wrap(PROGNAME . ": ", PROGNAME . ":   ", @_) . "\n";
}

sub fatal_error {
  die format_message(@_);
}

sub warning {
  warn format_message(@_);
}


sub show_help {
  pod2usage(-verbose => 1,
            -exitval => 0,
            -input => \*main::DATA);
}

sub show_version {
  print PROGNAME, ", version ", VERSION, "\n";
  exit 0;
}

sub main {
  my @args = @_;

  local @ARGV = @args;

  my %config = ();

  get_options(\%config);

  return 0;
}

exit main (@ARGV);

__DATA__

=pod

=head1 NAME

=head1  SYNOPSIS

B<PROGNAME> [I<options>]

=head1  OPTIONS

=over 8

=item B<--verbose|-v>

Verbose output.

=item B<--help|-h>

display this help and exit.

=item B<--version>

output version information and exit.

=back

=head1  DESCRIPTION

=cut
