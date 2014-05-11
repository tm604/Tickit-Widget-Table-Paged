package Tickit::Widget::Table::Adapter;

use strict;
use warnings;

=head1 NAME

Tickit::Widget::Adapter - base adapter class for L<Tickit::Widget::Table>

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut

sub count { }

sub get { }

sub slice {
	my ($self, $idx, $length) = @_;
	grep defined, map $self->get($_), $idx..($idx + $length);
}

sub mark_visible { }

sub mark_hidden { }

sub new { my $class = shift; bless { @_ }, $class }

1;

__END__

=head1 AUTHOR

Tom Molesworth <cpan@entitymodel.com>

=head1 LICENSE

Copyright Tom Molesworth 2013-2014. Licensed under the same terms as Perl itself.

