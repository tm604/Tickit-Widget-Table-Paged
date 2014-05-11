package Tickit::Widget::Table::Adapter::Array;

use strict;
use warnings;
use parent qw(Tickit::Widget::Table::Adapter);

=head1 NAME

Tickit::Widget::Adapter::Array - arrayref adapter for L<Tickit::Widget::Table>

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut

=head1 new

Instantiates a new array adapter.

This will start off with the items passed as arguments, so calling
L</new> without parameters will result in an empty arrayref.

=cut

sub new {
	my $class = shift;
	bless [ @_ ], $class
}

sub clear {
	my $self = shift;
	@$self = ();
	$self
}

sub append {
	my $self = shift;
	push @$self, [ @_ ];
	$self
}

sub modify {
	my ($self, $idx, @cols) = @_;
	die "row out of bounds" unless @$self >= $idx;
	$self->[$idx][$_] = $cols[$_] for 0..$#cols;
	$self
}

sub delete {
	my ($self, $idx) = @_;
	splice @$self, $idx, 1;
	$self;
}

=head1 count

=cut

sub count {
	my $self = shift;
	scalar @$self
}

=head1 get

=cut

sub get {
	my $self = shift;
	my $idx = shift;
	# die "$idx is out of bounds for " . $#$self unless $#$self >= $idx;
	$self->[$idx]
}

1;

__END__

=head1 AUTHOR

Tom Molesworth <cpan@entitymodel.com>

=head1 LICENSE

Copyright Tom Molesworth 2013-2014. Licensed under the same terms as Perl itself.


