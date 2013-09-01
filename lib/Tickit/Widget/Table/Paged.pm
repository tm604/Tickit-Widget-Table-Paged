package Tickit::Widget::Table::Paged;
# ABSTRACT: 
use strict;
use warnings;
use parent qw(Tickit::Widget);

our $VERSION = '0.001';

=head1 NAME

Tickit::Widget::Table::Paged - table widget with support for scrolling/paging

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut

use Tickit::RenderBuffer qw(LINE_SINGLE LINE_DOUBLE CAP_BOTH);
use Tickit::Utils qw(distribute substrwidth align textwidth);
use Tickit::Style;
use Scalar::Util qw(looks_like_number);
use POSIX qw(floor);

use constant CLEAR_BEFORE_RENDER => 0;
use constant KEYPRESSES_FROM_STYLE => 1;
use constant CAN_FOCUS => 1;

BEGIN {
	style_definition 'base' =>
		cell_padding         => 1,
		fg                   => 'white',
		highlight_b          => 1,
		highlight_fg         => 'yellow',
		highlight_bg         => 'blue',
		header_b             => 1,
		header_fg            => 'blue',
		scrollbar_fg         => 'white',
		scrollbar_bg         => 'black',
		scrollbar_line_style => 'none',
		scroll_b             => 1,
		scroll_fg            => 'white',
		scroll_bg            => 'black',
		scroll_line_style    => 'block';

	style_definition ':focus' =>
		'<Up>'               => 'previous_row',
		'<Down>'             => 'next_row',
		'<PageUp>'           => 'previous_page',
		'<PageDown>'         => 'next_page',
		'<Home>'             => 'first_row',
		'<End>'              => 'last_row',
		'<Left>'             => 'previous_column',
		'<Right>'            => 'next_column',
		'<Enter>'            => 'activate';
}

=head1 METHODS

=cut

sub lines { 1 }
sub cols { 1 }

=pod

align:
* left => 0
* center|centre|middle => 0.5
* right => 1

=cut

sub vscroll { 1 }
sub hscroll { 0 }
sub row_offset { shift->{row_offset} ||= 0 }

sub visible_lines {
	my $self = shift;
	return @{$self->{data}}[$self->row_offset..$self->row_offset + $self->scroll_dimension];
}

sub header_rect {
	my $self = shift;
	$self->{header_rect} ||= Tickit::Rect->new(
		top => 0, lines => 1, left => 0, cols => $self->window->cols
	);
}

sub body_rect {
	my $self = shift;
	$self->{body_rect} ||= Tickit::Rect->new(
		top => 1, lines => $self->window->lines - 1, left => 0, cols => $self->window->cols - 1
	);
}

sub scrollbar_rect {
	my $self = shift;
	$self->{scrollbar_rect} ||= Tickit::Rect->new(
		top => 1, lines => $self->window->lines - 1, left => $self->window->cols - 1, cols => 1,
	);
}

sub render_header {
	my ($self, $rb, $rect) = @_;
	my $win = $self->window;
	$rb->goto(0, 0);
	for (@{$self->{column_layout}}) {
		if(($_->{type} // '') eq 'padding') {
			$rb->text(' ' x $_->{value}, $self->get_style_pen('padding'));
		} else {
			$_->{style} = $self->get_style_pen('header');
			local $_->{text} = $_->{label} // '';
			$self->render_cell($rb, $_);
		}
	}
	$rb->text(' ', $self->get_style_pen('header')) if $self->vscroll;
}

sub render_to_rb {
	my ($self, $rb, $rect) = @_;
	my $win = $self->window;
	$self->{highlight_row} ||= 0;

	$self->render_header($rb, $rect) if $rect->intersects($self->header_rect);
	$self->render_body($rb, $rect) if $rect->intersects($self->body_rect);
	$self->render_scrollbar($rb, $rect) if $rect->intersects($self->scrollbar_rect);
	my $highlight_pos = 1 + $self->highlight_visible_row;
	$win->cursor_at($highlight_pos, 0);
}

sub render_body {
	my ($self, $rb, $rect) = @_;
	return $self unless my $win = $self->window;
	my $highlight_pos = 1 + $self->highlight_visible_row;
	my @visible = (undef, $self->visible_lines);
	LINE:
	for my $y ($rect->linerange) {
		next LINE unless $y;
		next LINE if $y >= @visible;
		next LINE unless $visible[$y];
		$rb->goto($y, 0);
		my @col_text = @{$visible[$y]};
		CELL:
		for (@{$self->{column_layout}}) {
			if($_->{type} eq 'widget') {
				$rb->skip($_->{value});
				my $w = $_->{attached_widget}[$y];
				$w->set_style_tag('highlight' => ($y == $highlight_pos) ? 1 : 0);
				(shift @col_text)->($w);
				next CELL;
			}

			if($_->{type} eq 'padding') {
				$rb->text(' ' x $_->{value}, $self->get_style_pen('padding'));
			} else {
				$_->{style} = $self->get_style_pen(($y == $highlight_pos) ? 'highlight' : undef);
				local $_->{text} = shift @col_text;
				$self->render_cell($rb, $_);
			}
		}
	}
}

sub apply_column_widget {
	my $self = shift;
	return $self unless my $win = $self->window;
	for my $y (1..$win->lines-1) {
		CELL:
		for (@{$self->{column_layout}}) {
			next CELL unless $_->{type} eq 'widget';

			my $sub = $win->make_sub($y, $_->{start}, 1, $_->{value});
			$_->{attached_widget}[$y] = $_->{factory}->($sub);
		}
	}
	$self;
}

sub render_scrollbar {
	my ($self, $rb, $rect) = @_;
	my $win = $self->window;
	my $cols = $win->cols;
	--$cols if $self->vscroll;

	my $h = $win->lines - 1;
	if(my ($min, $max) = map 1 + $_, $self->scroll_rows) {
		# Scrollbar should be shown, since we don't have all rows visible on the screen at once
		$rb->vline_at(1, $min - 1, $cols, LINE_SINGLE, $self->get_style_pen('scrollbar'), CAP_BOTH) if $min > 1;
		$rb->vline_at($min, $max, $cols, LINE_DOUBLE, $self->get_style_pen('scroll'), CAP_BOTH);
		$rb->vline_at($max + 1, $h, $cols, LINE_SINGLE, $self->get_style_pen('scrollbar'), CAP_BOTH) if $max < $h;
	} else {
		# Placeholder scrollbar - just render it as empty
		$rb->vline_at(1, $h, $cols, LINE_SINGLE, $self->get_style_pen('scrollbar'), CAP_BOTH);
	}
}

sub render_cell {
	my $self = shift;
	my $rb = shift;
	my $args = shift;

	# Trim first
	my $txt = textwidth($args->{text}) > $args->{value}
	   ? substrwidth $args->{text}, 0, $args->{value}
	   : $args->{text};

	my ($pre, undef, $post) = align textwidth($txt), $args->{value}, $args->{align};
	$rb->text(' ' x $pre, $args->{style}) if $pre;
	$rb->text($txt, $args->{style});
	$rb->text(' ' x $post, $args->{style}) if $post;
	return $self;
}

sub reshape {
	my $self = shift;
	delete @{$self}{qw(body_rect header_rect scrollbar_rect)};
	$self->SUPER::reshape(@_);
	$self->distribute_columns;
	$self->apply_column_widget;
}

sub distribute_columns {
	my $self = shift;
	my $pad = $self->get_style_values('cell_padding');
	my @spacing = @{$self->{columns}};
	(undef, @spacing) = map {;
		+{
			base => $pad,
			expand => 0,
			type => 'padding'
		},
		$_
	} @spacing if $pad;
	my $cols = $self->window->cols;
	--$cols if $self->vscroll;
	distribute $cols, @spacing;
	if($self->{column_layout}) {
		CELL:
		for my $cell (@{$self->{column_layout}}) {
			next CELL unless $cell->{type} eq 'widget';
			$_->set_window(undef) for grep defined, @{$cell->{attached_widget}};
		}
	}
	$self->{column_layout} = \@spacing;
	$self
}

sub window_gained {
	my $self = shift;
	$self->SUPER::window_gained(@_);
	my $win = $self->window;
	$win->set_expose_after_scroll(1) if $win->can('set_expose_after_scroll');
# reshape will do this already
#	$self->distribute_columns;
#	$self->apply_column_widget;
}

sub expose_rows {
	my $self = shift;
	return $self unless my $win = $self->window;
	my $cols = $win->cols;
	map Tickit::Rect->new(
		top => $_,
		left => 0,
		lines => 2,
		cols => $cols
	), @_;
}

sub highlight_row {
	my $self = shift;
	return $self->{highlight_row};
}

sub highlight_visible_row {
	my $self = shift;
	return $self->{highlight_row} - $self->row_offset;
}

=pod

row_offset - number of rows protruding off the top of the screen


=cut

sub key_previous_row {
	my $self = shift;
	return $self unless my $win = $self->window;
	return $self if $self->{highlight_row} <= 0;

	return $self->move_highlight(1) if $self->highlight_visible_row >= 1;
	return $self->scroll_highlight(1);
}

sub scroll_highlight {
	my $self = shift;
	my $up = shift;
	return $self unless my $win = $self->window;

	my $old = $self->highlight_visible_row;
	my $redraw_rect = Tickit::RectSet->new;
	$redraw_rect->add($self->active_scrollbar_rect);
	if($up) {
		--$self->{highlight_row};
		--$self->{row_offset};
	} else {
		++$self->{highlight_row};
		++$self->{row_offset};
	}
	$redraw_rect->add($self->active_scrollbar_rect->translate($up ? -1 : 1, 0));
	$redraw_rect->add($_) for $self->expose_rows($old, $self->highlight_visible_row);
	$win->scrollrect(1, 0, $win->lines, $win->cols, $up ? -1 : 1, 0);
	$win->expose($_) for map $_->translate($up ? 1 : -1, 0), $redraw_rect->rects;
}

sub move_highlight {
	my $self = shift;
	my $up = shift;
	return $self unless my $win = $self->window;
	my $old = $self->highlight_visible_row;
	if($up) {
		--$self->{highlight_row};
	} else {
		++$self->{highlight_row};
	}
	$win->expose($_) for $self->expose_rows($old, $self->highlight_visible_row);
	$self
}

sub key_next_row {
	my $self = shift;
	return $self unless my $win = $self->window;
	return $self if $self->{highlight_row} >= $self->row_count - 1;

	return $self->move_highlight(0) if $self->highlight_visible_row < $win->lines - 2;
	return $self->scroll_highlight(0);
}

sub key_first_row {
	my $self = shift;
	$self->{highlight_row} = 0;
	$self->{row_offset} = 0;
	$self->redraw;
}

sub key_last_row {
	my $self = shift;
	$self->{highlight_row} = $self->row_count - 1;
	$self->{row_offset} = $self->row_count > $self->scroll_dimension ? -1 + $self->row_count - $self->scroll_dimension : 0;
	$self->redraw;
}

sub key_previous_page {
	my $self = shift;
	$self->{highlight_row} -= $self->scroll_dimension;
	$self->{row_offset} -= $self->scroll_dimension;
	$self->{highlight_row} = 0 if $self->{highlight_row} < 0;
	$self->{row_offset} = 0 if $self->{row_offset} < 0;
	$self->redraw;
}

sub key_next_page {
	my $self = shift;
	$self->{highlight_row} += $self->scroll_dimension;
	$self->{row_offset} += $self->scroll_dimension;
	$self->{highlight_row} = $self->row_count - 1 if $self->{highlight_row} > $self->row_count - 1;
	my $max = $self->row_count > $self->scroll_dimension ? -1 + $self->row_count - $self->scroll_dimension : 0;
	$self->{row_offset} = $max if $self->{row_offset} > $max;
	$self->redraw;
}

sub scroll_position { shift->{row_offset} }
sub row_count { scalar @{shift->{data}} }

sub sb_height {
	my $self = shift;
	my $ext = $self->scroll_dimension;
	my $max = $self->row_count - $ext;
	return floor(0.5 + ($ext * $ext / $max));
}

sub scroll_rows {
	my $self = shift;
	my $cur = $self->scroll_position;
	my $ext = $self->scroll_dimension;
	my $max = $self->row_count - $ext;
	return if $max < $ext;
	my $y = floor(0.5 + ($cur * ($ext - $self->sb_height) / $max));
	return $y, $y + $self->sb_height;
}

sub active_scrollbar_rect {
	my $self = shift;
	return unless my ($start, $end) = $self->scroll_rows;
	Tickit::Rect->new(
		top => 1 + $start,
		bottom => 2 + $end,
		left => $self->window->cols - 1,
		cols => 1,
	);
}

sub scroll_dimension {
	my $self = shift;
	return 1 unless my $win = $self->window;
	$win->lines - 2;
}

sub clear {
	my $self = shift;
	$self->{data} = [];
	$self->{highlight_row} = 0;
	$self->redraw;
	$self
}

sub add_row {
	my $self = shift;
	push @{$self->{data}}, [ @_ ];
	$self
}

my %ALIGNMENT_TYPE = (
	left   => 0,
	right  => 1,
	centre => 0.5,
	center => 0.5,
	middle => 0.5,
);

sub add_column {
	my $self = shift;
	my %args = @_;
	# delete $args{width} if $args{width} eq 'auto';
	@args{qw(base expand)} = (0,1) unless exists $args{width};
	$args{fixed} = delete $args{width} if looks_like_number($args{width});
	$args{type} ||= 'text';
	$args{align} = $ALIGNMENT_TYPE{$args{align} || 0};
	push @{$self->{columns}}, \%args;
	$self
}

sub key_next_column { }
sub key_previous_column { }
sub key_first_column { }
sub key_last_column { }

sub on_key { warn "on_key with @_\n"; 0 }

sub key_activate {
	my $self = shift;
	warn "activate!";
	$self->{on_activate}->(
		$self->highlight_row,
		$self->{data}[$self->highlight_row]
	) if $self->{on_activate};
	$self
}

1;

__END__

=head1 SEE ALSO

=over 4

=item * L<Tickit::Widget::Table>

=item * L<Text::ANSITable>

=back

=head1 AUTHOR

Tom Molesworth <cpan@entitymodel.com>

=head1 LICENSE

Copyright Tom Molesworth 2012-2013. Licensed under the same terms as Perl itself.

