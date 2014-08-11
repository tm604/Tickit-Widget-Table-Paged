package Tickit::Widget::Table::Paged;
# ABSTRACT: a table widget for larger datasets
use strict;
use warnings;
use parent qw(Tickit::Widget);

our $VERSION = '0.005';

=head1 NAME

Tickit::Widget::Table::Paged - table widget with support for scrolling/paging

=head1 SYNOPSIS

 use Tickit;
 use Tickit::Widget::Table::Paged;

 my $tbl = Tickit::Widget::Table::Paged->new;
 $tbl->add_column(
 	label => 'Left',
 	align => 'left',
 	width => 8,
 );
 $tbl->add_column(
 	label => 'Second column',
 	align => 'centre'
 );
 $tbl->add_row('left', 'middle') for 1..100;
 Tickit->new(root => $tbl)->run;

=head1 DESCRIPTION

B<WARNING>: This is a preview release. API is subject to change in future,
please get in contact if you're using this, or wait for version 1.000.

=begin HTML

<p><img src="http://tickit.perlsite.co.uk/cpan-screenshot/tickit-widget-table-paged1.gif" alt="Paged table widget in action" width="430" height="306"></p>

=end HTML

This widget provides a scrollable table implementation for use on larger data
sets. Rather than populating the table with values, you provide an adapter
which implements the C<count> and C<get> methods, and the table widget will
query the adapter for the current "page" of values.

This abstraction should allow access to larger datasets than would fit in
available memory, such as a database table or procedurally-generated data.

See L<Adapter::Async::OrderedList::Array> if your data is stored in a Perl
array. Other subclasses may be available if you have a different source.

=head2 Transformations

Apply to:
* Row
* Column
* Cell

Row:

This takes the original data item for the row, and returns one of the following:

* Future - when resolved, the items will be used as cells
* Arrayref - holds the cells directly

The data item can be anything - an array-backed adapter would return an arrayref, ORM will give you an object for basic collections.

Any number of cells may be returned from a row transformation, but you may get odd results if the cell count is not consistent.

An array adapter needs no row transformation, due to the arrayref behaviour. You could provide a Future alternative:

 $row->apply_transformation(sub {
  my ($item) = @_;
  Future->wrap(
   @$item
  )
 });

For the ORM example, something like this:

 $row->apply_transformation(sub {
  my ($item) = @_;
  Future->wrap(
   map $item->$_, qw(id name created)
  )
 });

Column:

Column transformations are used to apply styles and formats.

You get an input value, and return either a string or a Future.

Example date+colour transformation on column:

 $col->apply_transformation(sub {
  my $v = shift;
  Future->wrap(
   String::Tagged->new(strftime '%Y-%m-%d', $v)
   ->apply_tag(0, 4, b => 1)
   ->apply_tag(5, 1, fg => 8)
   ->apply_tag(6, 2, fg => 4)
   ->apply_tag(9, 1, fg => 8)
  );
 });

Cell transformations are for cases where you need fine control over individual components. They operate similarly to column transformations,
taking the input value and returning either a string or a Future.

Typical example would be a spreadsheet:

 $cell->apply_transformation(sub {
  my $v = shift;
  return $v unless blessed $v;
  return eval $v if $v->is_formula;
  return $v->to_string if $v->is_formatted;
  return "$v"
 });



=cut

use Tickit::RenderBuffer qw(LINE_SINGLE LINE_DOUBLE CAP_BOTH);
use Tickit::Utils qw(distribute substrwidth align textwidth chars2cols);
use String::Tagged;
use Future::Utils qw(fmap_void repeat);
use Tickit::Style;
use Scalar::Util qw(looks_like_number blessed);
use POSIX qw(floor);

use Adapter::Async::OrderedList;
use Adapter::Async::OrderedList::Array;

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
		selected_b           => 1,
		selected_fg          => 'white',
		selected_bg          => 'red',
		header_b             => 1,
		header_fg            => 'blue',
		scrollbar_fg         => 'white',
		scrollbar_bg         => 'black',
		scrollbar_line_style => 'none',
		scroll_b             => 1,
		scroll_fg            => 'white',
		scroll_bg            => 'black',
#		scroll_line_style    => 'block';
# Technically we should ignore any keyboard input if we don't have focus,
# but other widgets don't currently do this and things seem to work without
# it anyway.
#	style_definition ':focus' =>
		'<Up>'               => 'previous_row',
		'<Down>'             => 'next_row',
		'<PageUp>'           => 'previous_page',
		'<PageDown>'         => 'next_page',
		'<Home>'             => 'first_row',
		'<End>'              => 'last_row',
		'<Left>'             => 'previous_column',
		'<Right>'            => 'next_column',
		'<Space>'            => 'select_toggle',
		'<Enter>'            => 'activate';
}

# Allow more descriptive terms for column alignment - these
# map to the values allowed by the Tickit::Utils::align series
# of functions.
my %ALIGNMENT_TYPE = (
	left   => 0,
	right  => 1,
	centre => 0.5,
	center => 0.5,
	middle => 0.5,
);

=head1 METHODS

=cut

=head2 new

Instantiate. Will attempt to take focus.

Takes the following named parameters:

=over 4

=item * on_activate - coderef to call when the user hits the Enter key,
will be passed the highlighted row or selection when in C<multi_select> mode,
see L</on_activate> for more details.

=item * multi_select - when set, the widget will allow selection of multiple
rows (typically by pressing Space to toggle a given row)

=item * adapter - an L<Adapter::Async::OrderedList::Array> instance

=item * data - alternative to passing an adapter, if you want to wrap an existing
array without creating an L<Adapter::Async::OrderedList> subclass yourself

=back

Returns a new instance.

=cut

sub new {
	my $class = shift;
	my %args = @_;
	my %attr;
	$attr{$_} = delete $args{$_} for qw(
		on_activate
		multi_select
		adapter
		item_transformations
		cell_transformations
		columns
		highlight_row
	);
	my $self = $class->SUPER::new(@_);

	# First we assign the adapter, since it might be used elsewhere
	$attr{adapter} ||= Adapter::Async::OrderedList::Array->new(
		data => $attr{data} || []
	);
	$self->on_adapter_change(delete $attr{adapter});

	# Special-case parameters which need method calls
	$self->on_activate(delete $attr{on_activate}) if $attr{on_activate};
	$self->multi_select(delete $attr{multi_select} || 0);

	# Some defaults
	$attr{item_transformations} ||= [ ];
	$attr{cell_transformations} ||= { };
	$attr{columns} ||= [];
	$attr{highlight_row} //= 0;

	# Apply our attributes now
	$self->{$_} = $attr{$_} for keys %attr;

	$self->take_focus;
	$self
}

sub adapter { shift->{adapter} }

=head2 bus

Bus for event handling. Normally an L<Adapter::Async::Bus> instance
shared by the adapter.

=cut

sub bus { $_[0]->{bus} ||= $_[0]->adapter->bus }

=head1 METHODS - Table content

=head2 clear

Clear all data in the table.

=cut

sub clear {
	my $self = shift;
	# Let our event handler take care of any required cleanup here
	$self->adapter->clear;
	$self
}

=head2 expose_row

Expose the given row (provided as an index into the underlying storage).

 $tbl->expose_row(14);

=cut

sub expose_row {
	my ($self, $idx) = @_;
	if(my $win = $self->window) {
		my $row = $self->row_from_idx($idx);
		return $self unless $row >= 0;
		my $rect = Tickit::Rect->new(
			top   => $row,
			left  => 0,
			lines => 1,
			cols  => $win->cols
		)->intersect($self->body_rect);
		$win->expose($rect) if $rect;
	}
	return $self;
}

=head2 add_column

Add a new column. Takes the following named parameters:

=over 4

=item * width - (optional) number of columns

=item * type - (optional) data type, currently only supports 'text' (the default)

=item * align - (optional) align left, center or right

=item * transform - (optional) list of transformations to apply

=item * visible - (optional) true if this column should be shown

=back

Returns $self.

=cut

sub add_column {
	my $self = shift;
	my %args = @_;
	# delete $args{width} if $args{width} eq 'auto';
	@args{qw(base expand)} = (0,1) unless exists $args{width};
	$args{fixed} = delete $args{width} if looks_like_number($args{width});
	$args{type} ||= 'text';
	$args{align} = $ALIGNMENT_TYPE{$args{align}} if defined($args{align}) && exists $ALIGNMENT_TYPE{$args{align}};
	$args{align} ||= 0;
	$args{visible} //= 1;
	$args{transform} ||= [];
	$args{transform} = [ $args{transform} ] unless ref $args{transform} eq 'ARRAY';
	push @{$self->{columns}}, \%args;
	$self
}

=head2 selected_rows

Returns the selected row, or multiple rows as a list if multi_select is enabled.
If multi_select is enabled it does not return the row currently highlighted (unless that row is also selected).

=cut

sub selected_rows {
	my $self = shift;

	if($self->multi_select) {
		my @selected = sort { $a <=> $b } grep $self->{selected}{$_}, keys %{$self->{selected}};
		return @{$self->data}[@selected];
	} else {
		my $idx = $self->highlight_row;
		return $self->data->[$idx];
	}
}

=head1 METHODS - Callbacks

=head2 on_activate

Accessor for the activation callback - if called without parameters,
will return the current coderef (if any), otherwise, will set the new
callback.

This callback will be triggered via L</key_activate>:

 $code->($row_index, $row_data_as_arrayref)

If multiselect is enabled, the callback will have the following:

 $code->(
   [$highlight_row_index, @selected_row_indices],
   $highlight_row_data_as_arrayref,
   @selected_rows_as_arrayrefs
 )

(the selected row data + index list could be empty here)

=cut

sub on_activate {
	my $self = shift;
	if(@_) {
		$self->{on_activate} = shift;
		return $self;
	}
	return $self->{on_activate}
}

=head2 multi_select

Accessor for multi_select mode - when set, this allows multiple rows
to be selected.

=cut

sub multi_select {
	my $self = shift;
	if(@_) {
		$self->{multi_select} = shift;
		return $self;
	}
	return $self->{multi_select} ? 1 : 0
}

=head1 METHODS - Other

=head2 lines

Number of lines to request.

=cut

sub lines { 1 }

=head2 cols

Number of columns to request.

=cut

sub cols { 1 }

=head2 vscroll

True if there's a vertical scrollbar (currently there is no way to
disable this scrollbar).

=cut

sub vscroll { 1 }

=head2 hscroll

True if there's a horizontal scrollbar. There isn't one, this always
returns false.

=cut

sub hscroll { 0 }

=head2 row_offset

Current row offset (vertical scroll position).

=cut

sub row_offset { shift->{row_offset} //= 0 }

=head2 header_rect

Returns the L<Tickit::Rect> representing the header area.

=cut

sub header_rect {
	my $self = shift;
	$self->{header_rect} ||= Tickit::Rect->new(
		top   => 0,
		lines => $self->header_lines,
		left  => 0,
		cols  => $self->window->cols
	);
}

=head2 body_rect

Returns the L<Tickit::Rect> representing the body area.

=cut

sub body_rect {
	my $self = shift;
	$self->{body_rect} ||= Tickit::Rect->new(
		top   => 1,
		lines => $self->window->lines - 1,
		left  => 0,
		cols  => $self->window->cols - 1
	);
}

=head2 scrollbar_rect

Returns the L<Tickit::Rect> representing the scroll bar.

=cut

sub scrollbar_rect {
	my $self = shift;
	$self->{scrollbar_rect} ||= Tickit::Rect->new(
		top   => 1,
		lines => $self->window->lines - 1,
		left  => $self->window->cols - 1,
		cols  => 1,
	);
}

=head2 header_lines

Returns the number of lines in the header. Hardcoded to 1.

=cut

sub header_lines { 1 }

=head2 body_lines

Returns the number of lines in the body.

=cut

sub body_lines { $_[0]->window->lines - $_[0]->header_lines }

=head2 body_cols

Returns the number of columns in the body.

=cut

sub body_cols { $_[0]->window->cols - 1 }

=head2 idx_from_row

Returns a storage index from a body row index.

=cut

sub idx_from_row {
	my ($self, $row) = @_;
	return $self->row_offset + $row - $self->header_lines;
}

=head2 row_from_idx

Returns a body row index from a storage index.

=cut

sub row_from_idx {
	my ($self, $idx) = @_;
	return $self->header_lines + $idx - $self->row_offset;
}

=head2 row_cache_idx

Returns a row cache offset from a storage index.

=cut

sub row_cache_idx {
	my ($self, $idx) = @_;
	die "no window yet" unless $self->window;
	return $self->body_lines + $idx - $self->row_offset;
}

=head2 idx_from_row_cache

Returns a storage index from a row cache offset.

=cut

sub idx_from_row_cache {
	my ($self, $row) = @_;
	return $row + $self->row_offset - $self->body_lines;
}

=head2 highlight_row

Returns the index of the currently-highlighted row.

=cut

sub highlight_row {
	my $self = shift;
	return $self->{highlight_row};
}

=head2 highlight_visible_row

Returns the position of the highlighted row taking scrollbar into account.

=cut

sub highlight_visible_row {
	my $self = shift;
	return $self->{highlight_row} - $self->row_offset;
}

sub loading_message { 'Loading...' }

=head1 METHODS - Rendering

=head2 render_to_rb

Render the table. Called from expose events.

=cut

sub render_to_rb {
	my ($self, $rb, $rect) = @_;
	my $win = $self->window;
	$self->{highlight_row} ||= 0;

	$self->render_header($rb, $rect);
	$self->render_body($rb, $rect);
	$self->render_scrollbar($rb, $rect) if $self->vscroll;
	my $highlight_pos = 1 + $self->highlight_visible_row;
	$win->cursor_at($highlight_pos, 0);
}

=head2 render_header

Render the header area.

=cut

sub render_header {
	my ($self, $rb, $rect) = @_;

	$rect = $rect->intersect($self->header_rect)
		or return $self;

	$rb->goto(0, 0);
	for my $col (0..$#{$self->{columns}}) {
		my $def = $self->{columns}[$col];
		$self->render_header_cell($rb, $def);
	}
	$rb->erase_to($self->window->cols, $self->get_style_pen('padding'));
}

=head2 render_header_cell

Render a specific header cell.

=cut

sub render_header_cell {
	my ($self, $rb, $def) = @_;
	my $base_pen = $self->get_style_pen(
		'header'
	);
	$rb->erase_to($def->{start}, $base_pen);
	my ($pre, undef, $post) = align textwidth($def->{label} // ''), $def->{value}, $def->{align};
	$rb->erase($pre, $base_pen) if $pre;
	$rb->text($def->{label} // '', $base_pen);
	$rb->erase($post, $base_pen) if $post;
}

=head2 render_scrollbar

Render the scrollbar.

=cut

sub render_scrollbar {
	my ($self, $rb, $rect) = @_;
	return $self unless my $win = $self->window;

	$rect = $rect->intersect($self->scrollbar_rect)
		or return $self;

	my $cols = $win->cols - 1;
	my $h = $win->lines - 1;

	# Need to clear any line content first, since we may be overwriting part of
	# the previous scrollbar rendering here
	$rb->eraserect(
		Tickit::Rect->new(
			top => 1,
			left => $cols,
			right => $cols,
			bottom => $h,
		)
	);
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

=head2 render_body

Render the table body.

=cut

sub render_body {
	my ($self, $rb, $rect) = @_;
	return $self unless my $win = $self->window;

	# Make sure we only step through the parts of
	# the expose event that relate to the body
	# area
	$rect = $rect->intersect($self->body_rect)
		or return $self;

	for my $line ($rect->linerange) {
		my $idx = $self->idx_from_row($line);
		my $f = $self->row_cache($idx);
		if($f->is_done) {
			$self->render_row($rb, $rect, $idx, $f->get);
		} elsif($f->is_ready) {
			$self->render_failed_row($rb, $rect, $idx, $f->is_cancelled ? 'cancelled' : $f->get);
		} else {
			$self->render_pending_row($rb, $rect, $idx);
			$f->on_done($self->curry::expose_row($idx));
		}
	}
}

=head2 render_row

Renders a given row, using storage index.

=cut

sub render_row {
	my ($self, $rb, $rect, $row, $data) = @_;

	my $line = $self->row_from_idx($row);
	my $base_pen = $self->get_style_pen(
		($row == $self->highlight_row)
		? 'highlight'
		: ($self->multi_select && $self->{selected}{$line + $self->row_offset - 1})
		? 'selected'
		: undef
	);
	for my $col (0..$#$data) {
		my $v = $self->apply_view_transformations($row, $col, $data->[$col]);
		my $def = $self->{columns}[$col];
		$rb->goto($line, $def->{start});
		my ($pre, undef, $post) = align textwidth($v), $def->{value}, $def->{align};
		$rb->erase($pre, $base_pen) if $pre;
		if(blessed($v) && $v->isa('String::Tagged')) {
			# Copy before modifying, might be overkill?
			my $st = String::Tagged->new($v);
			$st->merge_tags(sub {
				my ($k, $left, $right) = @_;
				return $left eq $right;
			});
			$st->iter_substr_nooverlap(sub {
				my ($substr, %tags) = @_;
				my $pen = Tickit::Pen::Immutable->new(
					$base_pen->getattrs,
					%tags
				);
				$rb->text($substr, $pen);
			});
		} else {
			$rb->text($v, $base_pen);
		}
		$rb->erase($post, $base_pen) if $post;
	}
}

sub render_failed_row {
	my ($self, $rb, $rect, $row, $failure) = @_;

	my $line = $self->row_from_idx($row);
	my $base_pen = $self->get_style_pen(
		($row == $self->highlight_row)
		? 'highlight'
		: ($self->multi_select && $self->{selected}{$line + $self->row_offset - 1})
		? 'selected'
		: 'failed'
	);

		$rb->goto($line, 0);
		my ($pre, undef, $post) = align textwidth($failure), $self->body_cols, 0.5;
		$rb->erase($pre, $base_pen) if $pre;
		$rb->text($failure, $base_pen);
		$rb->erase($post, $base_pen) if $post;
}
sub render_pending_row {
	my ($self, $rb, $rect, $row) = @_;

	my $line = $self->row_from_idx($row);
	my $base_pen = $self->get_style_pen(
		($row == $self->highlight_row)
		? 'highlight'
		: ($self->multi_select && $self->{selected}{$line + $self->row_offset - 1})
		? 'selected'
		: 'pending'
	);

		$rb->goto($line, 0);
		my ($pre, undef, $post) = align textwidth($self->loading_message), $self->body_cols, 0.5;
		$rb->erase($pre, $base_pen) if $pre;
		$rb->text($self->loading_message, $base_pen);
		$rb->erase($post, $base_pen) if $post;
}

=head2 on_scroll

Update row cache to reflect a scroll event.

=cut

sub on_scroll {
	my ($self, $offset) = @_;
	die "undef offset" unless defined $offset;

	# Our row cache is a scrolling fixed-size window over the previous,
	# current and next page, so any removals need to be compensated by
	# empty items at the other end
	my @replace = (undef) x ($offset < 0 ? -$offset : $offset);

	my @removed;
	if($offset > 0) {
		# Scrolling down means we throw away the first N rows
		@removed = splice @{$self->{row_cache}}, 0, $offset;
		push @{$self->{row_cache}}, @replace;
	} else {
		# and in the other direction, last N rows
		@removed = splice @{$self->{row_cache}}, @{$self->{row_cache}} + $offset, -$offset, @replace;
		unshift @{$self->{row_cache}}, @replace;
	}

	# Any items that were still in progress are no longer required, make
	# sure we cancel them to avoid unnecessary work.
	$_->cancel for grep defined($_) && !$_->is_ready, @removed;

	return $self if exists $self->{cache_primer};
	$self->{cache_primer} = 1;
	$self->window->tickit->later(sub {
		# Prime the cache for the missing entries
		$self->row_cache($self->idx_from_row_cache($_)) for grep !defined($self->{row_cache}[$_]), 0..$#{$self->{row_cache}}; 
		delete $self->{cache_primer};
	});
	$self
}

=head2 fold_future

Helper method to apply a series of coderefs to a value.

=cut

sub fold_future {
	my ($self, $prefix, $item, @steps) = @_;
	return Future->wrap($item) unless @steps;
	repeat {
		my $code = shift;
		Future->call(sub { $code->(@$prefix, $item) })->on_done(sub {
			$item = shift
		})
	} foreach => \@steps
}

=head2 row_cache

Row cache accessor.

=cut

sub row_cache {
	my ($self, $row) = @_;
	$self->{row_cache}[$self->row_cache_idx($row)] ||= do {
		$self->adapter->get(
			items => [$row],
		)->then(sub {
			# We have an item from storage. No idea what it is, could be an
			# object, hashref, arrayref... the item transformations will
			# convert it into something usable
			my ($item) = @{ $_[0] };
			return Future->fail('no such element') unless $item;

			# Somewhat tedious way to reduce() a Future chain
			$self->fold_future([ $row ], $item, @{$self->{item_transformations}})
		})->then(sub {
			# Our item is now accessible as an arrayref, start working on the columns
			my $item = shift;
			my @pending;
			for my $col (0..$#{$self->{columns}}) {
				my $cell = $item->[$col];
				push @pending, (
					$self->fold_future([ $row, $col ], $cell, @{$self->{columns}[$col]{transform} || [] })
				)->then(sub {
					# hey look at all these optimisations we're not doing
					$self->fold_future([ $row, $col ], shift, @{$self->{cell_transformations}{"$row,$col"} || []})
				})->on_fail(sub { warn "Fail: @_\n" })
			}
			# our transform at the tail of each Future chain should ensure that we
			# end up with a helpful list of cells for this item. One last thing to
			# do: bundle that back into an arrayref, because Reasons.
			Future->needs_all(@pending)->transform(
				done => sub { [ @_ ] }
			)
		})
	};
}

=head2 apply_view_transformations

Apply the transformations just before we render. Can return anything we know how to render.

=cut

sub apply_view_transformations {
	my ($self, $line, $col, $v) = @_;
	$v = $_->($line, $col, $v) for @{$self->{view_transformations}};
	$v
}

=head2 reshape

Handle reshape requests.

=cut

sub reshape {
	my $self = shift;
	delete @{$self}{qw(body_rect header_rect scrollbar_rect)};
	$self->SUPER::reshape(@_);
	$self->distribute_columns;
}

=head2 distribute_columns

Distribute space between columns.

=cut

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
	$self
}

=head2 window_gained

Called when a window has been assigned to the widget.

=cut

sub window_gained {
	my $self = shift;
	$self->SUPER::window_gained(@_);
	my $win = $self->window;

	# Row cache starts as empty. We should really
	# preserve any previous values here.
	$self->{row_cache} = [
		(undef) x ($self->body_lines * 3)
	];

	# Default anyway in newer versions
	$win->set_expose_after_scroll(1) if $win->can('set_expose_after_scroll');
}

=head2 expose_rows

Expose the given rows.

=cut

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

=head2 scroll_highlight

Update scroll information after changing highlight position.

=cut

sub scroll_highlight {
	my $self = shift;
	my $offset = shift;
	return $self unless my $win = $self->window;

	if($self->highlight_row + $offset < 0) {
		$offset = -$self->highlight_row;
	}
	if($self->highlight_row + $offset > $self->row_count - 1) {
		$offset = $self->row_count - $self->highlight_row;
	}
	return $self unless my $scrollbar_rect = $self->active_scrollbar_rect;
	my $old = $self->highlight_visible_row;

	# FIXME Work out the changed extents on the
	# scrollbar, and just update those - note that
	# T::W::ScrollBar should already have this logic
	# somewhere, as does ProgressBar
	my $redraw_rect = Tickit::RectSet->new;
	$redraw_rect->add($scrollbar_rect);

	$self->{highlight_row} += $offset;
	$self->{row_offset} += $offset;

	$redraw_rect->add($scrollbar_rect->translate($offset, 0));
	$redraw_rect->add($_) for $self->expose_rows($old, $self->highlight_visible_row);

	my $hdr = $self->header_lines;
	$win->scrollrect($hdr, 0, $win->lines - $hdr, $win->cols, $offset, 0);
	$self->on_scroll($offset);
	$win->expose($_) for map $_->translate(-$offset, 0), $redraw_rect->rects;
}

=head2 move_highlight

Move the highlighted row by the given offset (can be negative to move up).

=cut

sub move_highlight {
	my $self = shift;
	my $offset = shift;
	return $self unless my $win = $self->window;

	my $old = $self->highlight_visible_row;
	$self->{highlight_row} += $offset;

	$win->expose($_) for $self->expose_rows($old, $self->highlight_visible_row);
	$self
}

=head2 scroll_position

Current vertical scrollbar position.

=cut

sub scroll_position { shift->{row_offset} }

=head2 row_count

Total number of rows.

=cut

sub row_count {
	my $self = shift;
	$self->{item_count};
}

=head2 sb_height

Current scrollbar height.

=cut

sub sb_height {
	my $self = shift;
	my $ext = $self->scroll_dimension;
	my $max = $self->row_count - $ext;
	return 1 unless $max;
	return floor(0.5 + ($ext * $ext / $max));
}

=head2 scroll_rows

Positions of the scrollbar indicator.

=cut

sub scroll_rows {
	my $self = shift;
	my $cur = $self->scroll_position;
	my $ext = $self->scroll_dimension;
	my $max = $self->row_count - $ext;
	return unless $max;
	my $y = floor(0.5 + ($cur * ($ext - $self->sb_height) / $max));
	return $y, $y + $self->sb_height;
}

=head2 active_scrollbar_rect

Rectangle representing the area covered by the current scrollbar.

=cut

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

=head2 scroll_dimension

Size of the vertical scrollbar.

=cut

sub scroll_dimension {
	my $self = shift;
	return 1 unless my $win = $self->window;
	$win->lines - 2;
}

=head2 on_adapter_change

Applies a new adapter, taking care of any cleanup if there was an
adapter previously active.

Can be passed undef, to remove the adapter completely.

=cut

sub on_adapter_change {
	my ($self, $adapter) = @_;

	if(my $old = $self->{adapter}) {
		$old->bus->unsubscribe_from_event(
			@{$self->{adapter_subscriptions}}
		);
	}

	delete $self->{bus};
	$self->{adapter} = $adapter;
	undef $self->{item_count};
	return $self unless $adapter;

	# Want weakrefs in here, because we're storing the subscriptions
	# for later cleanup. 
	$self->bus->subscribe_to_event(@{
		$self->{adapter_subscriptions} = [
			splice => $self->curry::weak::on_splice_event,
			clear  => $self->curry::weak::on_clear_event,
		]
	});

	$self->adapter->count->on_done(sub {
		$self->{item_count} = shift
	});
	$self
}

=head2 on_splice_event

Invoked by the adapter when data is added to or removed from
the data source.

=cut

sub on_splice_event {
	my ($self, $ev, $idx, $len, $data) = @_;

	my $delta = @$data - $len;

	if(my $win = $self->window) {
		# Row cache update
		my $rc_start = $self->idx_from_row_cache(0);
		my $rc_end = $self->idx_from_row_cache(3 * $self->body_lines - 1);

		$self->scroll_highlight($delta) if $delta;
		$win->expose;
	}

	# Either update our cached count based on
	# the change, or request a new count if we have
	# none yet
	if(defined $self->{item_count}) {
		$self->{item_count} += $delta;
	} else {
		$self->adapter->count->on_done(sub {
			$self->{item_count} = shift
		});
	}
}

=head2 on_clear_event

Called by the adapter when all data has been removed from the
data source.

=cut

sub on_clear_event {
	my ($self, $ev) = @_;
	$self->{highlight_row} = 0;
	$self->{item_count} = 0;
	if(my $win = $self->window) {
		$win->expose;
	}
}

=head1 METHODS - Key bindings

=head2 key_previous_row

Go to the previous row.

=cut

sub key_previous_row {
	my $self = shift;
	return $self unless my $win = $self->window;
	return $self if $self->{highlight_row} <= 0;

	return $self->move_highlight(-1) if $self->highlight_visible_row >= 1;
	return $self->scroll_highlight(-1);
}

=head2 key_next_row

Move to the next row.

=cut

sub key_next_row {
	my $self = shift;
	return $self unless my $win = $self->window;
	return $self if $self->{highlight_row} >= $self->row_count - 1;

	return $self->move_highlight(1) if $self->highlight_visible_row < $win->lines - 2;
	return $self->scroll_highlight(1);
}

=head2 key_first_row

Move to the first row.

=cut

sub key_first_row {
	my $self = shift;
	$self->{highlight_row} = 0;
	$self->{row_offset} = 0;
	$self->redraw;
}

=head2 key_last_row

Move to the last row.

=cut

sub key_last_row {
	my $self = shift;
	$self->{highlight_row} = $self->row_count - 1;
	$self->{row_offset} = $self->row_count > $self->scroll_dimension ? -1 + $self->row_count - $self->scroll_dimension : 0;
	$self->redraw;
}

=head2 key_previous_page

Go up a page.

=cut

sub key_previous_page {
	my $self = shift;
	$self->scroll_highlight(-$self->scroll_dimension);
}

=head2 key_next_page

Go down a page.

=cut

sub key_next_page {
	my $self = shift;
	$self->scroll_highlight($self->scroll_dimension);
}

=head2 key_next_column

Move to the next column.

=cut

sub key_next_column { }

=head2 key_previous_column

Move to the previous column.

=cut

sub key_previous_column { }

=head2 key_first_column

Move to the first column.

=cut

sub key_first_column { }

=head2 key_last_column

Move to the last column.

=cut

sub key_last_column { }

=head2 key_activate

Call the C< on_activate > coderef with either the highlighted item, or the selected
items if we're in multiselect mode.

 $on_activate->([ row indices ], [ items... ])

The items will be as returned by the storage adapter, and will not have any of the
data transformations applied.

=cut

sub key_activate {
	my $self = shift;
	if(my $code = $self->{on_activate}) {
		my @selected = 
			  $self->multi_select
			? (sort { $a <=> $b } grep $self->{selected}{$_}, keys %{$self->{selected}})
			: ($self->highlight_row);
		my $f; $f = $self->adapter->get(
			items => \@selected,
		)->then(sub {
			$code->(\@selected, shift)
		})->on_ready(sub { undef $f });
	}
	$self
}

=head2 key_select_toggle

Toggle selected row.

=cut

sub key_select_toggle {
	my $self = shift;
	return $self unless $self->multi_select;
	$self->{selected}{$self->highlight_row} = $self->{selected}{$self->highlight_row} ? 0 : 1;
	$self
}

=head1 METHODS - Filtering

Very broken. Ignore these for now. Sorry.

=cut

# NYI
sub row_visibility_changed {
	my $self = shift;
}

=head2 row_visibility

Sets the visibility of the given row (by index).

Example:

 # Make row 5 hidden
 $tbl->row_visibility(5, 0)
 # Show row 0
 $tbl->row_visibility(0, 1)

=cut

sub row_visibility {
	my ($self, $idx, $visible) = @_;
	my $row = $self->adapter->get($idx);
	my $prev = ref($row);
	$prev = 'Tickit::Widget::Table::Paged::VisibleRow' if $prev eq 'ARRAY';
	my $next = $visible
	? 'Tickit::Widget::Table::Paged::VisibleRow'
	: 'Tickit::Widget::Table::Paged::HiddenRow';
	bless $row, $next;
	$self->row_visibility_changed($idx) unless $self->{IS_FILTER} || ($prev eq $next);
	$row
}

=head2 filter

This will use the given coderef to set the visibility of each row in the table.
The coderef will be called once for each row, and should return true for rows
which should be visible, false for rows to be hidden.

The coderef currently takes a single parameter: an arrayref representing the
columns of the row to be processed.

 # Hide all rows where the second column contains the text 'OK'
 $tbl->filter(sub { shift->[1] ne 'OK' });

Note that this does not affect row selection: if the multiselect flag is enabled,
it is possible to filter out rows that are selected. This behaviour is by design
(the idea was to allow union select via different filter criteria), call the
L</unselect_hidden_rows> method after filtering if you want to avoid this.

Also note that this is a one-shot operation. If you add or change data, you'll
need to reapply the filter operation manually.

=cut

sub filter {
	my ($self, $filter) = @_;
	# Defer any updates until we've finished making changes
	local $self->{IS_FILTER} = 1;
	for my $idx (0..$self->adapter->count - 1) {
		my $row = $self->adapter->get($idx);
		$self->row_visibility($idx, $filter->($row));
	}
	$self->redraw;
}

sub apply_filters_to_row {
	my ($self, $idx) = @_;
}

=head2 unselect_hidden_rows

Helper method to mark any hidden rows as unselected.
Call this after L</filter> if you want to avoid confusing
users with invisible selected rows.

=cut

sub unselect_hidden_rows {
	my $self = shift;
	delete @{$self->{selected}}{
		grep ref($self->adapter->get($_))->isa('Tickit::Widget::Table::Paged::HiddenRow'), 0..$self->adapter->count-1
	};
	$self
}

1;

__END__

=head1 TODO

Current list of pending features:

=over 4

=item * Column and cell highlighting modes

=item * Proper widget-in-cell support

=item * Better header support (more than one row, embedded widgets)

=back

=head1 SEE ALSO

Other tables and table-like things:

=over 4

=item * L<Tickit::Widget::Table> - older table implementation based on L<Tickit::Widget::HBox> and L<Tickit::Widget::VBox>
widgets. Does not support scrolling and performance isn't as good, so it will eventually be merged with this one.

=item * L<Text::ANSITable> - not part of L<Tickit> but has some impressive styling capabilities.

=back

And these are probably important background reading for formatting and data source support:

=over 4

=item * L<String::Tagged> - supported for applying custom formatting (specifically, pen attributes)

=item * L<Adapter::Async> - API for dealing with abstract data sources

=back

=head1 AUTHOR

Tom Molesworth <cpan@perlsite.co.uk>

=head1 CONTRIBUTORS

With thanks to the following for contribution:

=over 4

=item * Paul "LeoNerd" Evans for testing and suggestions on storage/abstraction handling

=item * buu, for testing and patches

=back

=head1 LICENSE

Copyright Tom Molesworth 2012-2014. Licensed under the same terms as Perl itself.

