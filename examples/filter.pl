#!/usr/bin/env perl 
use strict;
use warnings;
use Tickit::DSL qw(:async);

vbox {
	my $static;
	my $tbl;
	entry {
		my ($entry, $search) = @_;
		warn "search [$search]\n";
		$static->set_text("Search for [$search]");
		my $re = qr/$search/i;
		$tbl->filter(sub { shift->[0] =~ $re });
		$tbl->unselect_hidden_rows;
	};
	$static = static 'hi';
	$tbl = customwidget {
		my $w = Tickit::Widget::Table::Paged->new(
			multi_select => 1,
		);
		$w->add_column(
			label => 'Module',
		);
		$w->add_column(
			label => 'Current',
		);
		$w->add_column(
			label => 'Latest',
		);
		$w->add_row( 'IO::Async', '0.4', '0.52');
		$w->add_row( 'IO::Async::SSL', '0.4', '0.52');
		$w->add_row( 'Future', '0.4', '0.52');
		$w->add_row( 'bigrat', '0.4', '0.52');
		$w
	} expand => 1;
};
tickit->run;
