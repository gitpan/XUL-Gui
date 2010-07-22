use strict;
use warnings;
use XUL::Gui;

display
	STYLE('
		button {
			min-height: 30px;
			max-height: 30px;
			min-width:  40px;
			max-width:  40px;
		}
		button * {
			padding: 0px !important;
			margin:  0px !important;
		}
	'),
	TextBox( id => 'calc' ),
	do {
		our ($x, $y);
		my @stack;
		my $pop;
		my %command = (
			clr  => sub {$_ = ''},
			del  => sub {s/.$//},
			push => sub {push @stack, $_},
			pop  => sub {$_ .= @stack ? $pop = pop @stack : $pop},
			eval => sub {
				my $new = do {
					local $SIG{__WARN__} = sub {die @_};
					eval
				};
				$@ ? alert $@ : ($_ = $new)
			},
		);
		mapn {
			Hbox map {
				my $op = $_;
				Button
					label => $op,
					oncommand => sub {
						($command{$op} or $_ .= $op, next)->()
							for ID(calc)->value
					}
			} @_
		} 5 => qw{
			sin( cos( sqrt( ( )
			$x $y =  ** %
			1  2  3  +  -
			4  5  6  *  /
			7  8  9  0  .
			push pop clr del eval
		}
	};
