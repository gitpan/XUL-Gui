use strict;
use warnings;
use XUL::Gui;

display
	STYLE(q{
		button {
			min-height: 30px;
			max-height: 30px;
			min-width:  30px;
			max-width:  30px;
		}
	}),
	TextBox( id => 'calc'),
	mapn {
		Hbox map {
			my $op = $_;
			Button label => $op, oncommand => sub {
				$_ = $op eq '=' ? eval : $_ . $op
					for ID(calc)->value
			}
		} @_
	} 5 => qw{
		1 2 3 + -
	    4 5 6 * /
	    7 8 9 0 =
	};
