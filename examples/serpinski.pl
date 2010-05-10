use strict;
use warnings;
use XUL::Gui 'g->';

my $width = 800;
my $height = sqrt($width**2 - ($width/2)**2);

my $frame;

g->display(
	g->box(
		g->fill,
		g->middle,
		style => q{
			background-color: black;
			padding: 		  40px;
		},
		g->canvas(
			id     => 'canvas',
			width  => $width,
			height => int $height,
		)
	),
	g->delay(sub {
		my $canvas = g->id('canvas')->getContext('2d');
		$canvas->fillStyle = 'white';

		my @points = (
				   [$width/2, 0],
			[0, $height], [$width, $height],
			#[$width/2, $height/2],
			#[$width/4, $height/2],
			#[$width*(3/4), $height/2],
			#[$width/2, $height],
		);
		my ($x, $y, $p) = @{ $points[0] };
		while (1) {
			$p = $points[ rand 3 ];
			$x = ($x + $$p[0]) / 2;
			$y = ($y + $$p[1]) / 2;

			$canvas->fillRect($x + 1/4, $y + 1/4, 1/2, 1/2);

			unless (++$frame % 1_000) {
				$frame % 100_000
					   ? g->flush
					   : g->doevents # keeps firefox happy
			}
		}
	})
);
