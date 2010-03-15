use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name        => 'XUL::Gui',
    license            => 'perl',
    dist_author        => q{Eric Strom <asg@cpan.org>},
    dist_version_from  => 'lib/XUL/Gui.pm',
	dist_abstract	   => 'render cross platform gui applications with firefox from perl',
    build_requires => {
        'Test::More'   => 0,
	},
	requires => {
		'Carp'		   => 0,
		'Storable'     => 0,
		'List::Util'   => 0,
		'MIME::Base64' => 0,
		'Encode'       => 0,
		'File::Find'   => 0,
		'IO::Socket'   => 0,
		'Time::HiRes'  => 0,
    },
    add_to_cleanup     => [ 'XUL-Gui-*' ],
    create_makefile_pl => 'traditional',
	dynamic_config     => 0,
);

$builder->create_build_script();
