use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'XUL::Gui',
    license             => 'perl',
    dist_author         => q{Eric Strom <ejstrom@gmail.com>},
    dist_version_from   => 'lib/XUL/Gui.pm',
	dist_abstract		=> 'display cross platform gui apps with firefox',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'XUL-Gui-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
