#!/usr/bin/perl -w
use strict;
use Test::More tests => 13;

#BEGIN{ push @INC => '../lib' }

use XUL::Gui ':all';

ok  join('' => mapn {$_ % 2 ? "[@_]" : "@_"} 3 => 1 .. 10) eq '[1 2 3]4 5 6[7 8 9]10'
=>  'mapn';

ok  join(' ' => apply {s/a/b/g} 'abcba', 'aok') eq 'bbcbb bok'
=>  'apply';

ok  join(' ' => zip ['a'..'c'], [1 .. 3]) eq "a 1 b 2 c 3"
=>  'zip';

my $obj = object 'myobj', attr=>'rtta', onevt=>sub{'tveno'}, meth=>sub{'htem'};

ok  ref $obj eq 'XUL::Gui::Object'
=>  'object constructor';

ok  $$obj{TAG} eq 'myobj'
&&	$$obj{A}{attr} eq 'rtta'
&&  ref $$obj{A}{onevt} eq 'CODE'
&&  $$obj{A}{onevt}() eq 'tveno'
&&  ref $$obj{M}{meth} eq 'CODE'
&&  $$obj{M}{meth}() eq 'htem'
=>  'object internals';

my $lbl = Label value => 'test';

ok  $$lbl{A}{value} eq 'test'
&&  $$lbl{TAG} eq  'label'
=>  'tags';

ok  eval {XUL::Gui->oo}
=>  'oo constructor';

eval {XUL::Gui->oo('main')};
ok  $@ => 'oo safe';

my $btn = XUL::Gui->oo->button(label=>'btn', oncommand=>sub{});

ok  $$btn{A}{label} eq 'btn'
&&  $$btn{TAG} eq 'button'
&&  ref $$btn{A}{oncommand} eq 'CODE'
=>  'oo detailed';

SKIP: {
	print STDERR '    skipped 4/10 gui tests: define $ENV{XUL_GUI_TEST} to enable'
	and skip 'gui tests: define $ENV{XUL_GUI_TEST} to enable', 4
		unless defined $ENV{XUL_GUI_TEST};

	display silent=>1, Window title=>'XUL::Gui test', minwidth=>640, minheight=>480,
	    Vbox id=>'main', FILL, align=>'center', pack=>'center',
	        Label ( id=>'lbl', value => 'label'),
	        Button( id=>'btn', label => 'button',
	            oncommand=>sub{shift->label = 'ouch'}
	        ),
	        delay sub{
				ok 1, 'launch gui';
	            ok $ID{lbl}->value eq 'label', 'get value';
	            $ID{lbl}->value = 'update';
	            ok $ID{lbl}->value eq 'update', 'set value';
	            $ID{btn}->oncommand;
	            ok $ID{btn}->label eq 'ouch', 'event handler';
	            $ID{main}->removeChildren
	                     ->appendChild(H2 'tests done, please close the window');
	            quit;
	        };
}
