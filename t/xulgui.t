#!/usr/bin/perl -w
use strict;
use Test::More tests => 36;

use Data::Dumper;
$Data::Dumper::Useqq = 1;
$Data::Dumper::Terse = 1;

use XUL::Gui ':all';

print "XUL::Gui $XUL::Gui::VERSION\n";

{no warnings 'redefine';
    my $ok = \&ok;
    *ok = sub ($;$) {push @_, shift; goto &$ok}
}

ok  'mapn'
=>  join('' => mapn {$_ % 2 ? "[@_]" : "@_"} 3 => 1 .. 10) eq '[1 2 3]4 5 6[7 8 9]10';

ok  'apply'
=>  join(' ' => apply {s/a/b/g} 'abcba', 'aok') eq 'bbcbb bok';

ok  'zip'
=>  join(' ' => zip ['a'..'c'], [1 .. 3]) eq "a 1 b 2 c 3";

my $obj = object 'myobj', attr=>'rtta', onevt=>sub {'tveno'}, meth=>sub {'htem'};

ok  'object constructor'
=>  ref $obj eq 'XUL::Gui::Object';


ok  'object internals'
=>  $$obj{TAG}          eq 'myobj'
&&  $$obj{A}{attr}      eq 'rtta'
&&  ref $$obj{A}{onevt} eq 'CODE'
&&  $$obj{A}{onevt}()   eq 'tveno'
&&  ref $$obj{M}{meth}  eq 'CODE'
&&  $$obj{M}{meth}()    eq 'htem';

my $lbl = Label value => 'test';

ok  'tags'
=>  $$lbl{A}{value} eq 'test'
&&  $$lbl{TAG}      eq 'label';

ok  'valid id'
=>    eval {Label id => 'abc123_'}
&&  ! eval {Label id => 'asdf 234'};

ok  'style concat'
=>  eval {
		Label(style => 'color: red', style => 'font-weight: bold')
			-> {A}{style} eq 'color: red;font-weight: bold;'
	};

ok  'oo constructor'
=>  eval {XUL::Gui->oo('g')};

eval {XUL::Gui->oo('main')};
ok  'oo safe' => $@;

ok  'oo dyoi'
=>  (eval 'use XUL::Gui "draw->*"; 1')
&&  eval {draw->label};

my $btn = g->button(label=>'btn', oncommand=>sub{});

ok  'oo detailed'
=>  $$btn{A}{label}         eq 'btn'
&&  $$btn{TAG}              eq 'button'
&&  ref $$btn{A}{oncommand} eq 'CODE';

ok  'bitmap(2src)?'
=>   bitmap2src( 2, 2, qw(255 0 0 255 0 0 255 0 0 255 0 0))
eq  "data:image/bitmap;base64,Qk1GAAAAAAAAADYAAAAoAAAAAgAAAAIAAAABABgAAAAAABAAAAATCwAAEwsAAAAAAAAAAAAA/wAA\n/wAAAAD/AAD/AAAAAA==\n";

ok 'server assert pre'
=> ! eval {gui '1'};

SKIP: {
    print STDERR '    skipped gui tests: define $ENV{XUL_GUI_TEST} to enable'
    and skip 'gui tests: define $ENV{XUL_GUI_TEST} to enable', 22
        unless defined $ENV{XUL_GUI_TEST};

    display silent=>1, Window title=>'XUL::Gui test', minwidth=>640, minheight=>480,
        Vbox id=>'main', FILL, align=>'center', pack=>'center',
            Label ( id=>'lbl', value => 'label'),
            Button( id=>'btn', label => 'button',
                oncommand=>sub {$_->label = 'ouch'}
            ),
			Button( id=>'btn_js', label => 'javascript button',
                oncommand=> function q {this.label = 'ouch!'}
            ),
            delay { timeout { delay {
                ok 'server assert running'
                => eval {gui '1'};
                ok 'launch gui', 1;
                ok 'get value' => $ID{lbl}->value eq 'label';

                ok 'return'    => ($ID{lbl}->value = 'update') eq 'update';
                ok 'set value' =>  $ID{lbl}->value eq 'update';

                $ID{btn}->click;
                doevents;
                ok 'perl event handler' => $ID{btn}->label eq 'ouch';

				ID(btn_js)->click;
				ok 'javascript event handler' => $ID{btn_js}->label eq 'ouch!';

                ok 'tr boolean true'  => gui('true')  == 1;
                ok 'tr boolean false' => gui('false') == 0;
                ok 'tr undef from js' => !defined gui('null')
                                      && !defined gui('undefined');

                my $x = gui 'x = {val: 5, val2: 6}';

                ok 'tr undef to js'
                =>  ref $x eq 'XUL::Gui::Object'
                &&  $x->val == 5
                &&  do {
                        $x->val2++;
                        undef $x->val;
                        not defined gui 'x.val' and
                        not defined $x->val     and
                        $x->val2 == 7;
                    };

                ok 'return from perl()'
				=> $$ == gui q {perl( "ok 'call perl from js', 1; $$;" )};

                my $widget = widget {
					unless ($_->has('skip')) {
						ok 'widget local $_{W}'
						=> $_ == $_{W};
						ok 'widget local ID'
						=> \%ID == \%XUL::Gui::ID;
					}
                    Label id=>'lbl', $_->has('label->value!')
                } test => sub {
                    shift->{lbl}->value;
                };

                ok 'escape' => do {
                    g->id('main')->appendChild (
                        g->textbox (g->fill, id=>'txt', multiline=>'true')
                    );
                    g->id('txt')->value .= "$_\n\n"
                        for my @lines = (
                            qw/on'e t''wo th\'ree fo\ur/,
                            "\x{3084}\x{306f}\x{308a}\x{539f}\x{56e0}\x{306f} asdf(jk;l"
                    );
                    g->id('txt')->value eq "on'e\n\nt''wo\n\nth\\'ree\n\nfo\\ur\n\n".
                        "\343\202\204\343\201\257\343\202\212\345\216\237\345\233\240\343\201\257 asdf(jk;l\n\n";
                };

                $ID{main}->appendChild($widget->(id => 'wt1', label => 'widget test 1'));

                ok  'widget test 1'
                =>  $ID{wt1}->test eq 'widget test 1';

                my $widget2 = g->widget(sub {g->hbox(
                    g->label( id=>'lbl', value=>$_{A}{label} ),
					$widget->(-id => 'subwidget', -label => 'subwidget test', -skip => 1)
				)}, test => sub {
                    shift->{lbl}->value
                });

                $ID{main}->appendChild(scalar $widget2->(id => 'wt2', label => 'widget test 2'));

                ok  'widget test 2'
                =>  g->ID('wt2')->test eq 'widget test 2';

				ok  'widget nesting'
				=>  $ID{wt2}{subwidget}{lbl}->value eq 'subwidget test';

				my $inner = widget {Button id => 'btn', label => 'inner'}
					itest => sub {shift->parent->{caption}->label eq 'outer'};
				my $outer = widget {Groupbox Caption(id => 'caption', label=>'outer'), $inner->(id => 'innerw')}
					otest => sub {shift->{innerw}{btn}->label eq 'inner'};

				$ID{main}->replaceChildren($outer->(id => 'outer'));

				ok 'widget outer' => ID(outer)->otest;
				ok 'widget inner' => ID(outer)->{innerw}->itest;

				#    mro ISA

                $ID{main}->removeChildren
                         ->appendChild(H2 'tests done, close window');
                quit
            }} 10 };
    ok 'server assert post'
    => ! eval {gui '1'};
}
