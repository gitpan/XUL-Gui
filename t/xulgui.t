#!/usr/bin/perl -w
use strict;
use Test::Simple tests => 3;

#BEGIN{ push @INC => '../lib' }
use XUL::Gui;

display silent=>1, Window title=>'XUL::Gui test', minwidth=>640, minheight=>480,
    Vbox id=>'main', FILL, align=>'center', pack=>'center',
        Label ( id=>'lbl', value => 'label'),
        Button( id=>'btn', label => 'button',
            oncommand=>sub{shift->label = 'ouch'}
        ),
        delay sub{
            ok $ID{lbl}->value eq 'label', 'get value';
            $ID{lbl}->value = 'update';
            ok $ID{lbl}->value eq 'update', 'set value';
            $ID{btn}->oncommand;
            ok $ID{btn}->label eq 'ouch', 'event handler';
            $ID{main}->removeChildren
                     ->appendChild(H2 'tests done, please close the window');
            quit;
        };
