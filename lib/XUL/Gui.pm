package XUL::Gui;
    use warnings;
    use strict;
    use Carp;
    use Storable     'dclone';
    use List::Util   'max';
    use MIME::Base64 'encode_base64';
    use Encode       'encode';
    our $VERSION  =  '0.40';
    our $DEBUG    =   0;

    $Carp::Internal{"XUL::Gui$_"}++
        for '', qw(::Object ::Server);

    sub import {
        require Exporter and
         goto & Exporter::import if @_ == 1
             or 1 < (@_ = grep {
                not /^([\w:]*)->\*?([\w:]*)$/
                && XUL::Gui->oo( $2 or $1 )} @_)
    }


=head1 NAME

XUL::Gui - render cross platform gui applications with firefox from perl

=head1 VERSION

version 0.40

this module is under active development, interfaces may change.

this code is currently in beta, use in production environments at your own risk

this documentation is a work in progress

=head1 SYNOPSIS

    use XUL::Gui;
    display Label 'hello, world!';

    # short enough? s/Label/P/ for bonus points

    use XUL::Gui;
    display Window title => "XUL::Gui's long hello", minwidth=>300,
        GroupBox(
            Caption('XUL'),
            Button( label=>'click me', oncommand=> sub {shift->label = 'ouch'}),
            Button( id=>'btn',
                label=>'automatic id registration',
                oncommand=>sub{
                    $ID{btn}->label = 'means no more variable clutter';
                    $ID{txt}->value = 'and makes cross tag updates easy';
            }),
            Button( type=>'menu', label=>'menu button',
                MenuPopup map {MenuItem label=>$_} qw/first second third/
            ),
            TextBox( id=>'txt', FILL ),
            ProgressMeter(mode=>'undetermined'),
        ),
        GroupBox(
            Caption('HTML too'),
            TABLE( border=>1, TR map {TD $_}
                'one', I('two'), B('three'), U('four'), SUP('five')
            ),
            HR,
            P('all the HTML tags are in CAPS'),
        );

=head1 DESCRIPTION

this module exposes the entire functionality of mozilla firefox's rendering
engine to perl by providing all of the XUL and HTML tags as functions and
allowing you to interact with those objects directly from perl. gui applications
created with this toolkit are cross platform, fully support CSS styling, inherit
firefox's rich assortment of web technologies (browser, canvas and video tags,
flash and other plugins), and are even easier to write than HTML.

this module is written in pure perl, and only depends upon core modules, making
it easy to distribute your application.

all XUL and HTML objects in perl are exact mirrors of their javascript
counterparts and can be acted on as such. for anything not written in this
document or XUL::Gui::Manual, developer.mozilla.com is the official source of
documentation:

=over

=item * L<https://developer.mozilla.org/en/XUL>

=item * L<http://www.hevanet.com/acorbin/xul/top.xul> - XUL periodic table

=item * L<https://developer.mozilla.org/En/Documentation_hot_links>

=back

gui's created with this module are event driven. an arbitrarily complex (and
runtime mutable) object tree is passed to C<display>, which then creates the gui
in firefox and starts the event loop. C<display> will wait for and respond to
events until the C<quit> function is called, or the user closes the window.

all of javascript's event handlers are available, and can be written in perl
(normally) or javascript (for handlers that need to be very fast such as image
rollovers with onmouseover or the like). this is not to say that perl side
handlers are slow, but with rollovers and fast mouse movements, sometimes there
is mild lag due to protocol overhead.

the goal of this module is to make gui development as easy as possible. XUL's
widgets and nested design structure gets us most of the way there, and this
module with its light weight syntax, and "Do What I Mean" nature hopefully
finishes the job. everything has sensible defaults with minimal boilerplate,
and nested design means a logical code flow that isn't littered with variables.
please send feedback if you think anything could be improved.

=head2 tags

all tags (C<XUL>, C<HTML>, user defined widgets, and the C<display> function)
are parsed the same way, and can fit into one of four templates

=over 8

=item * C<< HR() is <hr /> >>

=item * C<< B('some bold text') is <b>some bold text<b/> >>

in the special case of a tag with one argument, which is not another tag, that
argument is added to that tag as a text node. this is mostly useful for HTML
tags, but works with XUL as well

=item * C<< Label( value=>'some text', style=>'color: red' ) >>

    <label value="some text" style="color: red;" />

=item * C<< Hbox( id=>'mybox', Label('hello'), B('world'), pack=>'center' ) >>

    <hbox id="mybox" pack="center">
        <label>hello</label>
        <b>world</b>
    </hbox>

unlike XML based XUL, attribute pairs and children can be mixed in any order,
but attributes should probably be kept at the front for readability

=back

setting the 'id' attribute names the object in the global C<%ID> hash. otherwise
an auto generated name matching C</^xul_\d+$/> is used.

    $object = Button( id=>'btn', label=>'OK' );

    #  $ID{btn} == $object

any tag attribute name that matches C</^on/> is an event handler (onclick,
onfocus...), and expects a C<sub{...}> (perl event handler) or
C<function q{...}> (javascript event handler).

perl event handlers get passed a reference to their object, and an event object

    Button( label=>'click me', oncommand=> sub {
        my ($self, $event) = @_;
        $self->label = $event->type;
    })

in the event handler, C<$_ == $_[0]> so a shorter version would be:

    oncommand => sub {$_->label = pop->type}

javascript event handlers have C<event> and C<this> set for you

    Button( label=>'click me', oncommand=> function q{
        this.label = event.type;
    })

any attribute with a name that doesn't match /^on/ that has a code ref value is
added to the object as a method

Tk's attribute style with a leading dash is supported.
this is useful for readability when collapsing attribute lists with C<qw//>

    TextBox id=>'txt', width=>75, height=>20, type=>'number', decimalplaces=>4;
    TextBox qw/-id txt -width 75 -height 20 -type number -decimalplaces 4/;

multiple 'style' attributes are joined with ';' into a single attribute

=head1 EXPORT

    use XUL::Gui;   # is the same as
    use XUL::Gui qw/:base :util :pragma :xul :html :const :image/;

    the following export tags are available:

    :base       %ID alert display quit widget
    :tools      function gui interval serve timeout toggle XUL
    :pragma     buffered cached delay doevents noevents now
    :const      BLUR FILL FIT FLEX MIDDLE SCROLL
    :widgets    filepicker
    :image      bitmap bitmap2src
    :util       apply mapn trace zip
    :internal   genid object realid tag

    :all     (all exports)
    :default (same as with 'use XUL::Gui;')

    :xul    (also exported as Titlecase)
      Action ArrowScrollBox Assign BBox Binding Bindings Box Broadcaster
      BroadcasterSet Browser Button Caption CheckBox ColorPicker Column Columns
      Command CommandSet Conditions Content DatePicker Deck Description Dialog
      DialogHeader DropMarker Editor Grid Grippy GroupBox HBox IFrame Image Key
      KeySet Label ListBox ListCell ListCol ListCols ListHead ListHeader
      ListItem Member Menu MenuBar MenuItem MenuList MenuPopup MenuSeparator
      Notification NotificationBox Observes Overlay Page Panel Param PopupSet
      PrefPane PrefWindow Preference Preferences ProgressMeter Query QuerySet
      Radio RadioGroup Resizer RichListBox RichListItem Row Rows Rule Scale
      Script ScrollBar ScrollBox ScrollCorner Separator Spacer SpinButtons
      Splitter Stack StatusBar StatusBarPanel StringBundle StringBundleSet Tab
      TabBox TabPanel TabPanels Tabs Template TextBox TextNode TimePicker
      TitleBar ToolBar ToolBarButton ToolBarGrippy ToolBarItem ToolBarPalette
      ToolBarSeparator ToolBarSet ToolBarSpacer ToolBarSpring ToolBox ToolTip
      Tree TreeCell TreeChildren TreeCol TreeCols TreeItem TreeRow TreeSeparator
      Triple VBox Where Window Wizard WizardPage

    :html   (also exported as html_lowercase)
      A ABBR ACRONYM ADDRESS APPLET AREA AUDIO B BASE BASEFONT BDO BGSOUND BIG
      BLINK BLOCKQUOTE BODY BR BUTTON CANVAS CAPTION CENTER CITE CODE COL
      COLGROUP COMMENT DD DEL DFN DIR DIV DL DT EM EMBED FIELDSET FONT FORM
      FRAME FRAMESET H1 H2 H3 H4 H5 H6 HEAD HR HTML I IFRAME ILAYER IMG INPUT
      INS ISINDEX KBD LABEL LAYER LEGEND LI LINK LISTING MAP MARQUEE MENU META
      MULTICOL NOBR NOEMBED NOFRAMES NOLAYER NOSCRIPT OBJECT OL OPTGROUP OPTION
      P PARAM PLAINTEXT PRE Q RB RBC RP RT RTC RUBY S SAMP SCRIPT SELECT SMALL
      SOURCE SPACER SPAN STRIKE STRONG STYLE SUB SUP TABLE TBODY TD TEXTAREA
      TFOOT TH THEAD TITLE TR TT U UL VAR VIDEO WBR XML XMP

constants:

    FLEX    flex => 1
    FILL    flex => 1, align =>'stretch'
    FIT     sizeToContent => 1
    SCROLL  style => 'overflow: auto'
    MIDDLE  align => 'center', pack => 'center'
    BLUR    onfocus => 'this.blur()'

    each is a function that returns its constant, prepended to its arguments,
    thus the following are both valid:

    Box FILL pack=>'end';
    Box FILL, pack=>'end';

=cut

    sub FLEX   {flex  => 1,                   @_}
    sub FILL   {qw/flex 1 align stretch/,     @_}
    sub FIT    {sizeToContent => 1,           @_}
    sub SCROLL {style => 'overflow: auto',    @_}
    sub MIDDLE {qw/align center pack center/, @_}
    sub BLUR   {qw/onfocus this.blur()/,      @_}

    our @Xul = map {$_, (ucfirst lc) x /.[A-Z]/} qw {
        Action ArrowScrollBox Assign BBox Binding Bindings Box Broadcaster
        BroadcasterSet Browser Button Caption CheckBox ColorPicker Column
        Columns Command CommandSet Conditions Content DatePicker Deck
        Description Dialog DialogHeader DropMarker Editor Grid Grippy GroupBox
        HBox IFrame Image Key KeySet Label ListBox ListCell ListCol ListCols
        ListHead ListHeader ListItem Member Menu MenuBar MenuItem MenuList
        MenuPopup MenuSeparator Notification NotificationBox Observes Overlay
        Page Panel Param PopupSet PrefPane PrefWindow Preference Preferences
        ProgressMeter Query QuerySet Radio RadioGroup Resizer RichListBox
        RichListItem Row Rows Rule Scale Script ScrollBar ScrollBox ScrollCorner
        Separator Spacer SpinButtons Splitter Stack StatusBar StatusBarPanel
        StringBundle StringBundleSet Tab TabBox TabPanel TabPanels Tabs Template
        TextBox TextNode TimePicker TitleBar ToolBar ToolBarButton ToolBarGrippy
        ToolBarItem ToolBarPalette ToolBarSeparator ToolBarSet ToolBarSpacer
        ToolBarSpring ToolBox ToolTip Tree TreeCell TreeChildren TreeCol
        TreeCols TreeItem TreeRow TreeSeparator Triple VBox Where Window Wizard
        WizardPage
    };
    our %HTML = map {("html_$_" => "html:$_", uc $_ => "html:$_")} qw {
        a abbr acronym address applet area audio b base basefont bdo bgsound big
        blink blockquote body br button canvas caption center cite code col
        colgroup comment dd del dfn dir div dl dt em embed fieldset font form
        frame frameset h1 h2 h3 h4 h5 h6 head hr html i iframe ilayer img input
        ins isindex kbd label layer legend li link listing map marquee menu meta
        multicol nobr noembed noframes nolayer noscript object ol optgroup
        option p param plaintext pre q rb rbc rp rt rtc ruby s samp script
        select small source spacer span strike strong style sub sup table tbody
        td textarea tfoot th thead title tr tt u ul var video wbr xml xmp
    };
    our %EXPORT_TAGS = (
        util     => [qw/zip mapn apply trace/],
        base     => [qw/%ID display quit alert widget/],
        widgets  => [qw/filepicker/],
        tools    => [qw/gui interval timeout toggle function serve XUL/],
        pragma   => [qw/buffered now cached noevents delay doevents/],
        xul      => [@Xul],
        html     => [keys %HTML],
        const    => [qw/FLEX FIT FILL SCROLL MIDDLE BLUR/],
        image    => [qw/bitmap bitmap2src/],
        internal => [qw/tag object genid realid/],
    );
    our @EXPORT_OK    = map @$_ => values %EXPORT_TAGS;
    our @EXPORT       = map @{ $EXPORT_TAGS{$_} } =>
                         qw/util base tools pragma xul html const image/;
    @EXPORT_TAGS{qw/default all/} = (\@EXPORT, \@EXPORT_OK);

    #for (qw/base tools pragma const widgets image util internal/) {
    #   printf "    :%-10s %s\n", $_, join ' '=> sort {
    #       lc $a cmp lc $b
    #   } @{ $EXPORT_TAGS{$_} }
    #}

    our %defaults = (
        window      => ['xmlns:html' => 'http://www.w3.org/1999/xhtml',
                         xmlns       => 'http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul' ],
        scrollbox   => [ scrollTo    => sub {my ($self, $x, $y) = @_; gui("scrollTo(ID.$$self{ID}, $x, $y);")} ],
        textbox     => [  value      => sub :lvalue {tie my $ret, 'XUL::Gui::Scalar', shift, '_value'; $ret},
                         _value      => sub :lvalue {tie my $ret, 'XUL::Gui::Scalar', shift,  'value'; $ret} ],
    );
    our $server = XUL::Gui::Server->new;
    our (%ID, %dialogs);

    {*ID = my $id = {};
        sub realid :lvalue {
            @_ ? $id->{$_[0]} : (my $id = $id)
        }
    }
    {my $id; sub genid () {'xul_' . ++$id}}

    sub isa_object {UNIVERSAL::isa @_ ? $_[0] : $_, 'XUL::Gui::Object'}

    sub mapn (&$@);

    sub CLONE_SKIP {1}

    sub parse {
        my (@C, %A, %M);
        local $_;
        while ($_ = shift) {
            if (isa_object) {push @C, $_; next}
            grep {not defined and $_ = '???'} $_, $_[0]
                and croak "parse failure: [ $_ => $_[0] ] @_[1..$#_],";
            s/^-//;
            if (/^on/ or ref $_[0] ne 'CODE') {
                /^style$/ and $A{$_} .= (shift).';'
                          or  $A{$_}  =  shift}
            else             {$M{$_}  =  shift}
        }
        C => \@C, A => \%A, M => \%M
    }


=pod

if you prefer an OO interface, there are a few ways to get one:

    use XUL::Gui 'g->*';  # DYOI: draw your own interface

C< g > (which could be any empty package name) now has all of XUL::Gui's
functions as methods.  since DYOI DWYM each of the following graphic styles
are equivalent: C<< g->*, g->, ->g, install_into->g >>.

no functions are imported into your namespace by default, but you can request
any you do want as usual:

    use XUL::Gui qw( g->* :base :pragma );

to use the OO interface:

    g->display( g->Label('hello world') );
    # is the same as
    XUL::Gui::display( XUL::Gui::Label('hello world') );

    use g->id('someid') or g->ID('someid') to access the %ID hash

    the XUL tags are also available in lc and lcfirst:
        g->label       == XUI::Gui::Label
        g->colorpicker == XUL::Gui::ColorPicker
        g->colorPicker == XUL::Gui::ColorPicker

    the HTML tags are also available in lc, unless an XUL tag
    of the same name exists

if you prefer an object (which behaves exactly the same as the package 'g'):

    use XUL::Gui ();        # or anything you do want
    my $g = XUL::Gui->oo;   # $g now has XUL::Gui's functions as methods

=cut
    {my %loaded;
    sub oo {
        no strict 'refs';
        my $pkg = ($_[1] || 'XUL::Gui::OO') . '::';

        if (defined %$pkg)
            {return $loaded{$pkg} || croak "package '$pkg' not empty"}
        mapn {
            my $method = \&{$_[1]};
            *{$pkg.$_} = sub {shift; goto &$method}
        } 2 => %{{
            (map {lc, $_} grep {not /_/} keys %HTML, @{$EXPORT_TAGS{const}}),
            (map {lcfirst, $_} @Xul),
            (map {$_, $_} grep {not /\W|^self$/} @EXPORT_OK)
        }};
        *{$pkg.'ID'} = *{$pkg.'id'} = sub :lvalue {$XUL::Gui::ID{$_[1]}};

        bless $loaded{$pkg} = {} => substr $pkg, 0, -2
    }}


=head1 FUNCTIONS

=head2 gui functions

=over 8

=item C<display LIST>

starts the http server, launches firefox, waits for events

takes a list of gui objects, and several optional parameters:

    debug     (0) .. 3  adjust verbosity to stderr
    silent    (0) 1     disables all stderr status messages
    trusted    0 (1)    starts firefox with '-app' (requires firefox 3+)
    launch     0 (1)    launches firefox, if 0 connect to http://localhost:8888
    skin       0 (1)    use the default 'chrome://global/skin' skin
    chrome     0 (1)    chrome mode disables all normal firefox gui elements,
                        setting this to 0 will turn those elements back on.
    port      (8888)    first port to start the server on, port++ after that
    delay  milliseconds delays each gui update cycle (for debugging)

if the first object is a C<Window>, that window is created, otherwise a default
one is added. the remaining objects are then added to the window.

C<display> will not return until the the gui quits

see SYNOPSYS and XUL::Gui::Manual for more details

=cut
    sub display {$server->start( {&parse} )}


=item C<quit>

shuts down the server (causes a call to C<display> to return at the end of the
current event cycle)

quit will shut down the server, but it can only shut down the client in trusted
mode.

=cut
    sub quit {
        gui('setTimeout("quit()", 5); 0');
        $$server{run} = 0
    }


=item C<serve PATH MIMETYPE DATA>

add a virtual file to the server

    serve '/myfile.jpg', 'text/jpeg', $jpegdata;

the paths C<qw( / /client.js /event /ping /exit /perl )> are reserved

=cut
    sub serve {$server->serve(@_)}


=item C<object TAGNAME LIST>

creates a gui proxy object, allows run time addition of custom tags

    object('Label', value=>'hello') is the same as Label( value=>'hello' )

=cut
    sub object {
        my $tag = lc (shift or '');
        unshift @_, @{ $defaults{$tag} } if $defaults{$tag};

        bless my $self = {
            TAG   => $tag,
            DIRTY => $tag,
            &parse
        } => 'XUL::Gui::Object';

        $$self{ID} = $$self{A}{id} ||= genid;

        $tag ? $ID{ $$self{ID} } = $self
             : $self
    }


=item C<tag NAME>

returns a code ref that generates proxy objects, allows for user defined tag
functions

    *mylabel = tag 'label';

    \&mylabel == \&Label

=cut
    sub tag {
        my @args = @_;
        sub {
            object @args,
                (@_ == 1 and not isa_object $_[0])
                    ? 'TEXT' : (),
                @_
        }
    }
    {no strict 'refs';
        *$_ = tag $_        for @Xul;
        *$_ = tag $HTML{$_} for keys %HTML;
    }


=item C<widget {CODE} HASH>

group tags together into common patterns, with methods and inheritance

    *MyWidget = widget {
        Hbox(
            Label( $_->has('label->value') ),
            Button( label => 'OK', $_->has('oncommand') ),
            $_->children
        )
    }   method  => sub{ ... },
        method2 => sub{ ... },
        some_data => [ ... ];   # unless the value is a CODE ref, each widget
                                # instance gets a new deep copy of the data

    $ID{someobject}->appendChild(
        MyWidget( label=>'widget', oncommand=>\&event_handler )
    );

    inside the widget's code block, several variables are defined:
    variable    contains the passed in
       $_{A} = { attributes }
       $_{C} = [ children   ]
       $_{M} = { methods    }
       $_    = a reference to the current widget (also as $_{W})
       @_    = unchanged runtime argument list

    widgets have the following predefined (and overridable) methods that are
    synonyms / syntactic sugar for the widget variables:

       $_->has('label')        ~~ exists $_{A}{label} ? (label=>$_{A}{label}):()
       $_->has('label->value') ~~ exists $_{A}{label} ? (value=>$_{A}{label}):()

       $_->has('!label !command->oncommand style')

       ->has(...) splits is arguments on whitespace and will search $_{A}, then
       $_{M} for the attribute. if an ! is attached (anywhere) to an attribute,
       it is required, and the widget will croak without it

       $_->attr( STRING )     $_{A}{STRING} # lvalue
       $_->attributes         %{ $_{A} }
       $_->child( NUMBER )    $_{C}[NUMBER] # lvalue
       $_->children           @{ $_{C} }
       $_->can( STRING )      $_{M}{STRING} # lvalue
       $_->methods            %{ $_{M} }

    most everything that you would want to access is available as a method of
    the widget (attributes, children, instance data, methods).  since there may
    be namespace collisions, here is the namespace construction order:

    %widget_methods = (
        passed in attributes,
        predefined widget methods,
        widget methods and instance data,
        passed in methods
    );

    widgets can inherit from other widgets using the ->extends() method:

        *MySubWidget = widget {$_->extends( &MyWidget )}
            submethod => sub{...};

    much more detail in XUL::Gui::Manual

=cut

    sub widget (&%) {
        my ($code, %methods, $sub) = @_;
        no strict 'refs';
        $sub = sub {
            my %data;
            my $id    = realid;
            my $inner = \%ID != $id;
            my $self  = {parse @_};
            my $wid   = $inner ? genid : $$self{A}{id} || genid;
            @$self{qw/T ID M/} = ($sub, $wid, {
                (map {
                    my $k = $_;
                    $_ => sub :lvalue {$$self{A}{$k}}
                } keys %{$$self{A}}),
                has => sub {
                    shift;
                    map {
                        my $required   = s/!//g;
                        my ($key, $as) = (/(.+)->(.+)/, $_, $_);
                        exists $$self{A}{ $key }
                             ? ($as => $$self{A}{ $key }) :
                        exists $$self{M}{ $key }
                             ? ($as => $$self{M}{ $key }) :
                        $required ? do {
                            local $Carp::CarpLevel = 1;
                            croak "widget requires attribute/method '$key'";
                        } : ()
                    } map {split /\s+/} @_
                },
                attr       => sub :lvalue {$$self{A}{ $_[1] }},
                child      => sub :lvalue {$$self{C}[ $_[1] ]},
                can        => sub :lvalue {$$self{M}{ $_[1] }},
                attributes => sub {%{ $$self{A} }},
                children   => sub {@{ $$self{C} }},
                methods    => sub {%{ $$self{M} }},
                parent     => sub {   $$self{W}  },
                id         => sub {   $$self{ID} },
                extends    => sub {
                    shift;
                    my $target = (\%ID == realid) ? $self : \%ID;
                    my $base = $_[0]{W} or croak 'extends takes a widget';
                    $$target{$_} = $$base{$_} for grep {/[a-z]/} keys %$base;
                    push @{$$self{ISA}}, $base;
                    @_
                },
                (map {
                    my ($k, $v) = ($_, $methods{$_});
                    ref $v eq 'CODE' ? ($k, $v)
                    : do {
                        $data{$k} = dclone $v;
                        $k => sub :lvalue {$data{$k}};
                    }
                } keys %methods),
                %{$$self{M}}
            });

            $$self{$_} = $data{$_} for keys %data;

            $$id{$wid} = bless $self => 'XUL::Gui::Object';

            $ID{$$self{A}{id} or genid} = $self if $inner;

            my $callid = (caller).'::ID';
            local %ID;
            local *$callid  = \%ID if defined %$callid;
            local ($_, *_)  = ($self) x 2;

            $_{W} = $self;

            $$self{NOPROXY} = 1;
            $$self{ISA}     = [];
            $$self{CONTENT} = [ &$code ];

            delete $_{W};

            for my $i (keys %ID) { # refactor to reduce copying
                $$self{$i} = $$id{ my $gid = genid } = $ID{$i};
                next unless isa_object $ID{$i};
                $ID{$i}{N}    = $ID{$i}{A}{id};
                $ID{$i}{ID}   = $ID{$i}{A}{id} = $gid;
                $ID{$i}{W}    = $self;
                $ID{$i}{$_} ||= $ID{$_}       for keys %ID;
                $ID{$i}{$_} ||= $$self{A}{$_} for keys %{$$self{A}};
            }
            local *_ = $$self{CONTENT};
            @_[0 .. $#_]
        }
    }


=item C<alert STRING>

open an alert message box

=cut
    sub alert {
        my $msg = &escape;
        gui( "alert('$msg')" );
        wantarray ? @_ : pop
    }


=item C<filepicker MODE FILTER_PAIRS>

opens a filepicker dialog. modes are 'open', 'dir', 'save'. returns the path or
undef on failure. if mode is 'open' and C<filepicker> is called in list context,
the picker can select multiple files.

    my @files = filepicker open =>
                    Text   => '*.txt;*.rtf',
                    Images => '*.jpg;*.gif;*.png';

=cut
    sub filepicker {
        my $want = wantarray;
        my $type = shift || 'open';
        my $mode = {
            open => $want
                    ? [modeOpenMultiple => 'Select Files'   ]
                    : [modeOpen         => 'Select a File'  ],
            save =>   [modeSave         => 'Save as'        ],
            dir  =>   [modeGetFolder    => 'Select a Folder'],
        }->{$type};

        my $res = gui(qq ~
            (function () {
                var nsIFilePicker = Components.interfaces.nsIFilePicker;
                var fp = Components.classes["\@mozilla.org/filepicker;1"]
                                   .createInstance(nsIFilePicker);
                fp.init(window, "$$mode[1]", nsIFilePicker.$$mode[0]);
             @{[mapn {qq{
                fp.appendFilter("$_[0]","$_[1]");
             }} 2 => @_ ]}
                var res =  fp.show();
                if (res == nsIFilePicker.returnCancel) return;~ .
        ($type eq 'open' && $want ? q {
                var files = fp.files;
                var paths = [];
                while (files.hasMoreElements()) {
                    var arg = files.getNext().QueryInterface(
                                Components.interfaces.nsILocalFile ).path;
                    paths.push(arg);
                }
                return paths.join("\n")
        } : q { return fp.file.path;}
        ) . '})()');
        defined $res
            ? $want
                ? split /\n/ => $res
                : $res
            : ()
    }


=item C<trace LIST>

carps LIST with object details, and then returns LIST unchanged

=cut
    sub trace {
        my $caller = caller;
        carp 'trace: ', join ', ' => map {
            (isa_object) ? lookup($_, $caller) : $_
        } @_;
        wantarray ? @_ : pop
    }

    {my %cache;
    sub lookup {
        no strict 'refs';
        my $self = shift;
        return $cache{$self} if $cache{$self};
        return $$self{ID} || $self unless $$self{W} || $$self{T};
        no warnings;
        our %space;
        local *space = \%{"$_[0]\::"};
        for (keys %space) {
            eval {*{$space{$_}}{CODE}} == ($$self{T} || $$self{W}{T})
                and return $cache{$self} = $_ .
                    ($$self{T}
                        ? '{'
                        : '{'.($$self{W}{A}{id} || $$self{W}{ID}).'}->{'
                    ).($$self{N} || $$self{ID}).'}'
        }
        $$self{ID} || $self
    }}


=item C<function JAVASCRIPT>

create a javascript event handler, useful for mouse events that need to be very
fast, such as onmousemove or onmouseover

    Button( label=>'click me', oncommand=> function q{
        this.label = 'ouch';
        alert('hello from javascript');
        if (some_condition) {
            perl("print 'hello from perl'");
        }
    })

    to access widget siblings by id, wrap the id with C< W{...} >

=cut
    sub function ($) {
        my $js = shift;
        bless [ sub {
            my $self = shift;
            my $func = 'ID.' . genid;
            delay( sub {
                $js =~ s[\$?W{\s*(\w+)\s*}] [ID.$$self{W}{$1}{ID}]g;
                gui(
                    qq{$func = function (event) {
                        try {return (function(){ $js }).call( ID.$$self{ID} )}
                        catch (e) {alert( e.name + "\\n" + e.message )}
                }})
            });
            "$func(event)"
        } ] => 'XUL::Gui::Function'
    }


=item C<interval {CODE} TIME>

perl interface to javascript's C<setInterval()>. interval returns a code ref
which when called will cancel the interval. TIME is in milliseconds.

=cut
    sub interval (&$) {
        my ($code, $time) = @_;
        my $id = genid;
        realid($id) = $code;
        gui( qq{ID.$id = setInterval( "perl('XUL::Gui::realid(q|$id|)->()')", $time)} );
        sub {gui(qq{clearInterval(ID.$id)})}
    }


=item C<timeout {CODE} TIME>

perl interface to javascript's C<setTimeout()>. timeout returns a code ref which
when called will cancel the timeout. TIME is in milliseconds.

=cut
    sub timeout (&$) {
        my ($code, $time) = @_;
        my $id = genid;
        realid($id) = $code;
        gui( qq{ID.$id = setTimeout( "perl('XUL::Gui::realid(q|$id|)->()')", $time)} );
        sub {gui(qq{cancelTimeout(ID.$id)})}
    }

    sub escape {
        for ("@_") {
            s/\\/\\\\/g;
            s/\n/\\n/g;
            s/\r/\\r/g;
            s/'/\\'/g;
            return encode ascii => $_, sub {sprintf '\u%04X', $_[0]}
        }
    }


=item C<XUL STRING>

converts an XML XUL string to XUL::Gui objects.  experimental.

this function is provided to facilitate drag and drop of XML based XUL from
tutorials for testing. the perl functional syntax for tags should be used in all
other cases

=cut
    {my %xul; @xul{map lc, @Xul} = @Xul;
    sub XUL {
        for ("@_") {
            s {<(\w+)(.+?)}       "$xul{lc $1}($2"g;
            s {/>}                '),'g;
            s {</\w+>}            '),'g;
            s {>}                 ''g;
            s {(\w+)\s*=\s*(\S+)} "'$1'=>$2"g;
            s <([^\\](}|"|'))\s+> "$1,"g;
            return eval 'package '.caller().";$_"
                or carp "content skipped due to parse failure: $@\n\n$_"
        }
    }}


=item C<gui JAVASCRIPT>

executes JAVASCRIPT in the gui, returns the result

=back

=cut
    {my ($buffered, @buffer, $cached, %cache, $now);
        sub gui :lvalue {
            my $msg = "@_\n";
            my $get = $msg =~ /^GET/;
               $msg =~ s/^[GS]ET;//;

            unless ($now) {
                push @buffer, $msg and return if $buffered;
                return $cache{$msg} if exists $cache{$msg};
            }
            $server->write('text/plain', $msg);
            my $res = $server->safe_read->{CONTENT};
            croak "invalid response: $res" unless $res =~ /^(...) (.*)/s;

            $res = $1 eq 'OBJ'
                       ? ($ID{$2} || object undef, id=>$2)
                       : $1 eq 'UND'
                             ? undef
                             : $2;
            $cache{$msg} = $res if $cached and $get;
            $res
        }


=head2 pragmatic blocks

the following functions all apply pragmas to their CODE blocks. in some cases,
they also take a list. this list will be C<@_> when the CODE block executes.
this is useful for sending in values from the gui, if you don't want to use a
C<now {block}>

=over 8

=item C<buffered {CODE} LIST>

delays sending gui updates

    buffered {
        $ID{$_}->value = '' for qw/a bunch of labels/
    }; # all labels are cleared at once

=cut
        sub buffered (&@) {
            $buffered++;
            &{+shift};
            gui splice @buffer unless --$buffered;
            return
        }


=item C<cached {CODE}>

turns on caching of gets from the gui

=cut
        sub cashed (&) {
            $cached++;
            my $ret = shift->();
            %cache = () unless --$cached;
            $ret
        }


=item C<now {CODE}>

execute immediately, from inside a buffered or cached block

=cut
        sub now (&) {
            $now++;
            my @ret = shift->();
            $now--;
            wantarray ? @ret : $ret[0]
        }
    }


=item C<delay {CODE} LIST>

delays executing its CODE until the next gui refresh

useful for triggering widget initialization code that needs to run after the gui
objects are rendered

=cut
    sub delay (&@) {
        my $code = shift;
        my @args = @_;
        push @{$$server{queue}}, sub {$code->(@args)};
        return
    }


=item C<noevents {CODE} LIST>

disable event handling

=cut
    sub noevents (&@) {
        gui('cacheEvents = false;');
        my @ret = &{+shift};
        gui('cacheEvents = true;');
        @ret
    }


=item C<doevents>

force a gui update before an event handler finishes (experimental)

=cut
    sub doevents () {
        $server->write('text/plain', 'NOOP');
        $server->safe_read('/ping');
        return
    }


=back

=head2 utility functions

=over 8

=item C<mapn {CODE} NUMBER LIST>

map over n elements at a time in C<@_> with C<$_ == $_[0]>

    print mapn {$_ % 2 ? "@_" : " [@_] "} 3 => 1..20;
    > 1 2 3 [4 5 6] 7 8 9 [10 11 12] 13 14 15 [16 17 18] 19 20

=cut
    sub mapn (&$@) {
        my ($sub, $n, @ret) = splice @_, 0, 2;
        croak '$_[1] must be >= 1' unless $n >= 1;

        return map $sub->($_) => @_ if $n == 1;

        my $want = defined wantarray;
        while (@_) {
            local *_ = \$_[0];
            if ($want) {push @ret =>
                  $sub->(splice @_, 0, $n)}
            else {$sub->(splice @_, 0, $n)}
        }
        @ret
    }


=item C<zip LIST of ARRAYREF>

    %hash = zip [qw/a b c/], [1..3];

=cut
    sub zip {
        map {my $i = $_;
            map $$_[$i] => @_
        } 0 .. max map $#$_ => @_
    }


=item C<apply {CODE} LIST>

apply a function to a copy of LIST and return the copy

    print join ", " => apply {s/$/ one/} "this", "and that";
    > this one, and that one

=cut
    sub apply (&@) {
        my ($sub, @ret) = @_;
        $sub->() for @ret;
        wantarray ? @ret : pop @ret
    }


=item C<toggle TARGET OPT1 OPT2>

alternate a variable between two states

    toggle $state;          # opts default to 0, 1
    toggle $state => 'red', 'blue';

=cut
    sub toggle {
        no warnings;
        my @opt = (splice(@_, 1), 0, 1);
        $_[0] = $opt[ $_[0] eq $opt[0] or $_[0] ne $opt[1] ]
    }


=item C<bitmap WIDTH HEIGHT OCTETS>

returns a binary .bmp bitmap image. OCTETS is a list of BGR values

    bitmap 2, 2, qw(255 0 0 255 0 0 255 0 0 255 0 0); # 2px blue square

for efficiency, rather than a list of OCTETS, you can send in a single array
reference. each element of the array reference can either be an array reference
of octets, or a packed string C< pack "C*" => OCTETS >

=cut
    sub bitmap {
        my ($width, $height) = splice @_, 0, 2;

        my @pad = map {(0) x ($_ and 4 - $_)} ($width*3) % 4;

        my $size = $height * ($width * 3 + @pad);

        pack 'n V n n (N)2 (V)2 n n N V (N)4 (a*)*' =>
            0x42_4D,         # "BM"
            (54 + $size),    # file size
            0x00_00,         # not used
            0x00_00,         # not used
            0x36_00_00_00,   # offset of bitmap data
            0x28_00_00_00,   # remaining bytes in header
            $width,
            $height,
            0x01_00,         # color planes
            0x18_00,         # bits/pixel (24)
            0x00_00_00_00,   # no compression
            $size,           # size of raw BMP data (after header)
            0x13_0B_00_00,   # horizontal res
            0x13_0B_00_00,   # vertical res
            0x00_00_00_00,   # not used
            0x00_00_00_00,   # not used
            reverse
              @_ > 1
                ? map {pack 'C*' => splice(@_, 0, $width*3), @pad} 1 .. $height
                : map {ref $_
                     ? pack 'C*'    => @$_, @pad
                     : pack 'a* C*' =>  $_, @pad
                } @{$_[0]}
    }


=item C<bitmap2src WIDTH HEIGHT OCTETS>

returns a packaged bitmap image that can be directly assigned to an image tag's
src attribute. arguments are the same as C<bitmap()>

    $ID{myimage}->src = bitmap2src 320, 180, @image_data;

=back

=cut
    sub bitmap2src {
        'data:image/bitmap;base64,' . encode_base64 &bitmap
    }


=head1 METHODS

    # access attributes and properties

        $object->value = 5;                   # sets the value in the gui
        print $object->value;                 # gets the value from the gui

    # the attribute is set if it exists, otherwise the property is set

        $object->_value = 7;                  # sets the property directly

    # method calls

        $object->focus;                       # void context or
        $object->appendChild( H2('title') );  # any arguments are always methods
        print $object->someAccessorMethod_;   # append _ to force interpretation
                                              # as a JS method call

in addition to mirroring all of an object's existing javascript methods /
attributes / and properties to perl (with identical spelling / capitalization),
several default methods have been added to all objects

=over 8

=cut

package
    XUL::Gui::Object;
    my $search; $search = sub {
        my ($self, $method) = @_;
        $self->{M}{$method} or do {
            for (@{$$self{ISA}}, @{$$self{C}})
                {defined and return $_ for $search->($_, $method)}
        }
    };

    sub AUTOLOAD :lvalue {
        my $self = $_[0];
        (my $AL) = our $AUTOLOAD =~ /([^:]+)$/
            or Carp::croak 'invalid autoload';

        if (my $method = $search->($self, $AL)) { # perl method call
            if (ref $method eq 'ARRAY') {
                splice @_, 0, 1, $$method[0];
                $method = $$method[1];
            }
            if ($XUL::Gui::DEBUG) {
                my $caller = caller;
                print XUL::Gui::lookup($self, $caller) . "->$1(" .
                    (join ',' => map {(XUL::Gui::isa_object)
                        ? XUL::Gui::lookup($_, $caller) : $_} @_[1..$#_]). ")\n"
            }
            goto &$method
        }
        Carp::croak "no method '$AL' on " . XUL::Gui::lookup($self, scalar caller)
            if $$self{NOPROXY} or not shift->{ID};

        if ($AL =~ s/_$// or @_ or not defined wantarray) { # js method call
            my ($js, $arg) = ('') x 2;

            {($$self{uc $AL} or next)->(local $_ = $self); return}

            $arg = join ',', map {not defined and 'null' or
                XUL::Gui::isa_object and do {
                    if ($_->{DIRTY}) {
                        ($$_{W} ? $$_{W}{W} : $$_{W}) ||= $$self{W};
                        $$_{P} ||= $self;
                        $js .= $_->toJS;
                    } "ID.$_->{ID}"
                } or "'" . XUL::Gui::escape($_) . "'"
            } @_;
            return XUL::Gui::gui "$js; ID.$self->{ID}.$AL($arg);"
        }
        tie my $ret, 'XUL::Gui::Scalar', $self, $AL; # proxy
        $ret
    }

    {my @queue;
    sub DESTROY {
        eval {$_[0]{ID}} or return;
        push @queue, "ID.$_[0]{ID} = null;";
        if (@queue == 1) {
            XUL::Gui::delay {
                XUL::Gui::gui "@queue", @queue = ();
            }
        }
    }}
    sub CLONE_SKIP {1}

    sub registerEvents {
        my $self = shift;
        for (keys %{$$self{A}}) {
            next unless /^on/ and ref $$self{A}{$_} eq 'CODE';
            $$self{uc $_} = $$self{A}{$_};
            $$self{A}{$_} = 'EVT(event);';
        }
        $self
    }

    sub toXUL {
        my $self = shift;
        my $tab  = shift || 0;
        my @xul  = ();
        my $text = '';

        $self->{DIRTY} = 0;
        $self->registerEvents;
        defined and return $_ for $$self{CODE};

        push @xul, "<$self->{TAG} ";
        for (keys %{$self->{A}}) {
            my $val = XUL::Gui::escape
                ref $$self{A}{$_} eq 'XUL::Gui::Function'
                    ? $$self{A}{$_}[0]( $self )
                    : $$self{A}{$_};
            if (/^TEXT$/) {
                $val =~ s/\\n/\n/g;
                $text = $val;
                next
            }
            push @xul, qq{$_="$val" }
        }
        if (@{$$self{C}} or $text) {
            push @xul, ">$text\n";
            push @xul, "\t" x ($tab+1), $_->toXUL($tab+1) for @{$$self{C}};
            push @xul, "\t" x $tab, "</$self->{TAG}>\n";
        } else {
            push @xul, "/>\n"
        }
        join '' => @xul
    }

    sub toJS {
        my ($self, $final) = @_;
        my @js;
        my $id = "ID.$$self{ID}";

        $$self{DIRTY} = 0;
        $self->registerEvents;
        defined and return $_ for $$self{CODE};

        push @js, $_->toJS for @{$$self{C}};
        push @js, qq{$id = document.createElement} .
            ($$self{TAG} !~ /:/
                ? qq{('$$self{TAG}');}
                : qq{NS('http://www.w3.org/1999/xhtml', '$$self{TAG}');});
        for (keys %{$$self{A}}) {
            my $val = XUL::Gui::escape
                ref $$self{A}{$_} eq 'XUL::Gui::Function'
                    ? $$self{A}{$_}[0]( $self )
                    : $$self{A}{$_};
            if (/^TEXT$/) {
                push @js, qq{$id.appendChild( document.createTextNode('$val') );};
                next
            }
            push @js, /^_(.+)/
                        ? qq{$id\['$1'] = '$val';}
                        : qq{$id.setAttribute('\L$_\E','$val');};
        }
        for (@{$$self{C}}) {
            $$_{P} = $self;
            push @js, qq{$id.appendChild(ID.$$_{ID});} if $$_{TAG};
        }
        join "\n" => @js, $final ? "$final.appendChild($id);" : ''
    }


=item C<< ->removeChildren( LIST ) >>

removes the children in LIST, or all children if none are given

=cut
    sub removeChildren {
        my $self = shift;
        @_  ? XUL::Gui::buffered {$self->removeChild($_) for @_} @_
            : XUL::Gui::gui "removeChildren(ID.$self->{ID});";
        $self
    }


=item C<< ->removeItems( LIST ) >>

removes the items in LIST, or all items if none are given

=cut
    sub removeItems {
        my $self = shift;
        @_  ? XUL::Gui::buffered {$self->removeItem($_) for @_} @_
            : XUL::Gui::gui "removeItems(ID.$self->{ID});";
        $self
    }


=item C<< ->appendChildren( LIST ) >>

appends the children in LIST

=cut
    sub appendChildren {
        my $self = shift;
        XUL::Gui::buffered {$self->appendChild($_) for @_} @_;
        $self
    }


=item C<< ->prependChild( CHILD, [INDEX] ) >>

inserts CHILD at INDEX (defaults to 0) in the parent's child list

=cut
    sub prependChild {
        my ($self, $child, $count, $first) = @_;
        if ($$self{TAG} eq 'tabs') {
            $first = $self->getItemAtIndex( $count || 0 )
        } else {
            $first = $self->firstChild;
            while ($count-- > 0) {
                last unless $first;
                $first = $first->nextSibling;
            }
        }
        $first ? $self->insertBefore( $child, $first )
               : $self->appendChild ( $child );
        $self
    }


=item C<< ->appendItems( LIST ) >>

append a list of items

=cut
    sub appendItems {
        my ($self, @items) = @_;
        XUL::Gui::buffered {
            (XUL::Gui::isa_object)
                ? $self->appendChild($_)
                : $self->appendItem( ref eq 'ARRAY' ? @$_ : $_ )
            for @items
        };
        $self
    }


=item C<< ->replaceItems( LIST ) >>

removes all items, then appends LIST

=back

=cut
    sub replaceItems {
        my ($self, @items) = @_;
        XUL::Gui::buffered {
        XUL::Gui::noevents {
            $self->removeItems
                 ->appendItems( @items )
        }};
        $self
    }


package
    XUL::Gui::Scalar;
    our @ISA = 'XUL::Gui::Object';
    use Carp;

    sub TIESCALAR {bless [ @_[1..$#_] ] => $_[0]}
    sub DESTROY {}
    sub CLONE_SKIP {1}

    sub FETCH {
        my ($self, $AL) = @{+shift};
        return $self->{uc $AL} if $AL =~ /^on/;
        XUL::Gui::gui $AL =~ /^_(.+)/
            ? "GET;ID.$self->{ID}\['$1'];"
            : "GET(ID.$self->{ID}, '$AL');"
    }

    sub STORE {
        my ($self, $AL, $new) = (@{+shift}, @_);
        if ($AL =~ /^on/) {
            if (ref $new eq 'XUL::Gui::Function') {
                $new = $$new[0]($self);
            } else {
                not defined $new or ref $new eq 'CODE'
                    or croak "assignment to event handler must be CODE ref or undef";
                $new = $new ? do {$$self{uc $AL} = $new; 'EVT(event)'} : '';
            }
        }
        $new = defined $new ? "'" . XUL::Gui::escape($new) . "'" : 'null';
        XUL::Gui::gui $AL =~ /^_(.+)/
            ? "SET;ID.$self->{ID}\['$1'] = $new;"
            : "SET(ID.$self->{ID}, '$AL', $new);"
    }


package
    XUL::Gui::Server;
    use Carp;
    use File::Find;
    use IO::Socket;
    use Time::HiRes qw/usleep/;

    my $port = 8888;
    our ($req, $client_js, $silent, $active);

    sub new {bless {}}

    sub message {print STDERR "XUL::Gui> @_\n" unless $silent; 1}

    sub start {
        my $self        = shift;
        $$self{args}    = shift;
        $$self{content} = $$self{args}{C};

        $$self{$_} = $$self{args}{A}{$_}
            for qw(debug silent trusted launch skin chrome port delay);

        local $silent = $$self{silent};

        defined $$self{$_} or $$self{$_} = 1
            for qw(launch chrome skin);

        $$self{caller} = caller 1;

        message "version $VERSION" if
            local $DEBUG = $$self{debug} || $DEBUG;

        $active = $self;

        $$self{port} ||= $port++;
        $$self{port}++ until $$self{server} = IO::Socket::INET->new(
            Proto     => 'tcp',
            PeerAddr  => 'localhost',
            LocalAddr => "localhost:$$self{port}",
            Listen    => 1,
        );

        local $| = 1;
        message "server started: http://localhost:$$self{port}";

        my $root;
        $$self{dispatch} = {
            exists $$self{dispatch} ? %{$$self{dispatch}} : (),
            '/' => sub {
                my $res = qq{<?xml version="1.0" encoding="UTF-8"?>\n};
                   $res.= qq{<?xml-stylesheet href="chrome://global/skin" type="text/css"?>\n} if $$self{skin};

                $root = $$self{content}[0]->{TAG} eq 'window'
                            ? shift @{$$self{content}}
                            : XUL::Gui::Window();

                $$self{onunload} ||= $$root{A}{onunload};
                $$self{onclose}  ||= $$root{A}{onclose};
                $$root{A}{onclose}  = q{ return shutdown() };
                $$root{A}{onunload} = q{ return shutdown() };
                unshift @{$$self{content}}, @{ $root->{C} };
                $root->{C} = [ XUL::Gui::Script(src=>"http://localhost:$$self{port}/client.js") ];
                $self->write('application/vnd.mozilla.xul+xml', $res . $root->toXUL);
            },
            '/client.js' => sub {
                $self->write( 'text/javascript', qq~
                    var port = $$self{port};
                    $client_js;
                    var root = ID.$root->{ID} = document.getElementById('$root->{ID}');~ .
                    join ";" => map {$_->toJS("ID.$root->{ID}")} @{$$self{content}}
                )
            },
            '/event' => sub {
                message "event $req->{CONTENT}" if $XUL::Gui::DEBUG > 1;
                my ($code, $id, $evt, $obj) = split ' ', $req->{CONTENT};
                $evt = "ON\U$evt";
                $id  = $root->{ID} if $id eq 'undefined'; # why doesn't onunload window send id?
                $id  = $ID{$id}{P}{ID} while $ID{$id}{P} and not $ID{$id}{$evt}; # more xul bugs
                if (ref $ID{$id}{$evt} eq 'CODE') {
                    $ID{$id}{$evt}->( local $_ = $ID{$id},
                                      XUL::Gui::object(undef, id=>$obj) )
                } else {message "no event handler found: $req->{CONTENT}"}
                $self->write('text/plain', 'NOOP');
            },
            '/ping' => sub {
                &$_ for @{$$self{queue}};
                @{$$self{queue}} = ();
                local $DEBUG = 1;
                $self->write('text/plain', 'NOOP');
            },
            '/favicon.ico' => sub {
                $self->write('text/plain', '');
            },
            '/close' => sub {
                my $shutdown = 1;
                for (grep defined, @$self{qw/onclose onunload/}) {
                    $shutdown = ref eq 'CODE' ? $_->() : XUL::Gui::gui $_;
                }
                $self->write('text/plain', 'RETURN ' . ($shutdown ? 'true' : 'false'));
                $$self{run} = ! $shutdown;
            }
        };
        if ($$self{launch}) {
            my @firefox;
            find sub {push @firefox, [length, $File::Find::name]
                        if /^firefox(?:-bin|\.exe)?$/ and -f} => $_
                for grep {/mozilla|firefox/i and -d}
                    map {
                        opendir my $handle => my $dir = $_;
                        map "$dir/$_" => readdir $handle
                    } grep -d,
                        $^O =~ /MSWin/  ? (map {chomp; "$_\\"}
                                           split /,/ => `echo \%ProgramFiles\%,\%ProgramFiles(x86)\%`) :
                        $^O =~ /darwin/ ? qw(. /Applications) :
                        split  /[:;]/  => $ENV{PATH};

            if (@firefox = sort {$$a[0] < $$b[0]} @firefox) {
                message 'launching firefox';
                my $app;
                for ($$self{trusted}) {defined and !$_  or
                                        $_ = `"$firefox[0][1]" -v` =~ /firefox 3/i}
                if  ($$self{trusted}) {
                    require "File/$_.pm" for qw/Temp Spec/;

                    $$self{dir} = File::Temp->newdir;
                    $$self{dir}->unlink_on_destroy(0); # for threads

                    my $name = $$self{dir}->dirname;
                    my $base = 'xulgui' .  time;

                    my ($file, $dir) = map {my $method = $_;
                        sub {File::Spec->$method( $name, $base, split /\s+/ => "@_" )}
                    } qw( catfile catdir );

                    mkdir File::Spec->catdir($name, $base);
                    mkdir $dir->($_) for qw(chrome defaults),
                                           "chrome $base",
                                           'defaults preferences';

                    open my $manifest, '>', $file->('chrome chrome.manifest');
                    print $manifest "content $base file:$base/";

                    open my $boot, '>', $file->('chrome', $base, 'boot.xul');{
                        no warnings 'redefine';
                        local *write = sub {pop};
                        print $boot $$self{dispatch}{'/'}();
                    }

                    open my $prefs, '>', $file->('defaults preferences prefs.js');
                    print $prefs qq {pref("toolkit.defaultChromeURI", "chrome://$base/content/boot.xul");};

                    open my $ini, '>', $app = $file->('application.ini');
                    print $ini split /[\t ]+/ => qq {
                        [App]
                        Name=$base
                        Version=$XUL::Gui::VERSION
                        BuildID=${\time}

                        [Gecko]
                        MinVersion=1.8
                        MaxVersion=3
                    }
                }
                if (not $$self{trusted} and $^O =~ /darwin/) {
                    system qq[osascript -e 'tell application "Firefox" to OpenURL "http://localhost:$$self{port}"']
                } else {
                    my ($chrome, $port) = @$self{qw/chrome port/};
                     unless (fork) {
                        $firefox[0][1] =~ tr./.\\. if $^O =~ /MSWin/;
                        exec qq{"$firefox[0][1]" }
                            . ($app
                                ? "-app $app"
                                : ($chrome ? '-chrome ' : '')
                                    . qq{"http://localhost:$port"}
                            ) . (q{ 1>&2 2>/dev/null} x ($^O !~ /MSWin/))
                    }
                }
            }
            else {local $silent; message 'firefox not found: start manually'}
        }
        $$self{run} = 1;
        $$self{cycle} = sub {
            if ($$self{dispatch}{$$req{URL}}) {
                $$self{dispatch}{$$req{URL}}->()
            } elsif (open my $file, '<', '.'.$$req{URL}) {
                $self->write('text/plain', do{local $/; <$file>})
            } else {
                message "file: $$req{URL} not found";
                $self->write('text/plain', '')
            }
        };
        eval {
            while ($$self{client} = $$self{server}->accept) {
                $$self{client}->autoflush(1);
                message 'client connected';
                while (local $req = $self->read) {
                    $$self{cycle}->();
                    $$self{run} or last
                }
                close $$self{client};
                $$self{run} or last
            }
        };
        my $error = $@;
        eval {$$self{client}->close}; $$self{client} = undef;
        eval {$$self{server}->close}; $$self{server} = undef;
        undef $active;
        {($$self{dir} or last)->unlink_on_destroy(1)}
        ref $error eq 'server stopped' or die $error if $error;
        $self->stop('server stopped')
    }

    sub safe_read {
        my ($self, $stop) = (@_, '/res');
        while (local $req = $self->read) {
            message "safe read $$req{URL}" if $DEBUG > 2;
            return $req if $$req{URL} eq $stop;
            $$self{cycle}->();
            die bless [] => 'server stopped' unless $$self{run}
        }
    }

    sub assert {
        return if $_[0];
        my $name = ((caller 2)[3] =~ /([^:]+)$/ ? "$1 " : '') . pop;
        croak "XUL::Gui> $name error: client not connected,"
    }

    sub read {
        my ($self, $client, %req) = ($_[0], $_[0]{client});
        assert $client => 'read';
        local ($/, $_) = "\015\012";
        reset;
        while (<$client>) {chomp;
            if (?^\s*\w+\s*([^\s]+)\s*HTTP?) {$req{ URL } = $1; next}
            if (/^\s*(.+?)\s*:\s*(.+?)\s*$/) {$req{lc $1} = $2; next}
            CORE::read $client => $req{CONTENT}, $req{'content-length'} || 0;
            last
        }
        for ($req{URL}) {
            if ($$self{delay})
                {usleep 1000*$$self{delay} unless m|^/ping$|}
            unless (defined) {
                croak 'broken message received';
            }
            ($_, $req{CONTENT}) = ($1, "OBJ $2")  if  /(.+?)\?(.*)/;
            message "read $_ $req{CONTENT}"       if  $DEBUG > 2 && !m|^/ping$|;
            if (m|^/perl$|) {
                message "perl $req{CONTENT}" if $DEBUG > 1;
                $self->write( 'text/plain', 'RETURN '.
                    (eval "no strict; package $$self{caller}; $req{CONTENT}" or '') );
                %req = %{ $self->read }
            }
        }
        \%req
    }

    sub write {
        assert my $client = $_[0]{client} => 'write';
        message "write @_[1,2]" if $DEBUG > 2;
        print $client join "\015\012" =>
            'HTTP/1.1 200 OK',
            'Expires: -1',
            'Keep-Alive: 300',
            'Connection: Keep-Alive',
            'Content-type: ' . $_[1],
            'Content-length: ' . length $_[2],
            '',
            $_[2]
    }

    sub stop {
        local $SIG{HUP} = 'IGNORE';
        kill HUP => -$$;
        message @_[1..$#_];
    }

    sub serve {
        my ($self, $path, $type, $data) = @_;
        $path =~ m[^/(?:client.js|event|ping|exit|perl)?$]
            and croak "reserved path: $path";
        $$self{dispatch}{$path} = sub {
            $self->write($type, $data)
        };
        $path
    }

    sub CLONE {
        eval {$$active{client}->close};
        eval {$$active{server}->close};
        eval {undef $$active{$_} for keys %$active};
    }

    $client_js = <<'END';

Object.prototype.__defineGetter__('__sid__', function () {
    var sid = 0;
    return function(){
        var id = sid++;
        this.__proto__ = {
             __proto__: this.__proto__,
             get __sid__(){ return id }
        };
        return id;
    }
}.call() );

Object.prototype.toString = function () {
    return '[Object ' + this.__sid__ + ']';
};

Function.prototype.cache = function () {
    var cid = 0;
    return function (strict, env) {
        var self  = this;
        var cache = {};
        env = env || null;
        var args;

        var getid = function (obj) {
            if (obj.__cid__) return obj.__cid__;
            var id = cid++;
            obj.__proto__ = {
                __proto__: obj.__proto__,
                get __cid__(){ return id }
            }
            return id;
        };

        var cached = function () {
            args = Array.prototype.slice.call(arguments);
            if (strict) args = args.map( function(arg) typeof arg == 'object' ?
                                            [arg.toSource(), getid(arg)] : arg );
            args = args.toSource();
            return (args in cache)
                ? cache[args]
                : cache[args] = self.apply(env, arguments);
        };

        cached.nocache = function () { return self };
        return cached;
    }
}.call();

Function.prototype.extends = Object.prototype.extends = function (super) {
   this.prototype.__proto__ = super.prototype;
   this.prototype.__super = super;
}

Object.prototype.__defineGetter__('keys', function ()  {
    var out = [];
    for (var i in this) {
        if (this.hasOwnProperty(i)) out.push(i);
    }
    return out;
});

Object.prototype.__defineGetter__('values', function () {
    return this.keys.map( function (k) this[k], this )
});

var id          = 0;
var ID          = {};
var noEvents    = {};
var cacheEvents = true;
var ping        = 250;
var interval    = setInterval( pinger, ping );
var host        = 'http://localhost:' + port + '/';
var queue       = [];
var mutex       = false;
var server      = new XMLHttpRequest();

function pinger () {
    if (mutex || !cacheEvents) return;
    EVT(null);
}

function shutdown () {
    return send('close','');
}

var retre = new RegExp(/^RETURN (.*)/);
function send (to, val) {
    var url = host + to;
    var resurl = host + 'res';
    var type;
    var realval;
    while (1) {
        server.open( 'POST', url, false );
        server.send( val );
        if (server.responseText != 'NOOP') {
            var ret = server.responseText.match(retre);
            if (ret) return eval (ret[1]);
            try {realval = eval( server.responseText )}
            catch(e) {
                if (e == 'quit') {
                    server = null;
                    return;
                }
                alert( [ e.name, server.responseText, e.message ].join("\n\n") )}

            url = resurl;
            val = realval;
            type = typeof val;

            if (val == 0 && type != 'string') {val = '0'}
            else if (val == null || val == undefined) {val = 'UND EF'; continue}

            if (val === true)  {val = 'RES 1'; continue}
            if (val === false) {val = 'RES 0'; continue}

            if (type == 'object') {
                if (val.hasAttribute && val.hasAttribute('id'))
                    {val = 'OBJ ' + val.getAttribute('id')}
                else {
                    ID[ 'xul_js_' + id ] = val;
                    val = 'OBJ xul_js_' + id++;
                }
            }
            else {val = 'RES ' + val}
        }
        else {break}
    }
    return realval;
}

var xul_id = function (obj) {
    do {
        if (obj.id) return obj.id
        obj = obj.parentNode
    } while (obj.parentNode && obj.parentNode != obj);
}.cache(true);

function EVT (event) {
    if (noEvents.__count__ > 0 && xul_id(event.target) in noEvents) return;
    if (mutex) {
        if(cacheEvents && event)
            queue.push(event);
        return;
    }
    mutex = true;
    var ret;
    do {
        if (event) {
            if (event.type == 'perl') {
                ret = send('perl', event.code);
                break;
            } else {
                ID['xul_js_' + id] = event;
                send('event', 'EVT ' + xul_id(event.target) +
                     ' ' + event.type + ' ' + ('xul_js_' + id++));
            }
        } else {
            send('ping', null)
        }
    } while (event = queue.shift());
    mutex = false;
    if (event) setTimeout(pinger, 10);
    return ret;
}

function GET (self, k) {
    if (typeof self.hasAttribute == 'function' && self.hasAttribute(k))
        return self.getAttribute(k);

    if (typeof self[k] == 'function')
        return self[k]();

    return self[k];
}

function SET (self, k, v) {
    if (typeof self.hasAttribute == 'function'
            && self.hasAttribute(k) )
        return self.setAttribute(k, v);

    return self[k] = v;
}

function quit () {
    clearInterval(interval);
    EVT = function(){};
    try {
        var appStartup = Components.classes[
            '@mozilla.org/toolkit/app-startup;1'
        ].getService(Components.interfaces.nsIAppStartup);
        appStartup.quit(Components.interfaces.nsIAppStartup.eForceQuit);
    } catch (e) {}
    try {
        window.close();
    } catch (e) {}
    throw 'quit';
}

function removeChildren (element) {
    while (element.firstChild)
        element.removeChild(element.firstChild);
}

function removeItems (element) {
    while (element.lastChild && element.lastChild.nodeName == 'listitem')
        element.removeChild(element.lastChild);
}

function scrollTo (element, x, y) {
    element.boxObject.QueryInterface(
        Components.interfaces.nsIScrollBoxObject
    ).scrollTo(x, y);
}

function perl (code) {
    return mutex ? send('perl', code)
                 : EVT({ type: 'perl', code: code })
}

Element.prototype.computed = function (style) {
    return document.defaultView.getComputedStyle( this, null ).getPropertyValue( style )
}

Element.prototype.noEvents = function (value) {
    return value
        ? noEvents[this] = true
        : delete noEvents[this]
}

END


=head1 CAVEATS

some options for display have been reworked from 0.36 to remove double negatives

widgets have changed quite a bit from version 0.36. they are the same under the
covers, but the external interface is cleaner. for the most part, the following
substitutions are all you need:

    $W       -->  $_ or $_{W}
    $A{...}  -->  $_{A}{...} or $_->attr(...)
    $C[...]  -->  $_{C}[...] or $_->child(...)
    $M{...}  -->  $_{M}{...} or $_->can(...)

    attribute 'label onclick'  -->  $_->has('label onclick')
    widget {extends ...}       -->  widget {$_->extends(...)}

export tags were changed a little bit from 0.36

thread safety should be better than in 0.36

currently it is not possible to open more than one window, hopefully this will
be fixed in the next release

the code that attempts to find firefox may not work in all cases, patches
welcome

for the TextBox object, the behaviors of the "value" and "_value" methods are
reversed. it works better that way and is more consistent with the behavior of
other tags.

=head1 AUTHOR

Eric Strom, C<< <asg at cpan.org> >>

=head1 BUGS

please report any bugs or feature requests to C<bug-xul-gui at rt.cpan.org>, or
through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=XUL-Gui>. I will be notified,
and then you'll automatically be notified of progress on your bug as I make
changes.

=head1 ACKNOWLEDGMENTS

the mozilla development team

=head1 COPYRIGHT & LICENSE

copyright 2009-2010 Eric Strom.

this program is free software; you can redistribute it and/or modify it under
the terms of either: the GNU General Public License as published by the Free
Software Foundation; or the Artistic License.

see http://dev.perl.org/licenses/ for more information.

=cut

__PACKAGE__ if 'first require'
