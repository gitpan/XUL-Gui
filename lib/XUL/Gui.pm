package XUL::Gui;
    use strict;
    use warnings;
    use Carp;
    use Exporter 'import';
    use Storable 'dclone';
    use List::Util 'max';
    use MIME::Base64 'encode_base64';
    our $VERSION = '0.30';
    our $DEBUG = 0;

=head1 NAME

XUL::Gui - render cross platform gui applications with firefox from perl

=head1 VERSION

version 0.30

this module is under active development, interfaces may change.

this code is currently in beta, use in production environments at your own risk

the code will be considered production ready, and interfaces finalized at version 0.50

this documentation is a work in progress

=head1 SYNOPSIS

    use XUL::Gui;
    display Label 'hello, world!';

    # short enough? s/Label/P/ for bonus points

    use XUL::Gui;
    display Window title => "XUL::Gui's long hello", minwidth=>300,
        GroupBox(
            Caption('XUL'),
            Button( label=>'click me', oncommand=> sub {shift->label = 'ouch'} ),
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
            TABLE( border=>1, TR map {TD $_} 'one', I('two'), B('three'), U('four'), SUP('five') ),
            HR,
            P('all the HTML tags are in CAPS'),
        );

=head1 DESCRIPTION

this module exposes the entire functionality of mozilla firefox's rendering
engine to perl by providing all of the XUL and HTML tags as functions and
allowing you to interact with those objects directly from perl.  gui applications
created with this toolkit are cross platform, fully support CSS styling, inherit
firefox's rich assortment of web technologies (browser, canvas and video tags, flash
and other plugins), and are even easier to write than HTML.

this module is written in pure perl, and only depends upon core modules, making it
easy to distribute your application.

all XUL and HTML objects in perl are exact mirrors of their javascript counterparts and can
be acted on as such.  for anything not written in this document or XUL::Gui::Manual,
developer.mozilla.com is the official source of documentation:

=over

=item * L<https://developer.mozilla.org/en/XUL>

=item * L<http://www.hevanet.com/acorbin/xul/top.xul> - XUL periodic table

=item * L<https://developer.mozilla.org/En/Documentation_hot_links>

=back

gui's created with this module are event driven.  an arbitrarily complex (and runtime mutable)
object tree is passed to C<display>, which then creates the gui in firefox and starts the
event loop.  C<display> will wait for and respond to events until the C<quit> function is
called, or the user closes the firefox window.

all of javascript's event handlers are available, and can be written in perl (normally)
or javascript (for handlers that need to be very fast such as image rollovers with
onmouseover or the like).  this is not to say that perl side handlers are slow, but with
rollovers and fast mouse movements, sometimes there is mild lag due to protocol overhead.

the goal of this module is to make gui development as easy as possible. XUL's widgets and
nested design structure gets us most of the way there, and this module with its light weight
syntax, and "Do What I Mean" nature hopefully finishes the job.  everything has sensible
defaults with minimal boilerplate, and nested design means a logical code flow that isn't
littered with variables.  now you can focus on your gui's design and functionality, and
hopefully not on the deficiencies of your toolkit. if XUL::Gui doesn't get you all the way
there yet, give it time, I'm still working on it.

=head2 tags

all tags (C<XUL>, C<HTML>, user defined widgets, and the C<display> function) are parsed
the same way, and can fit into one of four templates

=over 8

=item * C<< HR() is <hr /> >>

=item * C<< B('some bold text') is <b>some bold text<b/> >>

in the special case of a tag with one argument, which is not another tag, that argument is
added to that tag as a text node. this is mostly useful for HTML tags, but works with XUL as well

=item * C<< Label( value=>'some text', style=>'color: red' ) >>

    <label value="some text" style="color: red;" />

=item * C<< Hbox( id=>'mybox', Label('hello'), B('world'), style=>'border: 1px solid black') >>

    <hbox id="mybox" style="border: 1px solid black;">
        <label>hello</label>
        <b>world</b>
    </hbox>

unlike XML based XUL, attribute pairs and children can be mixed in any order,
but attributes should probably be kept at the front for readability

=back

setting the 'id' attribute names the object in the global C<%ID> hash.
otherwise an auto generated name matching C</^xul_\d+$/> is used.

    $object = Button( id=>'btn', label=>'OK' );

    #  $ID{btn} == $object

any tag attribute name that matches C</^on/> is an event handler (onclick, onfocus....), and
expects a C<sub{...}> (perl event handler) or C<function q{...}> (javascript event handler).

perl event handlers get passed a reference to themselves, and an event object

    Button( label=>'click me', oncommand=> sub {
        my ($self, $event) = @_;
        $self->label = $event->type;
    })

javascript event handlers have C<event> and C<this> set for you

    Button( label=>'click me', oncommand=> function q{
        this.label = event.type;
    })

any attribute with a name that doesn't match /^on/ that has a code ref value is added
to the object as a method

Tk's attribute style with a leading dash is supported.
this is useful for readability when collapsing attribute lists with C<qw//>

    TextBox id=>'txt', width=>75, height=>20, type=>'number', decimalplaces=>4;
    TextBox qw/-id txt -width 75 -height 20 -type number -decimalplaces 4/;

multiple 'style' attributes are joined with ';' into a single attribute

=head1 EXPORT

    use XUL::Gui;   # is the same as
    use XUL::Gui qw/:base :util :widget :pragma :xul :html :const :image/;

    the following export tags are available.  :base is required for minimal functionality

    :base       %ID display quit alert function gui XUL
    :widget     @C %A %M $W widget attribute extends
    :pragma     buffered now cached noevents delay doevents
    :const      FLEX FIT FILL SCROLL MIDDLE
    :image      bitmap bitmap2src serve
    :util       zip mapn apply trace toggle
    :internal   tag object genid

    :all    (all exports)

    :xul    (also exported as Titlecase)
        Action ArrowScrollBox Assign BBox Binding Bindings Box Broadcaster BroadcasterSet
        Browser Button Caption CheckBox ColorPicker Column Columns Command CommandSet Conditions
        Content DatePicker Deck Description Dialog DialogHeader DropMarker Editor Grid Grippy
        GroupBox HBox IFrame Image Key KeySet Label ListBox ListCell ListCol ListCols ListHead
        ListHeader ListItem Member Menu MenuBar MenuItem MenuList MenuPopup MenuSeparator
        Notification NotificationBox Observes Overlay Page Panel Param PopupSet PrefPane PrefWindow
        Preference Preferences ProgressMeter Query QuerySet Radio RadioGroup Resizer RichListBox
        RichListItem Row Rows Rule Scale Script ScrollBar ScrollBox ScrollCorner Separator Spacer
        SpinButtons Splitter Stack StatusBar StatusBarPanel StringBundle StringBundleSet Tab TabBox
        TabPanel TabPanels Tabs Template TextBox TextNode TimePicker TitleBar ToolBar ToolBarButton
        ToolBarGrippy ToolBarItem ToolBarPalette ToolBarSeparator ToolBarSet ToolBarSpacer
        ToolBarSpring ToolBox ToolTip Tree TreeCell TreeChildren TreeCol TreeCols TreeItem TreeRow
        TreeSeparator Triple VBox Where Window Wizard WizardPage

    :html   (also exported as html_lowercase)
        A ABBR ACRONYM ADDRESS APPLET AREA AUDIO B BASE BASEFONT BDO BGSOUND BIG BLINK BLOCKQUOTE
        BODY BR BUTTON CANVAS CAPTION CENTER CITE CODE COL COLGROUP COMMENT DD DEL DFN DIR DIV DL DT
        EM EMBED FIELDSET FONT FORM FRAME FRAMESET H1 H2 H3 H4 H5 H6 HEAD HR HTML I IFRAME ILAYER IMG
        INPUT INS ISINDEX KBD LABEL LAYER LEGEND LI LINK LISTING MAP MARQUEE MENU META MULTICOL NOBR
        NOEMBED NOFRAMES NOLAYER NOSCRIPT OBJECT OL OPTGROUP OPTION P PARAM PLAINTEXT PRE Q RB RBC RP
        RT RTC RUBY S SAMP SCRIPT SELECT SMALL SOURCE SPACER SPAN STRIKE STRONG STYLE SUB SUP TABLE
        TBODY TD TEXTAREA TFOOT TH THEAD TITLE TR TT U UL VAR VIDEO WBR XML XMP

    for the OO purists:

        use XUL::Gui ();
        my $gui = XUL::Gui->new;
        $gui->display( $gui->Label('hello world') );
        # use $gui->id('someid') to access the %ID hash;

=cut

    our @Xul = map {$_, (ucfirst lc) x /.[A-Z]/}
        qw/Action ArrowScrollBox Assign BBox Binding Bindings Box Broadcaster BroadcasterSet
        Browser Button Caption CheckBox ColorPicker Column Columns Command CommandSet Conditions
        Content DatePicker Deck Description Dialog DialogHeader DropMarker Editor Grid Grippy
        GroupBox HBox IFrame Image Key KeySet Label ListBox ListCell ListCol ListCols ListHead
        ListHeader ListItem Member Menu MenuBar MenuItem MenuList MenuPopup MenuSeparator
        Notification NotificationBox Observes Overlay Page Panel Param PopupSet PrefPane PrefWindow
        Preference Preferences ProgressMeter Query QuerySet Radio RadioGroup Resizer RichListBox
        RichListItem Row Rows Rule Scale Script ScrollBar ScrollBox ScrollCorner Separator Spacer
        SpinButtons Splitter Stack StatusBar StatusBarPanel StringBundle StringBundleSet Tab TabBox
        TabPanel TabPanels Tabs Template TextBox TextNode TimePicker TitleBar ToolBar ToolBarButton
        ToolBarGrippy ToolBarItem ToolBarPalette ToolBarSeparator ToolBarSet ToolBarSpacer
        ToolBarSpring ToolBox ToolTip Tree TreeCell TreeChildren TreeCol TreeCols TreeItem TreeRow
        TreeSeparator Triple VBox Where Window Wizard WizardPage/;

    our %HTML = map {("html_$_" => "html:$_", uc $_ => "html:$_")}
        qw/a abbr acronym address applet area audio b base basefont bdo bgsound big blink blockquote
        body br button canvas caption center cite code col colgroup comment dd del dfn dir div dl dt
        em embed fieldset font form frame frameset h1 h2 h3 h4 h5 h6 head hr html i iframe ilayer img
        input ins isindex kbd label layer legend li link listing map marquee menu meta multicol nobr
        noembed noframes nolayer noscript object ol optgroup option p param plaintext pre q rb rbc rp
        rt rtc ruby s samp script select small source spacer span strike strong style sub sup table
        tbody td textarea tfoot th thead title tr tt u ul var video wbr xml xmp/;

    our %EXPORT_TAGS = (
        util     => [qw/zip mapn apply trace toggle/],
        base     => [qw/%ID display quit alert function gui XUL/],
        widget   => [qw/@C %A %M $W widget attribute extends/],
        pragma   => [qw/buffered now cached noevents delay doevents/],
        xul      => [@Xul],
        html     => [keys %HTML],
        const    => [qw/FLEX FIT FILL SCROLL MIDDLE/],
        image    => [qw/bitmap bitmap2src serve/],
        internal => [qw/tag object genid/],
    );
    our @EXPORT_OK    = map @$_ => values %EXPORT_TAGS;
    $EXPORT_TAGS{all} = \@EXPORT_OK;
    our @EXPORT       = map @{ $EXPORT_TAGS{$_} } =>
                            qw/util base widget pragma xul html const image/;
    our %defaults = (
        window      => ['xmlns:html' => 'http://www.w3.org/1999/xhtml',
                         xmlns       => 'http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul' ],
        scrollbox   => [ scrollTo    => sub {my ($self, $x, $y) = @_; gui("scrollTo(ID.$$self{ID}, $x, $y);")} ],
        textbox     => [  value      => sub :lvalue {tie my $ret, 'XUL::Gui::Scalar', shift, '_value'; $ret},
                         _value      => sub :lvalue {tie my $ret, 'XUL::Gui::Scalar', shift,  'value'; $ret} ],
    );
    our $server ||= XUL::Gui::Server->init;
    our (%ID, %_ID, %dialogs);

{package
    XUL::Gui::OO;
    use strict;
    use warnings;
    our %lookup = map {lc, $_} @{ $EXPORT_TAGS{xul} };
    sub AUTOLOAD {
        shift;
        no strict 'refs';
        my ($sub) = our $AUTOLOAD =~ /([^:]+)$/;
        goto &{'XUL::Gui::' . ($lookup{lc $sub} || $sub)};
    }
    sub DESTROY {}
    sub id :lvalue {$XUL::Gui::ID{$_[1]}}
    sub ID :lvalue {$XUL::Gui::ID{$_[1]}}
}
    sub new {bless {} => 'XUL::Gui::OO'}

=head1 FUNCTIONS

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
        my $ret = defined wantarray;
        while (@_) {
            local *_ = \$_[0];
            if ($ret) {push @ret =>
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

    toggle $state => 0, 1;

=cut

    sub toggle {
        no warnings;
        $_[0] = $_[ 1 + ($_[0] eq $_[1] or $_[0] ne $_[2]) ]
    }

=item C<bitmap WIDTH HEIGHT OCTETS>

returns a binary .bmp bitmap image. OCTETS is a list of BGR values

    bitmap 2, 2, qw(255 0 0 255 0 0 255 0 0 255 0 0); # 2px blue square

=cut

    sub bitmap {
        my ($width, $height, @bitmap) = splice @_, 0, 2;
        my $pad = $width % 4;
        $pad = 4 - $pad if $pad;
        for (1 .. $height) {
            unshift @bitmap, splice( @_, 0, $width*3 ), (0) x $pad
        }
        pack 'n V n n (N)2 (V)2 n n N V (N)4 (C)*' =>
            0x42_4D,         # "BM"
            (54 + @bitmap),  # file size
            0x00_00,         # not used
            0x00_00,         # not used
            0x36_00_00_00,   # offset of bitmap data
            0x28_00_00_00,   # remaining bytes in header
            $width,
            $height,
            0x01_00,         # color planes (always 1)
            0x18_00,         # bits/pixel (24)
            0x00_00_00_00,   # no compression
            scalar(@bitmap), # size of raw BMP data (after header)
            0x13_0B_00_00,   # horizontal resolution (2,835 pixels/meter)
            0x13_0B_00_00,   # vertical resolution (2,835 pixels/meter)
            0x00_00_00_00,   # not used
            0x00_00_00_00,   # not used
            @bitmap
    }


=item C<bitmap2src WIDTH HEIGHT OCTETS>

returns a packaged bitmap image that can be directly assigned to an image
tag's src attribute. arguments are the same as C<bitmap()>

    $ID{myimage}->src = bitmap2src 320, 180, @image_data;

=cut

    sub bitmap2src {
        'data:image/bitmap;base64,' . encode_base64 &bitmap
    }

    {my $id = 0;
        sub genid () {'xul_' . $id++}
    }

    sub isa {UNIVERSAL::isa( (@_, $_)[1, 0] )}

    sub parse {
        my (@C, %A, %M);
        while (local $_ = shift) {
            if (isa 'XUL::Gui::Object') {push @C, $_; next}
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

=back

=head2 constants

    FLEX    flex => 1
    FILL    flex => 1, align =>'stretch'
    FIT     sizeToContent => 1
    SCROLL  style => 'overflow: auto'
    MIDDLE  align => 'center', pack => 'center'

    each is a function that returns its constant, prepended to
    its arguments, thus the following are both valid:

    Box FILL pack=>'end';
    Box FILL, pack=>'end';

=cut

sub FLEX   {flex  => 1,                   @_}
sub FILL   {align => 'stretch', FLEX,     @_}
sub FIT    {sizeToContent => 1,           @_}
sub SCROLL {style => 'overflow: auto',    @_}
sub MIDDLE {qw/align center pack center/, @_}

=head2 gui functions

=over 8

=item C<display LIST>

starts the http server, launches firefox, waits for events

takes a list of gui objects, and several optional parameters:

    debug     (0) .. 3   adjust verbosity to stderr
    silent    (0) 1      disables all status messages
    nolaunch  (0) 1      disables launching firefox, connect manually to http://localhost:8888
    nochrome  (0) 1      chrome mode disables all normal firefox gui elements, setting this
                         option will turn those elements back on.
    port      (8888)     first port to try starting the server on, port++ after that
    delay  milliseconds  delays each gui update cycle

if the first object is a C<Window>, that window is created, otherwise a default
one is added. the remaining objects are then added to the window.

C<display> will not return until the the gui quits

see SYNOPSYS and XUL::Gui::Manual for more details

=cut
    sub display {$server->start( &parse )}

    sub dialog {croak 'dialog not implemented yet'}

=item C<quit>

shuts down the server (causes a call to C<display> to return at the end of the current event cycle)

=cut
    sub quit {
        gui('quit();') unless @_;
        $$server{run} = 0;
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
        my $tag = lc shift;
        unshift @_, @{ $defaults{$tag} } if $defaults{$tag};
        bless my $self = {
            TAG   => $tag,
            DIRTY => $tag,
            &parse
        } => 'XUL::Gui::Object';
        $$self{ID} = $$self{A}{id} ||= genid;
        $tag ? $ID{ $$self{ID} } = $self
             : $self;
    }

=item C<tag NAME>

returns a code ref that generates proxy objects, allows for user defined tag functions

    *mylabel = tag 'label';

    \&mylabel == \&Label

=cut
    sub tag {
        my @args = @_;
        sub {
            object @args,
                (@_ == 1 and not isa 'XUL::Gui::Object' => $_[0])
                    ? 'TEXT' : (),
                @_
        }
    }

    no strict 'refs';

    *$_ = tag $_        for @Xul;       # dr tagfunction,
    *$_ = tag $HTML{$_} for keys %HTML; # or how i learned to fail pod-coverage.t and love no strict


=item C<widget {CODE} HASH>

group tags together into common patterns, with methods and inheritance

    *MyWidget = widget {
        Hbox(
            Label( value=> $A{label} ),
            Button( label=>'OK', attribute 'oncommand' ),
            @C
        )
    }   method  => sub{ ... },
        method2 => sub{ ... };

    $ID{someobject}->appendChild( MyWidget( label=>'widget', oncommand=>\&event_handler ) );

    inside widgets, several variables are defined
    variable    contains the passed in
       %A           attributes
       @C           children
       %M           methods
       $W           a reference to the current widget

    much more detail in XUL::Gui::Manual

=cut
    sub widget (&%) {
        my ($code, %methods, $sub) = @_;
        $sub = sub {
            my %data;
            while (my ($k, $v) = each %methods)
                {$data{$k} = dclone $v if ref $v ne 'CODE'}

            my $subwidget = defined %_ID;
            local *_ID = \%ID unless $subwidget;

            my %arg = parse @_;
            my $wid = $subwidget ? genid : $arg{A}{id} || genid;
            @arg{qw/M T ID/} = ({ %methods, %{ $arg{M} } }, $sub, $wid);

            my ($C, $A, $M, $W, $cID) = map {(caller)."::$_"} qw/C A M W ID/;
            local (*$C, *$A, *$M)     = @arg{qw/C A M/};
            local $$W = $_ID{$wid}    = bless {%data, %arg} => 'XUL::Gui::Object';
            $ID{$$A{id} || genid }    = $_ID{$wid} if $subwidget;

            local %ID;
            local *$cID = \%ID;
            $_ID{$wid}{WIDGET} = [ &$code ]; # NOT FINAL

            for my $k (keys %data) { $$M{$k} = sub : lvalue {$data{$k}} }
            $_ID{$wid}{M} = { %methods, %$M };

            for my $i (keys %ID) { # refactor to reduce copying
                $_ID{$wid}{$i} = $_ID{ my $gid = genid } = $ID{$i};
                next unless isa 'XUL::Gui::Object' => $ID{$i};
                $ID{$i}{N}    = $ID{$i}{A}{id};
                $ID{$i}{ID}   = $ID{$i}{A}{id} = $gid;
                $ID{$i}{W}    = $_ID{$wid};
                $ID{$i}{$_} ||= $ID{$_} for keys %ID;
                $ID{$i}{$_} ||= $$A{$_} for keys %$A;
            }
            wantarray ? @{ $_ID{$wid}{WIDGET} } : $_ID{$wid}{WIDGET}[0];
        }
    }

=item C<extends OBJECT>

indicate that a widget inherits from another widget or tag

    *MySubWidget = widget {extends MyWidget}
        submethod => sub{...};

    more details in XUL::Gui::Manual

=cut
    sub extends {
        croak 'extends only works inside widgets' unless defined %_ID;
        $ID{$_} = $_[0]{W}{$_} for grep {/[a-z]/} keys %{ $_[0]{W} };
        %{ (caller).'::M' } = %{ $_[0]{W}{M} };
        @_
    }

=item C<attribute NAME>

includes an attribute name if it exists, only works inside of widgets.
NAME is split on whitespace

    attribute 'label type' # is syntactic sugar for
    map {$_ => $A{$_}} grep {exists $A{$_}} qw/label type/

    attribute '+' # same as %A
    attribute '*' # any untouched attributes (by attribute)

=cut
    sub attribute ($) {
        croak 'attribute only works inside widgets' unless defined %_ID;
        my ($A, $W, @ret) = ((caller).'::A', (caller).'::W');
        my @keys = grep {
            $_ eq '*' ? do {push @ret, %$A; 0} :
            $_ eq '+' ? do {push @ret, map {$_ => $$A{$_}}
                grep {not $$W{SEEN}{$_}} keys %$A; 0
            } : 1
        } split /\s+/ => shift;
        $$W{SEEN}{$_}++ for @keys;
        map {$_ => $$A{$_}} grep {exists $$A{$_}} @keys
    }


=item C<XUL STRING>

converts an XML XUL string to XUL::Gui objects.  experimental.

this function is provided to facilitate drag and drop of XML based XUL from tutorials for testing.
the perl functional syntax for tags should be used in all other cases

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
                or carp "content skipped due to parse failure: $@\n\n$_";
        }
    }}

=item C<alert STRING>

open an alert message box

=cut
    sub alert {
        gui( "alert('\Q@_\E');" );
        wantarray ? @_ : pop
    }

=item C<trace LIST>

carps LIST with object details, and then returns LIST unchanged

=cut
    sub trace {
        my $caller = caller;
        carp 'trace: ', join ', ' => map {
            (isa 'XUL::Gui::Object') ? lookup($_, $caller) : $_
        } @_;
        wantarray ? @_ : pop
    }

    {my %cache;
    sub lookup {
        my $self = shift;
        return $cache{$self} if $cache{$self};
        return $$self{ID} unless $$self{W} || $$self{T};
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
        $$self{ID}
    }}

    use strict 'refs';

    sub calltrace {
        my $self = $_[0];
        my $caller = caller 1;
        our $AUTOLOAD =~ /([^:]+)$/;
        lookup($self, $caller) . "->$1(" .
            (join ',' => map {
                (isa 'XUL::Gui::Object') ? lookup($_, $caller) : $_
            } @_[1..$#_]) . ")\n";
    }


=item C<function JAVASCRIPT>

create a javascript function, useful for functions that need to be very fast, such as rollovers

    Button( label=>'click me', oncommand=> function q{
        this.label = 'ouch';
        alert('hello from javascript');
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
                gui( "$func = function (event) {
                    (function(){ $js }).call( ID.$$self{ID} )
                }" )
            });
            "$func(event)"
        } ] => 'XUL::Gui::Function'
    }

=item C<gui JAVASCRIPT>

executes JAVASCRIPT

=back

=cut

    {my ($buffered, @buffer, $cached, %cache, $now);
        sub gui :lvalue {
            push @_, "\n";
            unless ($now) {
                push @buffer, @_ and return if $buffered;
                return $cache{$_[0]} if exists $cache{$_[0]}
            }
            $server->write('text/plain', "@_");
            my $res = $server->safe_read->{CONTENT};
            croak "invalid response: $res" unless $res =~ /^(...) (.*)/;

            $res = $1 eq 'OBJ' ?
                ($ID{$2} || object undef, id=>$2) : $2;
            $cache{$_[0]} = $res if $cached and $_[0] =~ /^(GET|0)/;
            $res
        }

=head1 PRAGMATIC BLOCKS

the following functions all apply pragmas to their CODE blocks.
in some cases, they also take a list. this list will be C<@_> when
the CODE block executes.  this is useful for sending in values
from the gui, if you don't want to use a C<now {block}>

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
            gui(@buffer),
                @buffer = () unless --$buffered;
        }

=item C<cached {CODE}>

turns on caching of gets from the gui

=cut
        sub cashed (&) {
            $cached++;
            my $ret = shift->();
            %cache = () unless --$cached;
            $ret;
        }

=item C<now {CODE}>

execute immediately, from inside a buffered or cached block

=cut
        sub now (&) {
            $now++;
            my @ret = shift->();
            $now--;
            wantarray ? @ret : $ret[0];
        }
    }

=item C<delay {CODE} LIST>

delays executing its CODE until the next gui refresh

useful for triggering widget initialization code that needs to
run after the gui objects are rendered

=cut
    sub delay (&@) {
        my $code = shift;
        my @args = @_;
        push @{$$server{queue}}, sub{ $code->(@args) };
        return;
    }

=item C<noevents {CODE} LIST>

disable event handling

=cut
    sub noevents (&@) {
        gui('cacheEvents = false;');
        my @ret = &{+shift};
        gui('cacheEvents = true;');
        @ret;
    }

=item C<doevents>

force a gui update before an event handler finishes

=back

=cut
    sub doevents () {
        $server->write('text/plain', 'NOOP');
        $server->read; # should be safe_read
        return;
    }

=head1 METHODS

    # access attributes and properties

        $object->value = 5;                     # sets the value in the gui
        print $object->value;                   # gets the value from the gui

    # the attribute is set if it exists, otherwise the property is set

        $object->_value = 7;                    # sets the property directly

    # method calls

        $object->focus;                         # void context
        $object->appendChild( H2('title') );    # or any arguments are always method calls
        print $object->someAccessorMethod_;     # append _ to force interpretation as a JS method call

in addition to mirroring all of an object's existing javascript methods / attributes / and properties
to perl (with identical spelling / capitalization), several default methods have been added to all objects

=over 8

=cut

package
    XUL::Gui::Object;
    use warnings;
    use strict;
    my $search; $search = sub {
        my ($self, $method) = @_;
        $self->{M}{$method} or do {
            for (@{$$self{C}})
                {defined and return $_ for $search->($_, $method)}
        }
    };

    sub AUTOLOAD :lvalue {
        my $self = $_[0];
        return unless (my $AL) = our $AUTOLOAD =~ /([^:]+)$/;

        if (my $method = $search->($self, $AL)) { # perl method call
            if (ref $method eq 'ARRAY') {
                splice @_, 0, 1, $$method[0];
                $method = $$method[1];
            }
            if ($XUL::Gui::DEBUG) {
                my $caller = caller;
                print XUL::Gui::lookup($self, $caller) . "->$1(" .
                    (join ',' => map {(XUL::Gui::isa XUL::Gui::Object)
                        ? XUL::Gui::lookup($_, $caller) : $_} @_[1..$#_]). ")\n"
            }
            goto &$method
        }
        shift;
        if ($AL =~ s/_$// or @_ or not defined wantarray) { # js method call
            my ($js, $arg) = ('') x 2;
            $_->($self), return for $$self{uc $AL} or ();
            $arg = join ',', map {
                XUL::Gui::isa XUL::Gui::Object and do {
                    if ($_->{DIRTY}) {
                        ($$_{W} ? $$_{W}{W} : $$_{W}) ||= $$self{W};
                        $$_{P} ||= $self;
                        $js .= $_->toJS;
                    } "ID.$_->{ID}" }  or  "'\Q$_\E'"
                } @_;
            return XUL::Gui::gui($js . "ID.$self->{ID}.$AL($arg);")
        }
        tie my $ret, 'XUL::Gui::Scalar', $self, $AL; # proxy
        $ret
    }

    sub DESTROY {}

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

        $self->{DIRTY} = 0;
        $self->registerEvents;
        defined and return $_ for $$self{CODE};

        push @xul, "<$self->{TAG} ";
        push @xul, qq{$_="$self->{A}{$_}" } for keys %{$self->{A}};
        if (@{$$self{C}}) {
            push @xul, ">\n";
            push @xul, "\t" x ($tab+1),  $_->toXUL($tab+1) for @{$$self{C}};
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
            my $val = quotemeta(
                ref $$self{A}{$_} eq 'XUL::Gui::Function'
                    ? $$self{A}{$_}[0]( $self )
                    : $$self{A}{$_}
            );
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

removes the children in LIST, or all children if none given

=cut
    sub removeChildren {
        my $self = shift;
        @_  ? XUL::Gui::buffered {$self->removeChild($_) for @_} @_
            : XUL::Gui::gui "removeChildren(ID.$self->{ID});";
        $self
    }

=item C<< ->removeItems( LIST ) >>

removes the items in LIST, or all items if none given

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
                last if $first eq 'null';
                $first = $first->nextSibling;
            }
        }
        $first eq 'null'
            ? $self->appendChild( $child )
            : $self->insertBefore( $child, $first );
        $self
    }

=item C<< ->appendItems( LIST ) >>

append a list of items

=cut
    sub appendItems {
        my ($self, @items) = @_;
        XUL::Gui::buffered {
            (XUL::Gui::isa XUL::Gui::Object)
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
    use warnings;
    use strict;
    use Carp;

    sub TIESCALAR {bless [ @_[1..$#_] ] => $_[0]}

    sub FETCH {
        my ($self, $AL) = @{+shift};
        return $self->{uc $AL} if $AL =~ /^on/;
        XUL::Gui::gui $AL =~ /^_(.+)/
            ? "0;ID.$self->{ID}\['$1'];"
            : "GET(ID.$self->{ID}, '$AL');"
    }

    sub STORE {
        my ($self, $AL, $new) = (@{+shift}, @_);
        if ($AL =~ /^on/) {
            if (ref $new eq 'XUL::Gui::Function') {
                XUL::Gui::gui $$new[0];
                $new = $$new[1]
            } else {
                not defined $new or ref $new eq 'CODE'
                    or croak "assignment to event handler must be CODE ref or undef";
                $new = $new ? do {$$self{uc $AL} = $new; 'EVT(event)'} : '';
            }
        }
        XUL::Gui::gui $AL =~ /^_(.+)/
            ? "1;ID.$self->{ID}\['$1'] = '\Q$new\E';"
            : "SET(ID.$self->{ID}, '$AL', '\Q$new\E');"
    }


package
    XUL::Gui::Server;
    use warnings;
    use strict;
    use Carp;
    use File::Find;
    use IO::Socket;
    use Time::HiRes qw/usleep/;

    my $port = 8888;
    our ($req, $client_js, $silent);

    sub init {bless {}}

    sub message {print STDERR "XUL::Gui> @_\n" unless $silent}

    sub start {
        no strict;
        my $self    = shift || init;
        my %args    = @_;
        my @content = @{ $args{C} };
        my %params  = %{ $args{A} };

        local $silent = $params{silent};

        @$self{keys %params} = values %params;

        $$self{caller} = caller 1;

        message "version $VERSION" if
            local $DEBUG = $params{debug} || $DEBUG;

        my $port = $params{port} || $port++;
        $port++ until $$self{server} = IO::Socket::INET->new(
            Proto     => 'tcp',
            PeerAddr  => 'localhost',
            LocalAddr => "localhost:$port",
            ReuseAddr => 1,
            Listen    => 1,
            Reuse     => 1,
        );

        $| = 1;
        message "server started: http://localhost:$port";

        my $root;
        $$self{dispatch} = {
            exists $$self{dispatch} ? %{$$self{dispatch}} : (),
            '/' => sub {
                my $res = qq{<?xml version="1.0"?>\n<?xml-stylesheet }.
                          qq{href="chrome://global/skin" type="text/css"?>\n};

                $root = $content[0]->{TAG} eq 'window'
                            ? shift @content
                            : XUL::Gui::Window();

                $$self{onunload} ||= $$root{A}{onunload};
                $$root{A}{onunload} = q{ send('exit','') };
                unshift @content, @{ $root->{C} };
                $root->{C} = [ XUL::Gui::Script(src=>'/client.js') ];
                $self->write('application/vnd.mozilla.xul+xml', $res . $root->toXUL);
            },
            '/client.js' => sub {
                $self->write( 'text/javascript', qq~
                    var port = $port;
                    $client_js;
                    var root = ID.$root->{ID} = document.getElementById('$root->{ID}');~ .
                    join ";" => map {$_->toJS("ID.$root->{ID}")} @content
                )
            },
            '/event' => sub {
                message "event $req->{CONTENT}" if $XUL::Gui::DEBUG > 1;
                my ($code, $id, $evt, $obj) = split ' ', $req->{CONTENT};
                $evt = "ON\U$evt";
                $id  = $root->{ID} if $id eq 'undefined'; # why doesn't onunload window send id?
                $id  = $ID{$id}{P}{ID} while $ID{$id}{P} and not $ID{$id}{$evt}; # more xul bugs
                if (ref $ID{$id}{$evt} eq 'CODE') {
                    $ID{$id}{$evt} -> ( $ID{$id}, XUL::Gui::object(undef, id=>$obj) )
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
            '/exit' => sub {
                $$self{run} = 0;
                no warnings;
                local @CARP_NOT = qw/XUL::Gui XUL::Gui::Server XUL::Gui::Object/;
                local *XUL::Gui::gui = sub {croak 'gui not available'};
                use warnings;
                ref eq 'CODE' and $_->() for $$self{onunload};
            }
        };
        unless ($params{nolaunch}) {
            if ($^O =~ /darwin/) {
                system qq[osascript -e 'tell application "Firefox" to OpenURL "http://localhost:$port"']
            } else {
                my @firefox;
                for my $dir (
                    ($^O =~ /MSWin/)
                        ? (map {chomp; "$_\\"} split ',' => `echo \%ProgramFiles\%,\%ProgramFiles(x86)\%`)
                        : split /[:;]/ => $ENV{PATH}
                ) {
                    find sub {push @firefox, [$_, $File::Find::name] if /^firefox(?:-bin|\.exe)?$/ and -f} => $_
                        for grep {/mozilla|firefox/i} map {"$dir/$_"} grep -d, (-d $dir and chdir $dir) ? <*> : ()
                }
                if (@firefox = sort {length $$a[0] < length $$b[0]} @firefox) {
                    message 'launching firefox';
                    unless ($$self{pid} = fork) {
                        $firefox[0][1] =~ tr./.\\. if $^O =~ /MSWin/;
                        exec qq{"$firefox[0][1]" }
                            . ($params{nochrome} ? '' : '-chrome ')
                            . ($params{trusted}
                                ? qq{"chrome://xulgui/content/xulgui.xul?$port"}
                                : qq{"http://localhost:$port"}
                            ).(q{ 1>&2 2>/dev/null} x ($^O !~ /MSWin/))
                    }
                }
                else {local $silent; message 'firefox not found: start manually'}
            }
        }
        $$self{run} = 1;
        $$self{cycle} = sub {
            if ($$self{dispatch}{$$req{URL}}) {
                $$self{dispatch}{$$req{URL}}->()
            } elsif (open FILE, '.'.$$req{URL}) {
                $self->write('text/plain', do{local $/; <FILE>})
            } else {
                message "file: $$req{URL} not found";
                $self->write('text/plain', '')
            }
        };
        while ($$self{client} = $$self{server}->accept) {
            $$self{client}->autoflush(1);
            message 'client connected';
            while (local $req = $self->read) {
                $$self{cycle}->();
                last unless $$self{run}
            }
            close $$self{client};
            last unless $$self{run}
        }
        $self->stop('server stopped')
    }

    sub safe_read {
        my $self = shift;
        while (local $req = $self->read) {
            message "safe read $$req{URL}" if $DEBUG > 2;
            return $req if $$req{URL} eq '/res';
            $$self{cycle}->();
            return unless $$self{run}
        }
    }

    sub read {
        my ($self, $client, %req) = ($_[0], $_[0]{client});
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
                {usleep 1000*$$self{delay} unless m|/ping|}
            unless (defined) {
                $self->write( 'text/javascript', 'alert("error: broken message received")' );
                die 'broken message received';
            }
            ($_, $req{CONTENT}) = ($1, "OBJ $2")  if  /(.+?)\?(.*)/;
            message "read $_ $req{CONTENT}"       if  $DEBUG > 2 && !m|/ping|;
            if (m|/perl|) {
                $self->write( 'text/plain', 'RETURN '.
                    (eval("no strict;package $$self{caller}; $req{CONTENT}") || '' ));
                %req = %{ $self->read }
            }
        }
        \%req
    }

    sub write {
        message "write @_[1,2]" if $DEBUG > 2;
        print {$_[0]{client}} join "\015\012" =>
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
        message @_[1..$#_];
        local $SIG{HUP} = 'IGNORE';
        kill 1, -($^O =~ /MSWin/ ? shift->{pid} : $$);
    }

    sub serve {
        my ($self, $path, $type, $data) = @_;
        $path =~ m[^/(?:client.js|event|ping|exit)?$]
            and croak 'reserved path';
        $$self{dispatch}{$path} = sub {
            $self->write($type, $data)
        };
        $path
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
    mutex = true;
    send('ping', null);
    mutex = false;
}

var retre = new RegExp(/^RETURN (.*)/);
function send (to, val) {
    var url = host + to;
    var resurl = host + 'res';
    var type;
    while (1) {
        server.open( 'POST', url, false );
        server.send( val );
        if (server.responseText != 'NOOP') {
            var ret = server.responseText.match(retre);
            if (ret) return ret[1];
            try {val = eval( server.responseText )}
            catch(e) {alert( [ e.name, server.responseText, e.message ].join("\n\n") )}

            type = typeof val;

            if (val == 0 && type != 'string') {val = '0'}
            else if (val == null) {val = 'null'}

            url = resurl;

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
    return val;
}
/*
function send (to, val) {
    perl.open( 'POST', server + to );
    perl.send( val );
}

perl.onreadystatechange = function () {
    if (perl.readyState != 4) return;
    var val;
    if (perl.responseText != 'NOOP') {
        var ret = perl.responseText.match(retre);
        if (ret) return ret[1];
        try {val = eval( perl.responseText )}
        catch(e) {alert( [ e.name, perl.responseText, e.message ].join("\n\n") )}

        var type = typeof val;

        if (val == 0 && type != 'string') {val = '0'}
        else if (val == null) {val = 'null'}

        if (type == 'object') {
            if (val.hasAttribute && val.hasAttribute('id'))
                {val = 'OBJ ' + val.getAttribute('id')}
            else {
                ID[ 'xul_js_' + id ] = val;
                val = 'OBJ xul_js_' + id++;
            }
        }
        else {val = 'RES ' + val}
        send('res', val)
    }
}
*/
var xul_id = function (obj) {
    do {
        if (obj.id) return obj.id
        obj = obj.parentNode
    } while (obj.parentNode && obj.parentNode != obj);
}.cache(true);

function EVT (event) {
    if (noEvents.__count__ > 0 && xul_id(event.target) in noEvents) return;
    if (mutex){
        if(cacheEvents && event)
            queue.push(event);
        return;
    }
    mutex = true;
    do {
        if (event) {
            if (event.type == 'perl') {
                send('perl', event.code)
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
    if(event) setTimeout(pinger, 10);
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
    window.close();
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
var Perl = perl;

function dialog (target, code) {
    return target.install(code)
}

function install (code){
    return eval(code)
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

currently, it is not possible to open more than one window, or to use
any features available to privileged chrome apps. in most cases you
can get away with doing what you need in perl, but having proper file
dialogs and drag/drop would be nice, so this is near the top of my
todo list.

the code that attempts to find firefox may not work in all cases, patches welcome

for the TextBox object, the behaviors of the "value" and "_value" methods are reversed.
it works better that way and is more consistent with the behavior of other tags.

=head1 AUTHOR

Eric Strom, C<< <ejstrom at gmail.com> >>

=head1 BUGS

please report any bugs or feature requests to C<bug-xul-gui at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=XUL-Gui>.
I will be notified, and then you'll automatically be notified of progress on your bug as I make changes.

=head1 ACKNOWLEDGMENTS

the mozilla development team

=head1 COPYRIGHT & LICENSE

copyright 2009 Eric Strom.

this program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

see http://dev.perl.org/licenses/ for more information.

=cut

1; # End of XUL::Gui
