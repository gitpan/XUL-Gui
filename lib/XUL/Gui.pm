# XUL::Gui.pm - render cross platform gui applications with firefox from perl
#
# copyright (c) 2009 eric strom <ejstrom@gmail.com>. all rights reserved.
# this program is free software; you can redistribute it and/or modify it under the same terms as perl itself.

package XUL::Gui;
	use base 'Exporter';
	use strict;
	use warnings;
	use Carp;
	use Storable qw/dclone/;

	our $VERSION = 0.11;
	our $DEBUG = 0;
	our @Xul = map {$_, (ucfirst lc) x /.[A-Z]/}
		qw/Action ArrowScrollBox Assign BBox Binding Bindings Box Broadcaster BroadcasterSet Browser Button Caption
		CheckBox ColorPicker Column Columns Command CommandSet Conditions Content DatePicker Deck Description Dialog
		DialogHeader DropMarker Editor Grid Grippy GroupBox HBox IFrame Image Key KeySet Label ListBox ListCell ListCol
		ListCols ListHead ListHeader ListItem Member Menu MenuBar MenuItem MenuList MenuPopup MenuSeparator Notification
		NotificationBox Observes Overlay Page Panel Param PopupSet PrefPane PrefWindow Preference Preferences ProgressMeter
		Query QuerySet Radio RadioGroup Resizer RichListBox RichListItem Row Rows Rule Scale Script ScrollBar ScrollBox
		ScrollCorner Separator Spacer SpinButtons Splitter Stack StatusBar StatusBarPanel StringBundle StringBundleSet Tab
		TabBox TabPanel TabPanels Tabs Template TextBox TextNode TimePicker TitleBar ToolBar ToolBarButton ToolBarGrippy
		ToolBarItem ToolBarPalette ToolBarSeparator ToolBarSet ToolBarSpacer ToolBarSpring ToolBox ToolTip Tree TreeCell
		TreeChildren TreeCol TreeCols TreeItem TreeRow TreeSeparator Triple VBox Where Window Wizard WizardPage/;

	our %HTML = map {("_$_" => "html:$_", uc $_ => "html:$_")}
		qw/a abbr acronym address applet area audio b base basefont bdo bgsound big blink blockquote body br button canvas
		caption center cite code col colgroup comment dd del dfn dir div dl dt em embed fieldset font form frame frameset
		h1 h2 h3 h4 h5 h6 head hr html i iframe ilayer img input ins isindex kbd label layer legend li link listing map
		marquee menu meta multicol nobr noembed noframes nolayer noscript object ol optgroup option p param plaintext pre
		q rb rbc rp rt rtc ruby s samp script select small source spacer span strike strong style sub sup table tbody td
		textarea tfoot th thead title tr tt u ul var video wbr xml xmp/;

	sub FLEX {flex => 1, @_}
	sub FILL {align=>'stretch', FLEX @_}
	sub FIT  {sizeToContent => 1, @_}

	our @EXPORT = (keys %HTML, @Xul, qw/C A M W widget extends server Code %ID quit buffered alert now cached noevents
		dialog zip attribute hashif gui tag object delay run function XUL FLEX FIT FILL genid doevents trace mapn apply
		toggle lf start/);

	our %defaults = (
		window		=> ['xmlns:html' => 'http://www.w3.org/1999/xhtml',
						 xmlns       => 'http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul' ],
		scrollbox 	=> [ scrollTo    => sub{ my ($self, $x, $y) = @_; gui("scrollTo(ID.$$self{ID}, $x, $y);") } ],
	);

	# utility functions
	sub mapn (&$@) {
		my ($c, $by, @r) = splice @_, 0, 2;
		while (@_) {
			local *_ = \$_[0];
			push @r, $c->( splice @_, 0, $by )
		}
		@r
	}

	sub zip {
		map {my $i = $_;
			map {$$_[$i]} @_
		} 0 .. $#{$_[0]}
	}

	sub apply (&@) {
		my ($c, @r) = @_;
		$c->() for @r;
		wantarray ? @r : pop @r
	}

	sub toggle {
		no warnings;
		$_[0] = $_[ 1 + ($_[0] eq $_[1] or $_[0] ne $_[2]) ]
	}

	sub lf {(shift) ? @_ : ()}

	sub hashif ($\%) { my ($test, $hash) = @_;
		exists $$hash{$test} ? ($test => $$hash{$test}) : ()
	}

	sub attribute ($) {
		no strict 'refs';
		my ($key, $A) = (shift, (caller).'::A');
		map {exists $$A{$_} ? ($_ => $$A{$_}) : ()}
			ref $key eq 'ARRAY' ? @$key : $key
	}


	#  package functions
	our $server = XUL::Gui::Server->init;
	our (%ID, %_ID);
	our %dialogs;

	{
		my $id = 0;
		sub genid () {'xul_' . $id++}
	}

	sub isa {
		UNIVERSAL::isa @_ > 1 ? shift : $_, @_
	}

	sub parse {
		my (@C, %A, %M);
		while (local $_ = shift) {
			if (isa 'XUL::Gui::Object') {push @C, $_}
			elsif (/^on/ || ref $_[0] ne 'CODE') {
				if (/^style$/) {$A{style} .= (shift) . ';'}
				else		   {$A{$_}     =  shift}
			}
			else {$M{$_} = shift}
		}
		C => \@C, A => \%A, M => \%M
	}

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

	sub tag {
		my @arg = @_;
		sub {
			unshift @_, 'TEXT' if @_ == 1 and not isa $_[0] => 'XUL::Gui::Object';
			object @arg, @_
		}
	}
	{no strict 'refs';
		*{$_} = tag $_        for @Xul;
		*{$_} = tag $HTML{$_} for keys %HTML;
	}

	sub widget (&%) {
		no strict 'refs';
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
			$_ID{$wid}{WIDGET} = [ &$code ];   # NOT FINAL

			for my $k (keys %data)
				{ $$M{$k} = sub : lvalue {$data{$k}} }
			$_ID{$wid}{M} = { %methods, %$M };

			for my $i (keys %ID) {
				$_ID{$wid}{$i} = $_ID{ my $gid = genid } = $ID{$i};
				next unless isa $ID{$i} => 'XUL::Gui::Object';
				$ID{$i}{N}    = $ID{$i}{A}{id};
				$ID{$i}{ID}   = $ID{$i}{A}{id} = $gid;
				$ID{$i}{W}    = $_ID{$wid};
				$ID{$i}{$_} ||= $ID{$_} for keys %ID;
				$ID{$i}{$_} ||= $$A{$_} for keys %$A;
			}
			wantarray ? @{ $_ID{$wid}{WIDGET} } : $_ID{$wid}{WIDGET}[0];
		}
	}

	sub extends {
		no strict 'refs';
		croak 'extends only works inside widgets' unless defined %_ID;
		$ID{$_} = $_[0]{W}{$_} for grep {/[a-z]/} keys %{ $_[0]{W} };
		%{ (caller).'::M' } = %{ $_[0]{W}{M} };
		@_
	}

	sub Code ($) {
		my $c = object;
		$$c{CODE}   = shift;
		$$c{M}{run} = sub {gui( shift->{CODE} )};
		$c
	}

	sub run { &gui }

	sub function ($) {
		(my $js = shift) =~ s[\$?W{\s*(\w+)\s*}]
							 [ID.\$_->{W}{$1}{ID}]g;
		bless [ sub {
			my $id = shift;
			my $func = 'ID.' . genid;
			delay( sub{
				local *_ = \$ID{$id};
				gui( "$func = function(event){ (function(){ ". eval(qq/"$js"/) ." }).call( ID.$id )  }" );
			});
			"$func(event)";
		} ] => 'XUL::Gui::FUNCTION'
	}

	{my %xul; @xul{map {lc} @Xul} = @Xul;
	sub XUL {
		for ("@_") {
			s {<(\w+)(.+?)}		  "$xul{$1}($2"g;
			s {/>}				  '),'g;
			s {</\w+>}			  '),'g;
			s {>}				  ''g;
			s {(\w+)\s*=\s*(\S+)} "'$1'=>$2"g;
			s <([^\\](}|"|'))\s+> "$1,"g;
			return eval 'package '.caller().";$_"
				or croak "XUL Parse Failure: $@\n\n$_";
		}
	}}

	sub alert {
		gui( "alert('\Q@_\E');" );
		@_
	}

	sub server {
		$server->start( &parse )
	}

	sub start {$server->start( &parse )}

	sub dialog { carp 'dialog not implemented yet' }

	sub quit {
		gui('quit();');
		$$server{run} = 0;
	}

	sub trace {
		my $caller = caller;
		carp 'trace: ', join ', ' => map {
			(isa 'XUL::Gui::Object') ? lookup($_, $caller) : $_
		} @_;
		wantarray ? @_ : pop
	}

	{my %cache;
	sub lookup {
		no strict 'refs';
		my $self = shift;
		return $cache{$self} if $cache{$self};
		return $$self{ID} unless $$self{W} || $$self{T};
		no warnings;
		our %space;
		local *space = \%{"$_[0]\::"};
		for (keys %space) { eval {
			*{$space{$_}}{CODE} == ($$self{T} || $$self{W}{T})
				and return $cache{$self} = $_ .
					($$self{T}
						? '{'
						: "{".($$self{W}{A}{id} || $$self{W}{ID}).'}->{'
					).($$self{N} || $$self{ID}).'}'
		}}
		$$self{ID}
	}}

	sub calltrace {
		my $self = $_[0];
		my $caller = caller 1;
		our $AUTOLOAD =~ /([^:]+)$/;
		lookup($self, $caller) . "->$1(".
			(join ',' => map {
				(isa 'XUL::Gui::Object') ? lookup($_, $caller) : $_
			} @_[1..$#_]). ")\n";
	}


	{my ($buffered, @buffer, $cached, %cache, $now);
		sub gui : lvalue {
			push @_, "\n";
			unless ($now) {
				push @buffer, @_ and return if $buffered;
				return $cache{$_[0]} if exists $cache{$_[0]}
			}
			$server->write('text/plain', "@_");
			my $res = $server->read->{CONTENT};
			croak "Invalid Response: $res" unless $res =~ /^(...) (.*)/;

			$res = $1 eq 'OBJ' ?
				($ID{$2} || object undef, id=>$2) : $2;
			$cache{$_[0]} = $res if $cached and $_[0] =~ /^(GET|0)/;
			$res
		}
		sub buffered (&@) {
			$buffered++;
			&{+shift};
			gui(@buffer),
				@buffer = () unless --$buffered;
		}
		sub cashed (&) {
			$cached++;
			my $ret = shift->();
			%cache = () unless --$cached;
			$ret;
		}
		sub now (&) {
			$now++;
			my @ret = shift->();
			$now--;
			wantarray ? @ret : $ret[0];
		}
	}
	sub delay (&@) {
		my $code = shift;
		my @args = @_;
		push @{$$server{queue}}, sub{ $code->( @args ) };
		return;
	}
	sub noevents (&@) {
		gui('cacheEvents = false;');
		my @ret = &{+shift};
		gui('cacheEvents = true;');
		@ret;
	}
	sub doevents () {
		$server->write('text/plain', 'NOOP');
		$server->read;
		return;
	}



package XUL::Gui::Object;
	use warnings;
	use strict;
	my $search;
	$search = sub {
		my $self = shift;
		my $method = shift;
		if (exists $$self{M}{$method})
		   {return $$self{M}{$method}}
		for (@{$$self{C}})
			{defined and return $_ for $search->($_, $method)}
	};

	sub AUTOLOAD : lvalue {
		my $self = $_[0];
		return unless ($self->{AL}) = our $AUTOLOAD =~ /([^:]+)$/;

		if (my $method = $search->($self, $$self{AL})) { # perl method call
			if (ref $method eq 'ARRAY') {
				splice @_, 0, 1, $$method[0];
				$method = $$method[1];
			}
			if ($XUL::Gui::DEBUG) {
				my $caller = caller;
				print XUL::Gui::lookup($self, $caller) . "->$1(" .
					(join ',' => map {(XUL::Gui::isa XUL::Gui::Object) ? XUL::Gui::lookup($_, $caller) : $_} @_[1..$#_]). ")\n"
			}
			goto &$method
		}

		if (@_>1 or not defined wantarray) { # js method call
			shift;
			my ($js, $arg) = ('', '');
			$_->($self), return for $$self{uc $$self{AL}} or ();
			shift if @_ and $_[0] eq '_';
			$arg = join ',', map {
				XUL::Gui::isa XUL::Gui::Object and do
					{if ($_->{DIRTY}){
						($$_{W} ? $$_{W}{W} : $$_{W}) ||= $$self{W};
						$$_{P} ||= $self;
						$js .= $_->toJS;}
					"ID.$_->{ID}"}   or   "'\Q$_\E'"
				} @_;
			return XUL::Gui::gui($js . "ID.$self->{ID}.$self->{AL}($arg);")
		}

		tie my $ret, 'XUL::Gui::Scalar', $self; # proxy
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

		if (defined $$self{CODE})
		   {return  $$self{CODE}}

		push @xul, "<$self->{TAG} ";
		push @xul, qq{$_="$self->{A}{$_}" } for keys %{$self->{A}};
		if (@{$$self{C}}) {
			push @xul, ">\n";
			push @xul, "\t" x ($tab+1),  $_->toXUL($tab+1) for @{$$self{C}};
			push @xul, "\t" x $tab, "</$self->{TAG}>\n";
		}
		else {push @xul, "/>\n"}
		join '' => @xul
	}

	sub toJS {
		my ($self, $final) = @_;
		my @js;
		my $id = "ID.$$self{ID}";

		$$self{DIRTY} = 0;
		$self->registerEvents;

		if (defined $$self{CODE})
		   {return  $$self{CODE}}

		push @js, $_->toJS for @{$$self{C}};
		push @js, qq{$id = document.createElement} .
			($$self{TAG} !~ /:/
				? qq{('$$self{TAG}');}
				: qq{NS('http://www.w3.org/1999/xhtml', '$$self{TAG}');});
		for (keys %{$$self{A}}) {
			my $val = quotemeta(
				ref $$self{A}{$_} eq 'XUL::Gui::FUNCTION'
					? $$self{A}{$_}[0]( $$self{ID} )
					: $$self{A}{$_}
			);
			if (/^TEXT$/) {
				push @js, qq{$id.appendChild( document.createTextNode('$val') );};
				next
			}
			push @js, /^_(.+)/ ?
				qq{$id\['$1'] = '$val';} :
				qq{$id.setAttribute('\L$_\E','$val');};
		}
		for (@{$$self{C}}) {
			$$_{P} = $self;
			push @js, qq{$id.appendChild(ID.$$_{ID});} if $$_{TAG};
		}
		join "\n" => @js, $final ? "$final.appendChild($id);" : ''
	}

	sub removeChildren {
		my $self = shift;
		@_	? XUL::Gui::buffered {$self->removeChild($_) for @_} @_
			: XUL::Gui::gui "removeChildren(ID.$self->{ID});";
		$self
	}

	sub removeItems {
		my $self = shift;
		@_	? XUL::Gui::buffered {$self->removeItem($_) for @_} @_
			: XUL::Gui::gui "removeItems(ID.$self->{ID});";
		$self
	}

	sub appendChildren {
		my $self = shift;
		XUL::Gui::buffered {$self->appendChild($_) for @_} @_;
		$self
	}

	sub prependChild {
		my ($self, $child, $count, $first) = @_;
		if ($$self{TAG} eq 'tabs') {
			print "tabs: ";
			$first = $self->getItemAtIndex( $count || 0 )
		} else {
			$first = $self->firstChild;
			while ($count-- > 0) {
				last if $first eq 'null';
				$first = $first->nextSibling;
			}
		}
		print "$$first{TAG}\n\n" if $first ne 'null';
		$first eq 'null'
			? $self->appendChild( $child )
			: $self->insertBefore( $child, $first );
		$self
	}

	sub appendItems {
		my $self = shift;
		XUL::Gui::buffered {
			(XUL::Gui::isa XUL::Gui::Object)
				? $self->appendChild($_)
				: $self->appendItem( ref eq 'ARRAY' ? @$_ : $_ )
			for @_
		} @_;
		$self
	}

	sub replaceItems {
		my $self = shift;
		XUL::Gui::buffered {
			XUL::Gui::noevents {
				$self->removeItems
					 ->appendItems( @_ )
			} @_
		} @_;
		$self
	}



package XUL::Gui::Scalar;
	use base 'XUL::Gui::Object';
	use warnings;
	use strict;
	use Carp;

	sub TIESCALAR {bless pop, pop}

	sub FETCH {
		my $self = shift;
		return $self->{uc $self->{AL}} if $self->{AL} =~ /^on/;
		XUL::Gui::gui $self->{AL} =~ /^_(.+)/
			? "0;ID.$self->{ID}\['$1'];"
			: "GET(ID.$self->{ID}, '$self->{AL}');"
	}

	sub STORE {
		my ($self, $new) = @_;
		if ($$self{AL} =~ /^on/) {
			not defined $new or ref $new eq 'CODE'
				or croak "assignment to event handler must be CODE ref or undef";
			$new = $new ? do {$$self{uc $$self{AL}} = $new; 'EVT(event)'} : '';
		}
		XUL::Gui::gui $self->{AL} =~ /^_(.+)/
			? "1;ID.$self->{ID}\['$1'] = '\Q$new\E';"
			: "SET(ID.$self->{ID}, '$self->{AL}', '\Q$new\E');"
	}


package XUL::Gui::Server;
	use warnings;
	use strict;
	use Carp;
	use File::Find;
	use IO::Socket;

	my $port = 8888;
	our ($req, $client_js);

	sub init {bless {}}

	sub message {print STDERR "XUL::Gui> @_\n"}

	sub start {
		my $server  = shift || bless {};
		my %args    = @_;
		my @content = @{ $args{C} };
		my %params  = %{ $args{A} };

		@$server{keys %params} = values %params;

		$XUL::Gui::DEBUG = $params{debug} || $XUL::Gui::DEBUG;

		my $port = $params{port} || $port++;
		$port++ until $$server{server} = IO::Socket::INET->new(
			Proto	  => 'tcp',
			PeerAddr  => 'localhost',
			LocalAddr => "localhost:$port",
			ReuseAddr => 1,
			Listen	  => 1,
			Reuse	  => 1,
		);

		$| = 1;
		message "server started: http://localhost:$port";

		my ($root, %dispatch);
		%dispatch = (
			'/' => sub {
				my $res = qq{<?xml version="1.0"?>\n<?xml-stylesheet }.
						  qq{href="chrome://global/skin" type="text/css"?>\n};

				$root = $content[0]->{TAG} eq 'window'
							? shift @content
							: XUL::Gui::Window();

				$root->{A}{onunload} = sub {$$server{run} = 0};
				unshift @content, @{ $root->{C} };
				$root->{C} = [ XUL::Gui::Script(src=>'/client.js') ];
				$server->write('application/vnd.mozilla.xul+xml', $res . $root->toXUL);
			},
			'/client.js' => sub {
				$server->write( 'text/javascript', qq~
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
				$id  = $root->{ID} if $id eq 'undefined'; # why doesnt onunload window send id?
				$id  = $ID{$id}{P}{ID} while $ID{$id}{P} and not $ID{$id}{$evt}; # more xul bugs
				if (ref $ID{$id}{$evt} eq 'CODE'){
					$ID{$id}{$evt} -> ( $ID{$id}, XUL::Gui::object(undef, id=>$obj) )
				} else {message "no event handler found: $req->{CONTENT}"}
				$server->write('text/plain', 'NOOP');
			},
			'/ping' => sub {
				&$_ for @{$$server{queue}};
				@{$$server{queue}} = ();
				local $XUL::Gui::DEBUG = 1;
				$server->write('text/plain', 'NOOP');
			},
			'/favicon.ico' => sub {
				$server->write('text/plain', '');
			},
			'/exit' => sub {$$server{run} = 0} #Add old code back from USB key
		);
		unless ($params{nolaunch}) {
			if ($^O =~ /darwin/) {
				system qq[osascript -e 'tell application "Firefox" to OpenURL "http://localhost:$port"']
			} else {
				my @firefox;
				map {my $dir = $_;
					 find sub{ push @firefox, [$_, $File::Find::name] if /^firefox(?:-bin|\.exe)?$/ and -f } => $_
						for grep {/mozilla|firefox/i} map {"$dir/$_"} (-d and chdir $_) ? <*> : ()
				} ($^O =~ /MSWin/)
					? (map {chomp; "$_\\"} split ',' => `echo \%ProgramFiles\%,\%ProgramFiles(x86)\%`)
					: split /[:;]/ => $ENV{PATH};

				if (@firefox = sort {length $$a[0] < length $$b[0]} @firefox) {
					my $insert  = $params{nochrome} ? '' : '-P "perl" -chrome ';
					message 'launching firefox';
					unless ($$server{pid} = fork) {
						$firefox[0][1] =~ tr./.\\. if $^O =~ /MSWin/;
						exec qq{"$firefox[0][1]" $insert "http://localhost:$port" } . (q{1>&2 2>/dev/null} x ($^O !~ /MSWin/));
					}
					alarm 5; $SIG{ALRM} = sub {message "run 'firefox -P' and create a 'perl' profile"; exit};
				}
				else {message 'firefox not found: start manually'}
			}
		}

		$$server{run} = 1;
		run: while ($$server{client} = $$server{server}->accept) {
				alarm 0; $SIG{ALRM} = 'IGNORE';
				$$server{client}->autoflush(1);
				message 'client connected';
				while (local $req = $server->read) {
					if ($dispatch{$$req{URL}}) {
						$dispatch{$$req{URL}}->()
					} elsif (open FILE, '.'.$$req{URL}) {
						$server->write('text/plain', <FILE>)
					} else {
						message "file: $$req{URL} not found";
						$server->write('text/plain', '')
					}
					last unless $$server{run}
				}
				close $$server{client};
				last run unless $$server{run}
			}
		$server->stop('server stopped')
	}

	sub stop {
		message @_[1..$#_];
		local $SIG{HUP} = 'IGNORE';
		kill 1, -($^O =~ /MSWin/ ? shift->{pid} : $$);
	}

	sub read {
		my ($server) = @_;
		my $client = $$server{client};
		my %req;
		reset;
		local $_;
		local $/ = "\015\012";
		while (<$client>) {
			chomp;
			if    (?^\s*\w+\s*([^\s]+)\s*HTTP?) {$req{ URL } = $1}
			elsif (/^\s*(.+?)\s*:\s*(.+?)\s*$/) {$req{lc $1} = $2}
			elsif (not $_) {
				CORE::read $$server{client} => $req{CONTENT}, $req{'content-length'} || 0;
				last;
			}
		}
		unless ($req{URL}) {
			$server->write('text/javascript', 'alert("error: broken message received")');
			return $server->read;
		}

		if ($req{URL} =~ /(.+)\?(.+)/)
			{@req{'URL','CONTENT'} = ($1, "OBJ $2")}

		message "read @req{qw/URL CONTENT/}" if $XUL::Gui::DEBUG > 2 and $req{URL} ne '/ping';
		if ($req{URL} eq '/perl') {
			$server->write( 'text/plain', 'RETURN '. eval $req{CONTENT} );
			%req = %{ $server->read };
		}

		\%req
	}

	sub write {
		my $server = shift;
		message "write @_" if $XUL::Gui::DEBUG > 2;
		print {$$server{client}} join "\015\012" =>
			'HTTP/1.1 200 OK',
			'Expires: -1',
			'Keep-Alive: 300',
			'Connection: Keep-Alive',
			'Content-type: ' . shift,
			'Content-length: ' . length,
			'',
			$_ for "@_[1..$#_]";
	}


	$client_js = <<'END';

Object.prototype.__defineGetter__('__sid__', function(){
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

Object.prototype.toString = function(){
	return '[Object ' + this.__sid__ + ']';
};

Function.prototype.cache = function(){
	var cid = 0;
	return function(strict, env){
		var self  = this;
		var cache = {};
		env = env || null;
		var args;

		var getid = function(obj){
			if (obj.__cid__) return obj.__cid__;
			var id = cid++;
			obj.__proto__ = {
				__proto__: obj.__proto__,
				get __cid__(){ return id }
			}
			return id;
		};

		var cached = function(){
			args = Array.prototype.slice.call(arguments);
			if (strict) args = args.map( function(arg) typeof arg == 'object' ?
											[arg.toSource(), getid(arg)] : arg );
			args = args.toSource();

		//	document.writeln(args);
			return (args in cache)
				? cache[args]
				: cache[args] = self.apply(env, arguments);
		};

		cached.nocache = function(){ return self };
		return cached;
	}
}.call();

Function.prototype.extends = Object.prototype.extends = function(super){
   this.prototype.__proto__ = super.prototype;
   this.prototype.__super = super;
}



Object.prototype.__defineGetter__('keys', function(){
	var out = [];
	for (var i in this) {
		if (this.hasOwnProperty(i)) out.push(i);
	}
	return out;
});

Object.prototype.__defineGetter__('values', function(){
	return this.keys.map( function (k) this[k], this )
});

var id = 0;
var ID = {};
var noEvents = {};
var cacheEvents = true;
var ping = 250;
var interval = setInterval(pinger, ping);
var server = 'http://localhost:'+port+'/';

var perl = new XMLHttpRequest();

function pinger() {
	if(mutex || !cacheEvents) return;
	mutex = true;
	send('ping', null)
	mutex = false;
}

var retre = new RegExp(/^RETURN (.*)/);
function send(to, val){
	var url = server + to;
	var type;
	while (1) {
		perl.open( 'POST', url, false );
		perl.send( val );
		if (perl.responseText != 'NOOP') {
			var ret = perl.responseText.match(retre);
			if (ret) return ret[1];
			try {
				val = eval( perl.responseText );
			} catch(e) {
				alert( [ e.name, perl.responseText, e.message ].join("\n\n") );
			}

			type = typeof val;

			if (val == 0 && type != 'string') {
				val = '0'
			} else if (val == null) {
				val = 'null'
			}

			if (type == 'object') {
				if (val.hasAttribute && val.hasAttribute('id'))
					val = 'OBJ ' + val.getAttribute('id');
				else {
					ID[ 'xul_js_' + id ] = val;
					val = 'OBJ xul_js_' + id++;
				}
			} else {
				val = 'RES ' + val
			}
		} else {
			break
		}
	}
	return val;
}

var queue = [];
var mutex = false;


var xul_id = function( obj ){
	while (obj.parentNode && obj.parentNode != obj) {
		if(obj.id) return obj.id
		obj = obj.parentNode
	}
}.cache(true);

function EVT(event){
	if (noEvents.__count__ > 0 && xul_id(event.target) in noEvents) return;
	if (mutex){
		if(cacheEvents && event)
			queue.push(event);
		return;
	}
	mutex = true;
	do {
		if (event) {
			ID['xul_js_' + id] = event;
			send('event', "EVT " + xul_id(event.target) + " " + event.type + " " + ('xul_js_' + id++));
		} else {
			send('ping', null)
		}
	} while (event = queue.shift());
	mutex = false;
	if(event) setTimeout(pinger, 10);
}

function GET(self, k){
	if (typeof self.hasAttribute == 'function' && self.hasAttribute(k))
		return self.getAttribute(k);

	if (typeof self[k] == 'function')
		return self[k]();

	return self[k];
}

function SET(self, k, v){
	if (typeof self.hasAttribute == 'function'
			&& self.hasAttribute(k) )
		return self.setAttribute(k, v);

	return self[k] = v;
}

function quit(){
	clearInterval(interval);
	EVT = function(){}
	window.close();
}

function removeChildren(element){
	while (element.firstChild)
		element.removeChild(element.firstChild);
}

function removeItems(element){
	while (element.lastChild && element.lastChild.nodeName == 'listitem')
		element.removeChild(element.lastChild);
}

function scrollTo(element, x, y){
	element.boxObject.QueryInterface(
		Components.interfaces.nsIScrollBoxObject
	).scrollTo(x, y);
}

function Perl(code){
	return send('perl', code)
}

function dialog(target, code) {
	return target.install(code)
}

function install(code){
	return eval(code)
}

Element.prototype.computed = function( style ){
	return document.defaultView.getComputedStyle( this, null ).getPropertyValue( style )
}

Element.prototype.noEvents = function( value ){
	return value
		? noEvents[this] = true
		: delete noEvents[this]
}

END

package XUL::Gui;
=head1 NAME

XUL::Gui - render cross platform gui applications with firefox from perl

=head1 SYNOPSIS

use XUL::Gui;
start Label 'hello, world!';


use XUL::Gui;
start Window title => "XUL::Gui's long hello",
    GroupBox(
        Caption('XUL'),
        Button( label=>'click me', oncommand=> sub {shift->label = 'ouch'} ),
        Button( type=>'menu', label=>'menu button',
            MenuPopup map {MenuItem label=>$_} qw/first second third/
        ),
        TextBox( FILL ),
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

this module is presented for preview only and is under active development.
this code is currently in beta, use in production enviroments at your own risk

the code will be considered production ready, and interfaces finalized at version 0.25

all XUL and HTML objects in perl are exact mirrors of their javascript counterparts and can
be acted on as such.  developer.mozilla.com is the official source of documentation.

this documentation is very incomplete.  an updated module will hopefully be submitted in 1 - 2 weeks.

=head2 Functions

=over 8

=item C<start>

starts the XUL::Gui server, and launches firefox.  C<start> accepts a Window() object, or any
other XUL or HTML tag.  if a Window is not given, one will be created with default settings.

=item Tags

all of the current XUL and HTML tags are imported into the caller's namespace by default.
XUL tags start with a capital letter, and then have optionally capitalized subsequent words:
	ProgressBar == Progressbar
HTML tags are imported in all caps (H1 P DIV SPAN TABLE IMG....)

=item other functions

widget extends server Code quit buffered alert now cached noevents
dialog zip attribute hashif gui tag object delay run function XUL
FLEX FIT FILL genid doevents trace mapn apply toggle lf start

documentation is currently incomplete.
please see the source code for current functions

=back

=head1 LICENSE

this program is free software;
you can redistribute it and/or modify it under the same terms as perl itself.

=head1 AUTHOR

copyright (c) 2009 eric strom <ejstrom@gmail.com>. all rights reserved.

=head1 SEE ALSO

=cut
1;
