#!/usr/bin/perl -w
# die "AAAAAA";
#
# Perl package exporting a function "conjug" that conjugates
# Portuguese verbs. 
# 
# Author : Etienne Grossmann (etienne@isr.ist.utl.pt) 
# 
# Date   : May 1997 onwards.
# 
# Changes : 
#  6/30/97 - Verbos Abundantes.
#  7/01/97 - Verbos Defectivos.
# 12/27/97 - Iso 8859 Accents.
#    01 98 - Renaming of conj.pm as Lingua/PT/Conjugate.pm and
#            likewise for other files.
#          - Make iso-accents the default, use them in verb database
#            and source files.
#          - Added double-past-participles that I had forgotten about.
#          - Verb database as a string, is at the end of this file. 
#          - put use //o  whenever possible, as suggested by Eryq
#            <eryq@zeegee.com> 
#    02 98 - Recognize long forms of verbs
#          - Derivatives of "ter" (ugly fix)
#    03 98 - A few fixes, cleaned up code.
#    05 98 - A few more "defective" verbs.
# 
$VERSION=0.04;

# Just to make sure which file is loaded
# BEGIN{ print "SEE THIS ???\n",`pwd` }

package Lingua::PT::Conjugate;
use Lingua::PT::Accent_iso_8859_1 qw(iso2asc asc2iso un_accent);
use Exporter ;
@ISA = qw(Exporter);
# Yes, this package is a namespace polluter. 
@EXPORT = qw(conjug); 

@EXPORT_OK = qw( cedilla codify diffstr end_gu end_oiar end_uir
				 end_zer hard_c hard_g list_verbs locate same_model
				 soft_c soft_g tabcol tabrow verbify verify @tense
				 %tense %alt_tense %long_tense %endg %reg %verb
				 @regverb $vpat $cpat $wpat $vlist $letter );

# ##################### THE NAMES OF THE TENSES ##########################
# Various alternative ways of specifying tenses
# No accentuated characters
%alt_tense= ("presente" =>"pres",
			 "perfeito" =>"perf", 
			 "imperfeito"  =>"imp",
			 "futuro"  =>"fut",  
			 "mais-que-perfeito"=>"mdp",
			 "mais que perfeito"=>"mdp",
			 "mais"  =>{"que"=>{"perfeito"=>"mdp"}},  
			 "conjuntivo"=>{"presente"=>"cpres",
							"imperfeito"=>"cimp",
							"futuro"=>"cfut",
							"pres"=>"cpres",
							"imp"=>"cimp",
							"fut"=>"cfut"},
			 "conjuntivo presente"=>"cpres",
			 "conjuntivo imperfeito"=>"cimp",
			 "conjuntivo futuro"=>"cfut",
			 "condicional" =>"cond",
			 "imperativo"  =>"ivo",
			 "participio"=>{"passado"=>"pp"}, #'
			 "participio passado"=>"pp", #'
			 "gerundivo"  =>"grd" ,
			 "pres"=>"pres",
			 "perf"=> "perf",
			 "imp"=>"imp", 
			 "fut"=>"fut", 
			 "mdp"=>"mdp",  
			 "cpres"=>"cpres", 
			 "cimp"=>"cimp", 
			 "cfut"=>"cfut", 
			 "cond"=>"cond", 
			 "ivo"=>"ivo", 
			 "pp"=>"pp", 
			 "grd"=>"grd",
			);

# Full tense names
%long_tense= ("pres" =>"presente",
			  "perf" =>"perfeito", 
			  "imp"  =>"imperfeito",
			  "fut"  =>"futuro",  
			  "mdp"=>"mais-que-perfeito",
			  "cpres"=>"conjuntivo presente",
			  "cimp"=>"conjuntivo imperfeito",
			  "cfut"=>"conjuntivo futuro",
			  "cond" =>"condicional",
			  "ivo"  =>"imperativo",
			  "pp"=>"particípio passado", #'
			  "grd"  =>"gerundivo" ,
			 );


# WARNING : $tense[9,] eq "ivo" is assumed in verbify() below.
# WARNING : $tense[10,11] assumed to be partic'ipiopassado and
# gerundivo in verbify() below. 




# Tenses
# # DONT PUT IT IN BEGIN{
@tense =qw{ pres perf imp fut mdp  cpres cimp cfut cond ivo pp grd };
%tense = qw{ pres 1 perf 2 imp 3 fut 4 mdp 5 cpres 6 cimp 7 cfut 8
			 cond 9 ivo 10 pp 11 grd 12 };

%empty = ("pres",[],"perf",[],"imp",[],"fut",[],"mdp",[],
		  "cpres",[],"cimp",[],"cfut",[],"cond",[],"ivo",[],
		  "pp",[],"grd",[]);

# ####################### VOCALS, CONSONANTS ##################### 
# Vocals and Consonants   
$vocs = "aeiouáàäâãéèëêíìïîóòöôõúùüû";
$plainvoc = "aeiou";
$accvoc = "áàäâãéèëêíìïîóòöôõúùüû";
$only_acc = 
  {split("","á\'à\`ä\"â^ã~é\'è\`ë\"ê^í\'ì\`ï\"î^ó\'ò\`ö\"ô^õ~ú\'ù\`ü\"û^")};
$no_acc = 
  {split("","áaàaäaâaãaéeèeëeêeíiìiïiîióoòoöoôoõoúuùuüuûu")};

$vpat = "[$vocs]";
$cons = 'qwrtypsdfghjklzxcvbnm';
$cpat = "(?:[$cons]+|ç|gu)";
$wpat = "[ç$vocs$cons]";
$letter = "ç$vocs$cons";

# pres perf imp fut mdp  cpres cimp cfut cond ivo pp grd 


# ##############  REGULAR EXPRESSIONS THAT MATCH VERB ENDINGS ############
%endg = %{verbify( q"
      o   [aeiín]s  [aeim] [eaio]mos [ae]?is [ae]m,
      e?[íis]   [aei]ste  [eio][us] [aei]mos [aei]stes [aei]ram,
      (?:av|i)?a   (?:av|i)?as   (?:av|i)?a (?:av|áv|í|i)?[aá]mos
      (?:av|áv|í|i)?[aá]eis (?:av|i)?am, 
      [aeio]rei [aeio]r[aá]s [aeio]r[aáâ] [aeio]r[ae]mos [aeio]reis
      [aeio]rão,
      [aei]ra [aei]ras [aei]ra [aeiâáêéîí]ramos [aeiaeiâáêéîí]reis [aei]ram,
      [aeo] [ae]s [ae] [ae]mos [aei]s [ae]m,
      [ae]sse [ae]sses [ae]sse [aeâáêé]ssemos [aeiâáêéîí]sseis [ae]ssem,
      [aei]r [aei]res [aei]r [aei]rmos [aei]rdes [aei]rem, 
      [aeio]ria [aeio]rias [aeio]ria [aeio]r[iíî]amos
      [aeio]r[aeioâáêéîíóòô]eis [aeio]riam, 
      [aeim] [ae] [ae]mos [ae]m ,
      (?:[aií]do|to) , [aeio]ndo " 
				 )};

# print join(",",%endg);
# exit;

# #################### REGULAR VERBS ENDINGS ####################
%reg = ( 
		"er" => verbify( q{
		  o   es     e emos eis em,
		  i   este  eu emos estes eram,
		  ia   ias   ia íamos íeis iam,
		  erei erás erá eremos ereis erão,
		  era eras era êramos eram,
		  a as a amos am,
		  esse esses esse êssemos essem,
		  er eres er ermos erem, 
		  eria erias eria eríamos eriam,
		  e a amos am ,
		  ido , endo ,
		}) ,
		
		"ar" => verbify( q{
		  o   as     a  amos  ais     am , 
		  ei   aste  ou  amos  astes  aram ,
		  ava   avas  ava  ávamos áveis avam ,
		  arei arás ará aremos  areis  arão,
		  ara aras ara áramos aram ,
		  e es e emos em ,
		  asse  asses asse ássemos assem,
		  ar ares ar armos arem,
		  aria arias aria aríamos ariam,
		  a e emos em ,
		  ado , ando ,
		} ),
		
		"ir" => verbify( q{
		  o   es     e  imos   is    em , 
		  i   iste  iu  imos   istes  iram ,
		  ia   ias   ia  íamos íieis iam ,
		  irei irás irá iremos ireis  irão,
		  ira iras ira íramos iram,
		  a as a amos am,
		  isse isses isse íssemos issem,
		  ir ires ir irmos irem,
		  iria irias iria iríamos iriam,
		  e a amos am ,
		  ido , indo ,
		} ),
		
		"or" => verbify(q{ 
		  onho ões õe omos õem ,
		  us useste ôs usemos useram , 
		  unha unhas unha únhamos unham,
		  orei orás orá oremos orão,
		  usera useras usera uséramos useram,
		  onha onhas onha onhamos onham,
		  usesse usesses usesse uséssemos usessem,
		  user useres user usermos userem,
		  oria orias oria oríamos oriam,
		  pp osto grd ondo
		}),
	   );

# ################# AUXILIARY OR COMMON VERBS ################## 
%verb = (
		 "ter"=>verbify( q{ 
		   tenho tens tem temos tendes têm ,
		   tive tiveste teve tivemos tivestes tiveram,
		   tinha tinhas tinha tínhamos tínheis tinham,
		   terei terás terá teremos tereis terão,
		   tivera tiveras tivera tivéramos tiveram,
		   tenha tenhas tenha tenhamos tenham,
		   tivesse tivesses tivéssemos tivessem,
		   tiver tiveres tiver tivermos tiverem,
		   cond teria terias teria teríamos teriam,
		   ivo tem tenha tenhamos tenham ,
		   tido tendo 
		 } ),
		 
		 "ser"=>verbify( q{
		   sou és é somos são, 
		   fui foste foi fomos fostes foram,
		   era eras era éramos éreis eram,
		   serei serás será seremos serão ,
		   fora foras fora fôramos foram ,
		   seja sejas seja sejamos sejam,
		   fosse fosses fosse fôssemos fossem,
		   for fores for formos forem,
		   seria serias seria seríamos seriam,
		   sê seja sejamos sejam,
		   sido sendo
		 } ),
		 
		 "estar"=>verbify( q{
		   estou estás está estamos estão,
		   estive estiveste esteve estivemos estiveram,
		   estava estavas estava estávamos estavam,
		   estarei estarás estará estaremos estarão,
		   estivera estiveras estivera estivéramos estiverãm,
		   esteja estejas esteja estejamos estejam,
		   estivesse estivesses estivesse estivéssemos estivessem,
		   estiver estiveres estiver estivermos estiverem,
		   estaria estarias estaríamos estariam,
		   está estéja estejamos estejam,
		   estado estando
		 } ),
		 
		 "haver"=>verbify( q{
		   hei hás há havemos haveis hão,
		   houve houveste houve houvemos houveram,
		   havia havias havia havíamos haviam,
		   haverei haverás haverá haveremos haverão,
		   houvera houveras houvera houveramos houveram,
		   haja hajas haja hajamos hajam,
		   houvesse houvesses houvesse houvéssemos houvessem,
		   houver houveres houver houvermos houverem,
		   haveria haverias haveria haveríamos haveriam,
		   hajas haja hajamos hajam, havido  havendo
		 } ),
		 
		 
		 "pôr"=>verbify( q{ pôr
							  ponho pões põe pomos põem ,
							  pus puseste pôs pusemos puseram , 
							  punha punhas punha púnhamos punham,
							  porei porás porá poremos porão,
							  pusera puseras pusera puséramos puseram,
							  ponha ponhas ponha ponhamos ponham,
							  cimp pusesse pusesses pusesse puséssemos pusessem,
							  puser puseres puser pusermos puserem,
							  poria porias poria poríamos poriam,
							  pp posto grd pondo
							}),
		 
		);

#  A few regular verbs
@regverb = qw{ receitar viver andar partir fintar fracturar guiar
			   habituar garantir iludir imitir infundir inquirir
			   insistir infringir infligir impingir insurgir
			   intermitir irromper };

# ############### INITIALIZE THE DATABASE STRING OF VERBS ##############

# Why use DATA filehandles that are 'closed' when one needs them?
goto ALL_THE_WAY;
# $Lingua::PT::vlist is  initialised 
I_M_BACK:  


$vlist =~ s/\#.*\n+/\n/mg;
$vlist =~ s/\n/ /mg;

########################## CODE, at last ############################

# Specify that $_[0] is the model of conjugation for @_[1,$#_].
# Usage : 
# same_model('model verb1 verb2 ...') 
# same_model('model','verb1','verb2') 
sub same_model {
  
  my $verb = ( ref($_[0]) eq "HASH") ? shift  : \%verb ;
  @_ = map {split /\s+/ }   @_ ;
  my $m = shift;
  # print "Same model $m ",join(",",@_),"\n";
  foreach (@_) { $verb->{$_}->{model} = $m   }
}

# Convert a single verb entry in verb.pt into a %verb hash.
sub verbify {
  
  my ($a,$t,$tc,$p,%res,$x,$y,$root,$edg,@accent);
  $tc = 0;
  %res = ();					# %empty;
  
  $a = $_[0];
  
  $a =~ s/,/ , /g; 
  $a =~ s/^\s+//;
  $a =~ s/\s+$//;
  @_ = split(/\s+/,$a);
  
  # print "verbify >$a<\n";
  
  $t = $tense[$tc=0];
  $p = 0;
  
  if( $_[0] =~ /([aeioô]r)$/ ){ 
	
    # Extract Root and Ending
    $edg  = $1;
    $root = shift; 
    $root =~ s/..$//;
	# print "verbifying >> $root , $edg <<\n";
  } 
  # print "Verbifying $_[0]\n";
  while($_ = shift) {
	
    s/^\s*//;
    chomp($_);
	
	# print "???? $_ \n" if $root eq "faz" && $t eq "";
    if($_ eq "model"){
      warn "Model not found in verbify" unless $_ = shift ;
      s/^\s*//;
      chomp($_);
	  
      $res{model} = $_ ;
      next;
    }
	
	
    if(defined($tense{$_}) || ("$_" eq ",") || $p==6 ){ 
	  
	  # All persons passed
      $p6 = (! defined($tense{$_}) && ("$_" ne ","))? 1 : 0;
	  
      if($p==5){				# If no 2nd person plural was found
        $res{$t}->[5] = $res{$t}->[4] ;
      }
	  # Ready for next tense
      $p = 0;
      if(defined($tense{$t=$_})){
        for( $tc=0 ; "$tense[$tc]" ne "$t" ; $tc++ ){};
		# print "Tense $t\n";
        
      } else {
        $tc++;
        $t = $tense[$tc];
      }
      
	  # if($_ eq "cimp"){
	  # print "Starting cimp for $root,$edg\n";
	  # }
      next unless $p6;
	  
	  # HERE CAREFUL if @tense changes
    }  elsif( ($tc==10) && ($p==1)  ){
	  
      $p = 0;
      $tc++ ;
      $t = $tense[$tc];
	  
    } elsif( ($tc==9) && ($p==0) ){
	  
      # Safer, but slower  
      # if( ($tense{$tc} eq "ivo" ) && ($p==0) );
	  
      $p++ ;
	  
	  # Build default, if possible
    } elsif( $_ eq "etc" && $edg && $p && ($x=$res{$t}->[$p-1])   ){
	  
      # If last input matches a regular model, adopt that model
      $edg2 = $edg;
      my $e;
      if( $x !~ / $reg{$edg}->{$t}->[$p-1] $/x ) {
        foreach $e ("ir","ar","er") {
          if( $x =~ / $reg{$e}->{$t}->[$p-1] $/x ){
            $edg2=$e; last;
          } 
        }
      }
      $x =~ s/ $reg{$edg2}->{$t}->[$p-1] $//x;
      $x =~ s/ [e]+ $//x;
	  
      while( $p < 6 ){
        $res{$t}->[$p] = $x . $reg{$edg2}->{$t}->[$p] unless 
          $p==3 && $reg{$edg2}->{$t}->[$p] =~ /^i/ && 
            $x =~ /i([$cons]{1,2}|ç|gu)$/o   ;
		# print "$t , $p , $res{$t}->[$p] <<\n";
        $p++;
      }
	  
      $p = 5  ;
      $_ = ".";
    }   elsif( $_ eq "acc" && $root && $edg ){
	  push @accent, $t;
	  next;
	}
	
    warn "Verbify problem root=$root, $_, $t, $tc " 
	  unless defined($tense{$t}) ;
	
	# if($root && $t eq "cimp"){
	# print "Defining  cimp for $root,$edg,$p : $_ \n";
	# }
	
	# $res{$t}->[$p] = $_  if defined($_) and "$_" ne ".";
    $res{$t}->[$p] = $_   if "$_" ne ".";
    $p++;
  } 
  if($p==5){
    $res{$t}->[5] = $res{$t}->[4] ;
  }
  
  foreach $t (@accent){
	# $|=1;
	# !!! HERE : Would be great not to do call conjug 
	$res{$t}->[3] = conjug({"$root$edg"=>\%res},"s","$root$edg",$t,4);
	# Before iso-accentuating all 
	# $res{$t}->[3] =~ tr/\'\^/\^\'/ ;
	$res{$t}->[3] =~ tr/áéíâêî/âêîáéí/ ;
  }
  \%res;
  
}								# End verbify 

# Read a string in the format of $vlist, and put the equivalent data
# in a %verb hash.  
sub codify {
  
  my ($r,$v,$c,$f,$tmp,@s) = ("","","")  ;
  
  my $verb = ( ref($_[0]) eq "HASH") ? shift  : \%verb ;
  
  $_ = join("",@_);
  
  s/\#.*$//m;
  s/^\s+//m;
  s/\s+$//m;
  
  # @s = split(/(\w+)[\s\n]*([:=])/,$_ );
  # @s = split(/([\w\\\"\^\'\~]+)[\s\n]*([:=])/,$_ ); #'"
  # @s = split(/($wpat+)[\s\n]*([:=])/,$_ ); #'
  # @s = grep {/\S/} split(/([^=:])[\s\n]*([=:])[\s\n]*/,$_ ); #'
  @s = split(/[\s\n]*([=:])[\s\n]*([^=:]+)[\s\n]+([^=:]+)/,$_ ); #'
  @s = grep {/\S/} @s;
  # print " $#s \n";
  $s[$#s-1] .= pop @s;
  
  
  $v=shift @s;
  $v = shift @s unless $v;
  while( ($c=shift @s) && ($c!~/[:=]/) ){ # Skip if needed
	warn " codify first finds : >$v<, then >$c< \n";
	$v=$c;
  }
  
  $r= shift @s;
  
  
  while ( $c && $c=~/[:=]/ && $v && $r ){
	# print "codify loop : >$v< >$c< >$r< \n";
	
	if($r=~/[:=]/){warn "codify finds \$r = >$r< \n"}
	if($c eq ":"){ 
	  
	  $tmp = verbify( "$v $r " ); 
	  @{$verb->{$v}}{keys(%$tmp)} =  values(%$tmp);
	  
    } elsif( $v =~ /defectivos([1234])?/){ 
	  # print "found defective -- $v,$1,$r --\n";
	  foreach (split(/\s+/,$r)){
		s/[\n\s]+//g;
		next unless $_;
		# print "found defective >>$v,$1,$_<<\n";
		# $verb->{"defectivos". ($1 eq "3" ? "": "$1")}->{"$v"}= $1 ; 
		# print " Def $v,$1,defectivos",($1 eq "3") ? "": "$1","\n";
		# $verb->{defectivos}->{"$v"} = ($1 eq "3") ? "$v" : $1;
		$verb->{"defectivos". ($1 eq "3" ? "": "$1")}->{"$_"}= $1 ; 
		# print " Def $v,$1,defectivos",($1 eq "3") ? "": "$1","\n";
		$verb->{defectivos}->{"$v"} = ($1 eq "3") ? "$_" : $1;
	  }
	  
    } else {
      same_model($verb, "$v $r " ) ;
    }
    $v=shift @s; $c=shift @s; 
    $r= shift @s; 
  }
  if(@s){
	warn "codify leaves out $#s elements, of which >$v< >$c< >$r< \n";
  }
  
}								# End codify

# ### Make a list of knows verb names in the global variable \%verb.
sub list_verbs {
  
  my ($r,$v,$c,$f,$tmp,@s) = ("","","")  ;
  
  my $verb =  \%verb ;
  my @res;
  
  $_ = $vlist;
  
  s/\#.*$//m;
  s/^\s+//m;
  s/\s+$//m;
  
  # @s = split(/(\w+)[\s\n]*([:=])/,$_ );
  # @s = split(/([\w\\\"\^\'\~]+)[\s\n]*([:=])/,$_ );" 
  @s = split(/([$wpat]+)[\s\n]*([:=])/o,$_ ); #
  $v=shift @s;
  while( ($c=shift @s) && ($c!~/[:=]/) ){$v=$c;}
  
  $r= shift @s;
  
  while ( $c && $c=~/[:=]/ && $v && $r ){
	
    if($c eq ":"){ 
	  push(@res,$v);
    } elsif( $v =~ /defectivos([1234])?/){ 
	  foreach (split(/\s+/,$r)){
		s/[\n\s]+//g;
		next unless $_;
		# print "found defective >>$v,$1,$_<<\n";
		# $verb->{"defectivos". ($1 eq "3" ? "": "$1")}->{"$v"}= $1 ; 
		# print " Def $v,$1,defectivos",($1 eq "3") ? "": "$1","\n";
		# $verb->{defectivos}->{"$v"} = ($1 eq "3") ? "$v" : $1;
		push(@res,$v);
	  }
	  
    } else {
	  push @res,split(/\s+/,$r);
    }
    $v=shift @s; $c=shift @s; 
    $r= shift @s;
  }
  @res;
}


# verify( reference_string, [%verb] )
# Compares the reference string with the output of conjug.
sub verify {
  my ($errcnt,$r,$v,$c,$e,$f,$d,$d2,@s,@t,@u) = 
	(0,     "","","","","","","")  ;
  @s=@t=@u=();
  
  my ($res,$w,@ckd) = ("","");
  
  # print "Verify $#_ , \n", join(", ", @_ ),"\n";
  $_ = shift ;
  my $verb = ( ref($_[0]) eq "HASH") ? shift  : \%verb ;
  
  s/\#.*$//m;
  s/^\s+//m;
  s/\s+$//m;
  
  # print "Ver1 >$verb< ",($verb==\%verb)?"(\%verb)":"","\n";
  # @s = split(/(\w+)[\s\n]*([:=])/,$_ );
  # @s = split(/([\w\\]+)[\s\n]*([:=])/,$_ );
  
  @s = split(/($wpat+)[\s\n]*([:=])/o,$_ );
  
  # print "Ver2 ",join(", ",@s);
  
                                # Find first verb
  $v=shift @s;
  while( @s && ($c=shift @s) && ($c!~/[:=]/) ){$v=$c}
  
                                # @u = reference of conjugation
  @u= split("\n",shift @s);
  shift(@u) ;
  
  while ( $c && $c=~/[:=]/ && $v && @u ){
	
	
	if($c eq ":"){ 
	  # !!! HERE : Would be great no to do call conjug 
	  @t = split("\n",conjug(  $verb,"x" , $v ));
	  shift @t;
      
	  while ( defined($e=shift @u) && defined($d=shift @t) ){
		
		$e =~ s/\s+/ /g; $e =~ s/^\s+//; $e =~ s/\s+$//;
		$d =~ s/\s+/ /g; $d =~ s/^\s+//; $d =~ s/\s+$//;
		
		$d2 = $d;
		$d2 =~ s/\\/\\\\/g;
		$d2 =~ s/([^\\])([\'\"\^\~])/$1\\$2/g; #'" 
		# $d2 =~ s/([^\\])([\'\"\^\~])/$1\\$2/g;#'" 
		$w .= join("",tabcol(-2,[
								 sprintf("  %3d ",++$errcnt),
								 split(/\s/,$d),
								 "\n  REF ", split(/\s/,$e)] ) )
		  if ($e !~ /$d2/);
		# print ">$e<\n>$d2<\n" if ($e !~ /$d2/);
	  }
	  if($#u>=0){
		$w .= "   ABS ".join("\n   ABS ",@u)."\n"
	  }
	  if($#t>=0){
		$w .= "   EXC ".join("\n   EXC ",@t)."\n"
	  }
	  if( $w ) {
		$res .= "IN $v ".
		  ( defined($verb->{$v}->{model}) ? 
			"model   $verb->{$v}->{model}" : "" )
			."\n$w\n" ;
	  } else {
		push @ckd, $v;
	  }
	} 
	
	( $v, $c, @u ) = (@s) ? 
	  ( shift @s, shift @s, split("\n",shift @s)):
		("","",()) ;
	shift(@u) ;
	
	$w="";
	$errcnt = 0;
  }
  # print " $v, $c, $#u, $#s \n";
  $w = join(" ",sort(@ckd));
  $w =~ s/(.{80}\S+)/$1\nOK /g;
  $res .= "OK $w\n" if "$w";
  $res ;
}								# End verify


sub diffstr {
  
  my ($a,$b) = (@_);
  
  # while($a && substr($a,0,1) eq substr($a,0,1)){
  # substr($a,0,1)="";       substr($b,0,1)="";
  # }
  # while($a && substr($a,-1,1) eq substr($a,-1,1)){
  # chop $a;           chop $b;
  # }
  
}

############## SUBS FOR MODIFYINGS THE TERMINATIONS  ###########
# Each sub applies a simple spelling rule.

################# HERE : Take out all these
#################### needless arguments.
sub soft_g {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w=~ s/g([^g]+)$/j$1/ if( $w =~ /g[aou][^g]*$/);
  $w ;
}

sub soft_c {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w=~ s/c([^c]+)$/ç$1/ if( $w =~ /c[aou][^c]*$/);
  $w ;
}

sub hard_g {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w =~ s/g([^g]+)$/gu$1/ if($w =~ /g[ei][^g]*$/);
  $w;    
}

sub hard_c {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w =~ s/c([^c]+)$/qu$1/ if($w =~ /c[ei][^c]*$/);
  $w;    
}

sub cedilla {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w =~ s/ç[e]([^ç]*)$/ce$1/;
  $w;
}

sub end_gu {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w =~ s/gu([^g]+)$/g$1/ if $w =~ /gu[aou][^g]*$/;
  $w;
}

#sub end_oiar {
# my ( $w , $root, $edg , $p , $t ) = @_ ;
#
# $w =~ s/oó/ó/ ;
# $w;
#}

sub end_zer {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w =~ s/z.$/z/ if
	$p==3 && $t eq "pres" || $p == 2 && $t eq "ivo" ;
  
  $w;
}

sub end_uir {
  my ( $w , $root, $edg , $p , $t ) = @_ ;
  
  $w =~ s/[$vocs]([$cons]?)$/i$1/o if
	$t eq "pres" && ($p==2||$p==3) ||$t eq "ivo" && $p == 2 ;
  
  # Here ??Needed??
  $w =~ s/$root i/$root\'i/x if
	$t eq "imp" || $t eq "mdp" || $t eq "perf" && $p!=3 || 
	  $t eq "pres" && $p==4 ;
  
  $w;
}

# #################### THE MAIN FUNCTION IN THIS FILE ####################
# 
# conjug [[qvx] [verb]+ [tense]+ [1-6]+]+ 
# 

sub conjug {
  
  my($v,$w,@v,@t,@p);
  
  my ($verbose,$rc,$regexp,$isoacc) = (1,"c",0,1);
  
  #	print "Received : >",join("<   >",@_),"<\n";
  # print "HASH FOUND \n" if ( ref($_[0]) eq "HASH");
  my $verb = ( ref($_[0]) eq "HASH") ? shift  : \%verb ;
  
  # Extract options verb, tense and person.
  while( ($v=shift) && ($v=~ /^\-? [hvqrcsxi]+ $/x ) ){
	# print "option $v\n";
	if( $v=~/[iaeoô]r$/ ){ 
	  # unshift @_,$v;
	  # print "NOT OPT\n";
	  last ;
	}
	if   ( $v =~ /q/ ) {$verbose = 0 }
	elsif( $v =~ /v/ ) {$verbose = 1 }
	if   ( $v =~ /r/ ) { $rc = "r" }
	elsif( $v =~ /c/ ) { $rc = "c" }
	elsif( $v =~ /s/ ) { $rc = "s"; $verbose = 0; }
	elsif( $v =~ /h/ ) { $rc = "h"; }
	if   ( $v =~ /x/ ) { $regexp = 1 }
	if   ( $v =~ /i/ ) { $isoacc = 0; }
  }
  
  while( $v && !defined($alt_tense{$w = lc(un_accent($v)) }) && ($v!~/[\d]/)){ 
	# print "found verb $v\n";
	push @v,$v;
	$v=shift;
  }
  my $cur_verb = \%alt_tense;
  @t = ();
  
  $w = lc(un_accent($v)) if $v;
  #	print "$w\n";
  while( $w && defined($cur_verb->{$w}) ){
	if(ref($cur_verb->{$w}) eq "HASH" ){
	  $cur_verb = $cur_verb->{$w};
	} else {
	  push @t, $cur_verb->{$w};
	  $cur_verb = \%alt_tense;
	}
	$w = ($v = shift) ? 
	  lc(un_accent($v)) :
		""	;
	#			print "$w\n";
  }
  @t = @tense unless @t ;
  
  # if($v && defined($tense{$v})){
  # @t = ($v);
  # while(($v=shift) && defined($tense{$v})){ push @t,$v};
  #
  # } else {
  # @t = @tense;
  # }
  
  if( defined($v) && $v=~/^ [1-4,6] $/x ){
    @p = ($v);
    while(($v=shift) && $v=~ /^[1-4,6] $/x){ push @p,$v};
  } else {
    @p = (1..4,6);
  }
  
  # print "VERB  ",join(",",@v);
  # print "\nTENSE ",join(",",@t);
  # print "\nPERS  ",join(",",@p),"\n";
                                # CONJUGATION
  
  my  (@res,%res);              # Result (as array and hash),tmp.
  my ($root,$rr,$vr,$cr,$edg);  # Root, $root = "$rr$vr$cr$edg"; 
  my ($m,   $rm,$vm,$cm);       # Model $m  = "$rm$vm$cm$edg"; 
  my ($prefix, $missing);
  my ($y,$cy,$vy,$ey);          # Found conjugated form, 
  my ($ex,$z,$s);               # EXplicitely defined? temps.
  
  map {$_=asc2iso($_) if /[\"\'\^\\\~]/} @v ; #
  # print "CONJUG  \n>",join(",",@v),"<\n>", 
  # join(",",@t),"<\n>",join(",",@p),"<\n"; #'"
  foreach $v (@v) {
	
	# print " D1 " if $verb->{defectivos}->{$v};
	# print " D ";
	locate($verb,$v);
	
	# print " D2 " if $verb->{defectivos}->{$v};
	
	# Check that verb looks like a verb 
	unless( $v =~ /^(.*)([aeioô]r)$/ ){ 
	  warn "$v does not look like a verb." ;
	  next;
	}
	# Extract Root and Ending
	$edg  = $2;
	$root = $1; 
	
                                # Is there a recognizable model 
    if   ( $v =~ /g[ei]r$/ )    {  $modif = \&soft_g }
    elsif( $v =~ /c[ei]r$/ )    {  $modif = \&soft_c }
    elsif( $v =~ /g[ao]r$/ )    {  $modif = \&hard_g }
    elsif( $v =~ /çar$/ )    {  $modif = \&cedilla }
    elsif( $v =~ /c[ao]r$/ )    {  $modif = \&hard_c }
    elsif( $v =~/gu[ei]r$/ )    {  $modif = \&end_gu }
    elsif( $v =~ /[^g]uir$/)    {  $modif = \&end_uir }
    elsif( $v =~ /air$/)        {  $verb->{$v}->{model} = "sair" 
									 unless $v eq "sair" }
    elsif( $v =~ /oer$/)        {  $verb->{$v}->{model} = "moer" 
									 unless $v eq "moer" }
    elsif( $v =~ /oar$/)        {  $verb->{$v}->{model} = "perdoar" 
									 unless $v eq "perdoar" }
    elsif( $v =~ /oiar$/ && $v ne "boiar" )        {  
	  $verb->{$v}->{model} = "boiar" ; 
	  # $modif = \&end_oiar ;
    }
    elsif( $v =~ /(uzir|zer)$/ ){  $modif = \&end_zer }
    elsif( $v =~/ear$/ )        {  $verb->{$v}->{model} = "passear" 
									 unless $v eq "passear" }
    else                        { $modif = 0 }
	
	# if($v =~/or$/){       # verbs in "or"
	# $verb->{$v}->{model} = "pôr" unless defined($verb->{$v});
	# }
	
    if($verbose){
	  
      push @res, "$v : ", defined($verb->{defectivos}->{$v}) ? 
		("defectivo","") :
		  defined($verb->{$v}) ?
			defined($verb->{$v}->{model}) ?
			  ("model",$verb->{$v}->{model}) :
				("irreg","") : ("",""), 
				("","",""),   
			}
	
    if( defined($verb->{$v}) ) { # Irregular Verb
	  
      warn " Root $v -> $root ,$cpat,of unexpected kind" unless
        (($rr,$vr,$cr) = 
         ($root  =~  /^ (.*) ([$vocs]+) ($cpat* \^?) $/ox ))
		  || $root=~/^ $cpat* \^? $/ox ;  
	  # The \^? serves only for p^or
	  # print "Root $root yields ($rr,$vr,$cr,$edg)\n";
	  
	  # Is there a model ?
      if(defined($m = $verb->{$v}->{model})){
		locate($verb,$m) unless defined($verb->{$m});
		($rm = $m) =~ s/..$//;
		# print "Model : $rm, $m \n";
		($vm,$cm) = ($rm   =~   / ([$vocs]+) ($cpat{0,2}) $/ox ); 
		
		# print "   Model $model yields ($rr,$vr,$cr,$edg) \n";
		# print "   Prefix is $prefix\n" if 
		$missing = 0;
		unless(($prefix) = ($v=~/(.*)$m$/)){
		  my $em = substr($rm,1);
		  unless((length($em)>1) && 
				 (($prefix) = ($v=~/(.*)$em$/)) && ($missing=1)) {
			$em= substr($em,1);
			length($em)>1 && 
			  (($prefix) = ($v=~/(.*)$em$/)) &&  ($missing=2);
		  }
		  # print " em $em ";
		}
		# print "Prefix $m, $v, $prefix, $missing\n";
		
      }
	  
	  
      foreach $t (@t){ 
		
        next unless defined($reg{er}->{$t});
		
        push @res, "$t" if $verbose ;
        
        foreach $p (@p){
          
		  # Is it explicitly defined ?
		  $ex = ($w = $verb->{$v}->{$t}->[$p-1])?1:0 ;
		  
		  if(!$w && $m &&
			 ($y = $verb->{$m}->{$t}->[$p-1]) ){ 
			
			# pass from explicit model to conjd. form.
			if($prefix){                    
			  $y = substr($y,$missing); # SUSPICIOUS
			  $w= "$prefix$y";
			} else {
			  warn " $y ,$t,$p,$endg{$t}->[$p-1] of unexpected kind" 
				unless
				  ($vy,$cy,$ey) = 
					$y=~/ ($vpat+) ($cpat?) ($endg{$t}->[$p-1]) $/x;
			  
			  # print "cm,cy = $cm,$vy,$cy,$ey\n";
			  $w = ($cm eq $cy) ? 
				"$rr$vy$cr$ey" : "$rr$vy$cy$ey" ;
			  
			}
		  }
		  
		  if( (!$w) && ("$t" eq "cpres") &&
			  (($y=$verb->{$v}->{cpres}->[0]) || 
			   ($m && ($y=$verb->{$m}->{cpres}->[0])))  ){ 
			
			# print "Root $root , $rr , $vr , $cr , $edg \n";
			$vy=$cy=$ey="";
			warn "Cpres bug $y ($vy,$cy,$ey)" unless 
			  ($vy,$cy,$ey) = $y  =~ 
				/ ($vpat+) ($cpat?) ($endg{cpres}->[0]) $/x;
			# print "Cpres rule $y ($vy,$cy,$ey) <$endg{cpres}->[0]> \n";
			$y=  ($cr eq $cy) ? "$rr$vy$cy" : "$rr$vy$cr" ;
			$w = "$y$reg{$edg}->{cpres}->[$p-1]";
		  }                
		  
		  # Default Conjuntivo passado/futuro for irregular
		  # verbs is built from 1st person perfeito 
		  if( (!$w) && ("$t" eq "cimp" || "$t" eq "cfut") &&
			  (($y=$verb->{$v}->{perf}->[0]) || 
			   ($m && ($verb->{$m}->{perf}->[0])))  ){ 
			
			if(!$y) { 
			  $y = $verb->{$m}->{perf}->[0];
			  if($prefix){
				$y = substr($y,$missing); # SUSPICIOUS
				$y="$prefix$y"; 
			  } else {
				$vy=$cy=$ey="";
				warn "Cpassad bug $y ($vy,$cy,$ey)" unless 
				  ($vy,$cy,$ey) = $y  =~ 
					/ ($vpat+) ($cpat?)($endg{$perf}->[0]) $/x;
				
				$y=  ($cr eq $cy) ? "$rr$vy$cr" : "$rr$vy$cy" ;
			  }
			}
			$z = $reg{$edg}->{$t}->[$p-1];
			
			# ??                if($y=~s/([\'\^\"]?[$vocs])$//){#"
			if($y=~s/([$vocs])$//ox){
			  $z = $1.$z;
			  $z = iso2asc($z);	# Swap accents
			  $z =~  s/^([\'\^\"])([$vocs])([\'\^\"]?)([$vocs])/$1$2/ox 
				|| $z =~ s/^([$vocs])([\'\^\"]?)([$vocs])/$2$1/ox; #"
			  $z = asc2iso($z);
			}
			$y .= $z;
			
			# $w = "$y";

			$w = $y;

			# Default imperativo is built from conjuntivo
		  } elsif (!$w  && "$t" eq "ivo" && $p!=1 && 
				   (($y=$verb->{$v}->{cpres}->[$p-1]) || 
					($m && $verb->{$m}->{cpres}->[$p-1] ))
				  ){
			
			if(!$y) { 
			  if($prefix){
				$y="$verb->{$m}->{cpres}->[$p-1]"; 
				$y = $prefix . substr($y,$missing); # SUSPICIOUS
			  } else {
				$y = $verb->{$m}->{cpres}->[$p-1];
				$vy=$cy=$ey="";
				warn "Ivo bug $y , $p,  ($vy,$cy,$ey) " unless 
				  ($vy,$cy,$ey) = $y  =~ 
					/ ([$vocs]) ($cpat) ($endg{cpres}->[$p-1]) $/x;
				# print "-$endg{cpres}->[$p-1]-$y-$1-$2-$3\n";
				$y= "$rr$vy$cr$ey";
			  }
			}
			$w = "$y";
			
		  }
		  
		  $w = "$root$reg{$edg}->{$t}->[$p-1]"   if 
			!$w  && defined($reg{$edg}->{$t}->[$p-1]) ;
		  
		  $w = &$modif( $w ,$root, $edg ,$p ,$t ) 
			if(  $w && !$ex && $modif );
		  
		  unless( $regexp || !defined($w)){
			$w =~ s/ \[ ([^\]]) [^\]]* \] /$1/gx; 
			$w =~ s/ \( ([^\|\)]*) \|? .* \) /$1/gx; 
		  }
		  
		  if($verb->{defectivos}->{$v}){
# Is this code ever used ?
			# print "Defectivo\n";
			if( $verb->{defectivos}->{$v}=~ /[12]/ &&
				
				!( $reg{$edg}->{$t}->[$p-1] =~
				   /["^$vocs"]*["$vocs"]["^$vocs"]*["$vocs"]/o ||
				   $reg{$edg}->{$t}->[$p-1] =~
				   /["^$vocs"]*(["$vocs"])/o && 
				   ($1 eq "i" ||  $1 eq "í" ||(print("AAA\n") && 0)||
					$verb->{defectivos}->{$v}==2 && $1 eq "e") 
				 )
				|| $verb->{defectivos}->{$v} == 4 && $p!=3 && $p!=6
				|| $verb->{defectivos}->{$v} eq "precaver" && 
				( $t eq "pres" && $p!=4 || $t =~ /(cpres|ivo)/
				)
				|| $verb->{defectivos}->{$v} eq "adequar" && 
				( $t =~ /c?pres/ && $p!=4 || $t eq "ivo" )
				
			  ){
			  # unless($v eq "recear" && $t !~ /(c?pres|ivo)/  ||
			  
			  $w = " ";
			}
		  }
		  
		  $w=~s/^x$/ / if $w ;
		  
		  push @res, $w ;
		  $res{$t}->[$p] = $w;
        }
	  }
	} else {					# Regular Verb
	  
      foreach $t (@t){  
		
        next unless defined($reg{er}->{$t});        
		
        push @res, "$t" if $verbose ;
		
        foreach $p (@p){
          $w = "";
          
          # Duplicate code
          if(defined($s = $reg{$edg}->{$t}->[$p-1])) {
			$w="$root$s";
			$w = &$modif( $w ,$root, $edg ,$p ,$t ) if( $modif );
			
			if($verb->{defectivos}->{$v}){
#			  print "Def\n";
			  if( $verb->{defectivos}->{$v}=~ /[12]/ &&
				  
				  !( $reg{$edg}->{$t}->[$p-1] =~
					 /["^$vocs"]*["$vocs"]["^$vocs"]*["$vocs"]/o ||
					 $reg{$edg}->{$t}->[$p-1] =~
					 /["^$vocs"]*(["$vocs"])/o && 
					 ($1 eq "i" || $1 eq "í" || 
					  $verb->{defectivos}->{$v}==2 && $1 eq "e") 
				   )
				  || $verb->{defectivos}->{$v} == 4 && $p!=3 && $p!=6
				  || $verb->{defectivos}->{$v} eq "precaver" && 
				  ( $t eq "pres" && $p!=4 || $t =~ /(cpres|ivo)/
				  )
				  || $verb->{defectivos}->{$v} eq "adequar" && 
				  ( $t =~ /c?pres/ && $p!=4 || $t eq "ivo" )
				  
				){
				# unless($v eq "recear" && $t !~ /(c?pres|ivo)/  ||
				
				$w = " ";
			  }
			  
			}
          } 
          $w=~s/^x$/ /;
          push @res, $w ;
          $res{$t}->[$p] = $w;
        } } }
  }
  
  unless($isoacc){
	# print "Iso un-accentuating \n";  
	if($rc ne "h"){
	  @res = iso2asc(@res);
	}else{
	  @res{keys(%res)}=iso2asc(values(%res));
	}
  }
  # Format output
  if   ( $rc eq "c" ){ return tabcol($verbose+@p,\@res); }
  elsif( $rc eq "r" ){ return tabrow($verbose+@p,\@res); }
  elsif( $rc eq "s" ){ $_ = join(" ",@res); s/\s+$//mg; return $_ }
  elsif( $rc eq "h" ){ return \%res }
  return \@res ;
  
}

# Tries to find a verb in $vlist (string containing verb defs)
# Eventually, finds model verbs for it.
sub locate {
  
  my $verb = ( ref($_[0]) eq "HASH") ? shift : \%verb ;
  # HERE  5 7 97
  # print "locate($_[0]) with ",($verb==\%verb)?"global":"local","\n";
  my $v=$_[0];
  
  return if !$v || defined($verb->{$v});
  
  while( $v ){
	
    return if defined($verb->{$v});
	
	# print "Trying to locate >>$v<<\n";
    if( $vlist =~ / \b$v \s* : \s* ( [^=:]+ [=:]? ) /mx ){
	  # print "Located >>$1<<\n";
      $_ = $1 ;
      s/\S+\s*[:=]//g;
	  
	  # print "Becomes >>$v $_<<\n";
      $verb->{$v} = verbify( "$v $_" );
	  
    }
	
    my $m = "";
    if($vlist =~ /  \b$v \s* ([^\s=:]|\Z)  /x  &&  
	   # $`  =~ / \b(\S*)\s* ( = [^:=]*) \Z/x ){
       $`  =~ / ([^\s\n]*)\s* ( = [^\:\=]*) \Z/x && 
       $1 !~/^defectivos[1234]?$/ ){
	  
	  # print "found for model : >>$1,$2<<\n";
	  $m = $1;  
	  $verb->{$v}->{model} = $m ;
    }
	
    if($vlist =~ /  defectivos([1234])?\s* ( = [^\:\=]*) \b$v \s*
       ([^\s=:]|\Z)  /x  
	  ){
	  
	  # print "FOUND DEFECTIVE >>$1,$2<<\n";
	  $verb->{"defectivos". ($1 eq "3" ? "": "$1")}->{"$v"}= $1 ; 
	  # print " Def $v,$1,defectivos",($1 eq "3") ? "": "$1","\n";
	  $verb->{defectivos}->{$v} = ($1 eq "3") ? "$v" : $1;
	  $v="";
	  
    }
    $v = $m ;
	
  }
  
}
######################################################################
################ A few Output-formatting functions ###################

# Tabify a list into a string
sub tabcol {
  my ($ncols,$l) = @_ ;
  
  # print "tabcol received $ncols, $#$l ",join(" ,",@$l),"\n";
  
  $ncols = 1 unless $ncols;
  $ncols = ($#{$l} + 1)/(-$ncols) if($ncols<0);
  
  my @mx = (0) x $ncols ;		# not 0 x $ncols or whatever 
  
  my ($i,$res,$a)   = (0,"",0) ;
  
  foreach (@$l) { 
	# $mx[$i] = $a if( $mx[$i] < ($a=length($_))); 
    $mx[$i] = $a   if( defined($_) && ($mx[$i]  < ($a=length($_)))); 
    $i = ($i+1)% $ncols ;
  }
  
  # print "mx ",join(" ,",@mx),"\n";
  $i=0;
  foreach (@$l) { 
    $res .= sprintf("%-$mx[$i]s ",defined($_)?$_:"");
    $i = ($i+1)%$ncols ;
    $res .= "\n" unless $i ;
  }
  $res .= "\n" unless $res =~ /\n$/;
  $res;
}

# Tabify a list into a string
sub tabrow {
  my ($nrows,$l) = @_ ;
  my $nn = $#$l+1 > $nrows ? $#$l+1 : $nrows ;

  $nrows = 1 unless $nrows;
  $nrows = ($#{$l} + 1)/(-$nrows) if($nrows<0);
  
  my @mx = (0) x $nn ;
  my @res = "" x $nn ;
  my ($i,$j,$a)  = (0,0,"") ;
  # print "n=$nrows $#$l $nn\n";
  foreach (@$l) { 
	$_ = "" unless defined($_);
    $mx[$j] = $a if(defined($_) && 	$mx[$j] < ($a=length($_))); 
    $i = ($i+1)% $nrows ;
    $j++ unless $i;
  }
  
  $i=$j=0;
  foreach (@$l) { 
    $res[$i] .= sprintf("%-$mx[$j]s ",$_);
    $i = ($i+1)%$nrows ;
    $j++ unless $i;
  }
  $res = join("\n",@res)."\n";
  $res =~ s/\n[\n\s]+/\n/mg;
  $res;
}
######################################################################

# I just hope this way of exiting does not break down in the future.

return 1 if defined($allready_passed_here);

ALL_THE_WAY:
$allready_passed_here=1;

# ## Database for Portuguese verbs. The non-commented text below has the
# ## format : 
# 
# model_verb =  verb1  verb2 ... 
#
# ## To specify that verb1, verb1 ... conjugate like model_verb.
#
#
# verb : conjugo conjugues ...
# 
# ## To specify the conjugation of verb. 
#
# # WARNING ### don't write "=" and ":" on the same line.
# 
# Order of tenses :
# pres perf imp fut mdp  cpres cimp cfut cond ivo pp grd 
# 
# Cool : Emacs perl-mode highlights the infinitives (as labels?). 

$vlist = <<EOD ; 

obter: obtenho obténs obtém ivo obtém model ter 
abster: abstenho absténs abstém ivo abstém model ter 
ater: atenho aténs atém ivo atém model ter 
conter: contenho conténs contém ivo contém model ter 
deter: detenho deténs detém ivo detém model ter 
entreter: entretenho entreténs entretém ivo entretém model ter 
reter: retenho reténs retém ivo retém model ter 
suster: sustenho susténs sustém ivo sustém model ter 

# obter = abster ater conter deter entreter reter suster # phoey
boiar:
  bóio etc 
  cpres bóie bóies bóie boiemos bóiem
  ivo bóia bóie boiemos bóiem
perdoar:
  perdoo perdoas perdoa perdoamos perdoam 
moer:
  moo moís mói moemos moem,
  moí, 
  moía moías  moía moíamos moíam, cfut moer etc cimp moesse etc
  ivo mói pp moído
passear:
  passeio passeias passeia passeamos passeiam,
  ivo passeia  passeie  . . passeiem
incendiar:
  incendeio incendeias incendeia incendiamos incendeiam 
  cpres incedeie incendeies incendeie incendiemos incendeiem 
  ivo incendeia incendeie incendiemos  incendeiem
incendiar = ansiar mediar odiar remediar   
dizer: 
  digo . diz,
  disse disseste disse dissemos disseram,,
  direi etc
  cpres diga etc,
  cimp dissesse dissesses dissesse dissêssemos dissessem
  cond diria etc,
  ivo diz,
  pp dito
dizer = desdizer
fazer = contrafazer desfazer satisfazer refazer
fazer:
  faço . faz ,
  fiz fizeste fez fizemos fizeram ,
  fazia fazias fazia fazíamos faziam,
  fut farei farás fará faremos farão,
  fizera etc , # fizeras fizera fizéramos fizeram,
  faça etc , # faças faça façamos façam,
  cond faria etc , # farias faria faríamaos faria
  ivo faz 
  pp feito
dar: 
  dou dás dá damos dais dão,
  dei deste etc 
  mdp dera deras dera déramos deram,
  dê dês dê dêmos deis dêem,
  desse etc 
  der deres der dermos derem ,
poder:
  posso podes etc
  pude pudeste pôde pudemos puderam, 
  mdp pudera etc
  cpres possa etc
  cimp pudesse pudesses pudesse pudéssemos pudessem 
caber:
  caibo perf coube etc cpres caiba etc 
  cimp acc 
  mdp coubera acc etc
sentir:
  sinto sentes etc
  cpres sinta etc
  # HERE Must check
  ivo sente sinta etc
sentir = ressentir assentir consentir mentir desmentir
ir:
  vou  vais  vai vamos ides vão ,
  fui  foste foi fomos foram , 
  cpres vá vás vá vamos vão, 
  fosse fosses fosse fôssemos fossem,
  for fores for formos foram
  ivo vai ide vamos vão
valer:
  valho vales vale valemos valem,
  cpres valha etc
  ivo vale 
prover: perf provi etc pp provido model ver 
rever:  model ver 
sair:
  saio sais sai saímos saem,
  saí saíste saiu saímos saíram,
  saía saías saía saíamos saíam
  mdp saíra saíras saíra saíramos saíram
  cpres saia saias saia saiamos saiam
  ivo   sai saia saiamos saiam
abrir: pp aberto
abrir = entreabrir
saber:  
  sei   sabes    sabe  sabemos  sabem , 
  soube soubeste soube soubemos souberam 
  mdp soubera acc etc 
  cpres saiba etc # saibas saiba saibamos saibam
  cimp acc 
  ivo sabe
querer: 
  . . quer . . ,
  quis quiseste quis quisemos quiseram,
  mdp quisera acc etc 
  cpres queira etc
  cimp quisesse acc etc 
requerer: 
  requeiro . requer ,
  requeri requereste requereu requeremos requerem ,
  cpres requeira etc , cimp requeresse etc , cfut requerer etc 
  ivo requer 
ganhar: pp (ganho|ganhado)
gastar: pp gast(|ad)o
pagar: pp pago
trazer:
  trago trazes traz trazemos trazem,
  trouxe trouxeste etc
  mdp trouxera acc etc 
  fut tarei trarás trará traremos trarão, 
  cpres traga etc
  cond traria etc
  ivo traz traga etc
ferir:  firo cpres fira ivo fere fira etc
ferir = conferir preferir transferir gerir digerir preterir
    servir divertir advertir reflectir repetir compelir vestir 
seguir:
  sigo cpres siga etc ivo segue 
seguir = perseguir prosseguir
#  pres perf imp fut mdp  cpres cimp cfut cond ivo pp grd 
ler:
  leio lês lê lemos lêem
  cpres  leia leias leia leiamos  leiam
  ivo   lê lêia leiamos leiam
ler = reler tresler
atribuir:
  atribuo atribuis atribui atribuímos atribuem,
  atribuí atribuíste  atribuiu  atribuímos  atribuíram, 
  atribuía atribuías  atribuía  atribuíamos  atribuíam, 
  cfut   atribuir . atribuir atribuirmos .
  ivo atribui
averiguar: 
  cpres averigúe averigúes  averigúe .  averigúem 
  ivo averigua
pedir: 
  peço cpres peça etc ivo pede peça etc
ver:  
  vejo vês  vê vemos vêem,
  vi    viste viu vimos viram,
  mdp vira etc
  cpres veja vejas veja vejamos vejam
  ivo vê
  pp visto
ver = antever antrever prever rever 
vir:  
  venho vens vem vimos vêm,
  vim vieste veio viemos vieram,
  vinha vinhas vinha vínhamos vinham,
  mdp   viera vieras viera viéramos vieram,
  cpres venha venhas venha venhamos venham,
  cimp viesse viesses viesse viéssemos viessem,
  cfut vier vieres vier viermos vierem,
  pp vindo
vir = advir convir intervir   

ouvir: 
  o[iu]ço ouves ouve ouvimos ouvem, 
  cpres o[iu]ça etc # ouças ouça ouçamos ouçam,
  # alternative : cpres oiça  oiças  oiça  oiçamos  oiçam 
  ivo ouve oiça 
  # alternative : ivo . oiça 
rir:
  rio ris ri rimos riem
  cpres ria rias ria ríamos riam
  ivo ri ria riamos riam 
fugir: 
  fujo foges foge fugimos fogem ivo foge
dormir:   durmo , cpres durma 
cobrir:   cubro cpres cubra pp coberto 
cobrir = encobrir descobrir
agredir:  agrido agrides etc , cpres agrida etc ivo agride
agredir = prevenir progredir transgredir

                                # More irregular verbs
escrever: pp escrito
escrever = descrever inscrever reescrever prescrever
dormir =  abolir demolir

reaver:
       x x x  reavemos reaveis x ,
       reouve reouveste reouve reouvemos reouveram,
       reavia reavias reavia reavíamos reaviam,
       reaverei reaverás reaverá reaveremos reaverão,
       reouvera reouveras reouvera reouveramos reouveram,
       x x x x x,
       reouvesse reouvesses reouvesse reouvéssemos reouvessem,
       reouver reouveres reouver reouvermos reouverem,
       reaveria reaverias reaveria reaveríamos reaveriam,
       x x x x, reavido  reavendo

pedir = despedir medir impedir expedir
# Double Particípio Passado
aceitar: pp aceit(o|e|ado)
afeiçoar: pp afe(ct|içoad)o
cativar: pp cativ(|ad)o
cegar: pp cag(|ad)o
completar: pp complet(|ad)o
cultivar: pp cult(|ivad)o
descalçar: pp descalç(|ad)o
entregar: pp entreg(ue|ado)
enxugar: pp enxu(t|gad)o
expulsar: pp expuls(|ad)o
fartar: pp fart(|ad)o
findar: pp find(|ad)o
infectar: pp infect(|ad)o
inquietar: pp inquiet(|ad)o
isentar: pp isent(|ad)o
juntar: pp junt(|ad)o
libertar: pp libert(|ad)o
limpar: pp limp(|ad)o
manifestar: pp manifest(|ad)o
matar: pp (matado|morto)
murchar: pp murch(|ad)o
ocultar: pp ocult(|ad)o
salvar: pp salv(|ad)o
secar: pp sec(|ad)o
segurar: pp segur(|ad)o
fechar: pp fech(|ad)o
afligir: pp afli(t|gid)o 
concluir:pp conclu(s|id)o
corrigir:pp corr(ect|igid)o
dirigir:pp dir(ect|igid)o
distingir:pp distin(t|guid)o
emergir:pp emer(s|gid)o
erigir:pp er(ect|igid)o
exprimir:pp expr(ess|imid)o
extinguir:pp ext(int|inguid)o
frigir:pp fri(t|gid)o
imergir:pp imer(s|gid)o
imprimir:pp impr(ess|imid)o
incluir:pp inclu(s|id)o
inserir:pp ins(ert|erid)o
omitir:pp om(ess|itid)o
oprimir:pp opr(ess|imid)o
repelir:pp rep(uls|elid)o
submergir:pp submer(s|gid)o
atingir:pp atin(t|gid)o
absorver:pp absor(t|vid)o
acender:pp ace(s|ndid)o
agradecer:pp (grat|agradecid)o
atender:pp aten(t|did)o
benzer:pp ben(t|zid)o
convencer:pp conv(ict|encid)o
corromper:pp corr(upt|ompid)o
defender:pp def(es|endid)o
dissolver:pp dissol(lut|vid)o
eleger:pp ele(it|gid)o
envolver:pp envol(t|vid)o
incorrer:pp inc(urs|orrid)o
morrer:pp mor(t|rid)o
nascer:pp na(d|scid)o
perverter:pp perver(s|tid)o
prender:pp pre(s|ndid)o
pretender:pp preten(s|did)o
revolver:pp revol(t|vid)o
romper:pp ro(t|mpid)o
submeter:pp subm(iss|etid)o
suspender:pp suspen(s|did)o
tender:pp ten(s|did)o


defectivos1= abolir adir banir carpir colorir combalir comedir
delinquir delir demolir descomedir embair empedernir escapulir
extorquir falir florir munir remir renhir retorquir 
 
defectivos2= aturdir brandir brunir emergir exaurir fremir fulgir
haurir imergir jungir submergir ungir #

defectivos4= acontecer concernir grassar constar assentar

defectivos3=    precaver adequar


EOD
goto I_M_BACK;
#}
