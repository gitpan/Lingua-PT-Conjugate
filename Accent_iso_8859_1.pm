#!/usr/bin/perl -w

# 
# Accent_iso_8859_1.pm
# Author          : Etienne Grossmann
# Created On      : December 1997
# Last Modified On: January  1998
# Language        : Perl
# Status          : Use with caution!
# 
# (C) Copyright 1998 Etienne Grossmann
# 
# 
#              Convert to-from iso accent
# 01/10/97 
# Bug : 'e   -(iso2asc)->  'e   -(asc2iso)->  chr(233)!="'e" 
# Fix : iso2asc("'")  == "' "
#       asc2iso("' ") == "'"
# 

package Lingua::PT::Accent_iso_8859_1;
use Exporter ;
@ISA = qw(Exporter);
# Yes, this package is a namespace polluter. 
@EXPORT = qw(iso2asc asc2iso);
@EXPORT_OK = qw( iso2ascii ascii2iso );
%iso2ascii = (
           "\'"     =>"' ",
           chr(0347)=>'\c',
           
           chr( 224)=>'`a',
           chr( 225)=>'\'a',
           chr( 226)=>'^a',
           chr( 227)=>'~a',

           chr( 232)=>'`e',
           chr( 233)=>'\'e',
           chr( 234)=>'^e',

           chr( 236)=>'`i',
           chr( 237)=>'\'i',
           chr( 238)=>'^i',

           chr( 211)=>'\'O',
           chr( 242)=>'`o',
           chr( 243)=>'\'o',
           chr( 244)=>'^o',
           chr( 245)=>'~o',

           chr( 249)=>'`u',
           chr( 250)=>'\'u',
           chr( 251)=>'^u',
           );
%ascii2iso = reverse %iso2ascii;
%ascii2iso_keys = (
            "\' "     =>"'", 
           '\\\\c'=>chr(0347),
           
           '\`a'=>chr( 224),
           '\'a'=>chr( 225),
           '\^a'=>chr( 226),
           '\~a'=>chr( 227),

           '\`e'=>chr( 232),
           '\'e'=>chr( 233),
           '\^e'=>chr( 234),

           '\`i'=>chr( 236),
           '\'i'=>chr( 237),
           '\^i'=>chr( 238),

           '\'O'=>chr( 211),
           '\`o'=>chr( 242),
           '\'o'=>chr( 243),
           '\^o'=>chr( 244),
           '\~o'=>chr( 245),

           '\`u'=>chr( 249),
           '\'u'=>chr( 250),
           '\^u'=>chr( 251),

              );
# Accent-matching regexp
$find_iso_accent = "[".join("",keys(%iso2ascii))."]";

# Accent-matching regexp
$find_ascii_accent = join("|",keys(%ascii2iso_keys));

sub iso2asc {
    my ($x,@res);

#    print "iso2asc : ";
    while( $#_ >=0 ){
        $x = shift @_ ;
#        print "$x, ";
        $x=~s/($find_iso_accent)/$iso2ascii{$1}/g if defined($x);
        push @res,$x;
    }
#    print "\n";
    $#res || wantarray ? @res : $res[0] ;
}

sub asc2iso {
    my ($x,@res);

#    print " N args $#_ \n";
#    print "\nrrr",join("RRR\nRRR",@_),"rrr\n";
    while( $#_>=0 ){
        $x = shift @_;
        $x=~s/($find_ascii_accent)/$ascii2iso{$1}/g if $x; 
        push @res,$x;
    }
#    print "\n SSS ",join("sss \n sss ",@res)," SSS \n";
    $#res ? @res : $res[0] ;
}

1;
