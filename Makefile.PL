
use ExtUtils::MakeMaker;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
$Verbose = 1;


WriteMakefile(
    'NAME'         => 'Lingua::PT::Conjugate',
    'VERSION_FROM' => 'Conjugate.pm',
    'EXE_FILES' => ['conjug','treinar'],
   'MAN1PODS'=>{'conjug.pod'=>'$(INST_MAN1DIR)/conjug.1',
                'treinar.pod'=>'$(INST_MAN1DIR)/treinar.1',},
   'MAN3PODS'=>{'Conjugate.pod'=>'$(INST_MAN3DIR)/Lingua::PT::Conjugate.3',},
    'PM'        => {
                    'Conjugate.pm'=>'$(INST_LIBDIR)/Conjugate.pm',
                    },
    'dist'         => { COMPRESS => "gzip -9f",
                        SUFFIX => 'gz',
                      }
);

sub MY::postamble { q(

conjug.pl: conjug conjug.pod Conjugate.pm
	perl -pe 's/perl\s+\-w/perl /' < Conjugate.pm > conjug.pl
	cat conjug.pod >> conjug.pl
	perl -pe 's/use/import/' < conjug >> conjug.pl
	chmod +x conjug.pl

treinar.pl: treinar treinar.pod Conjugate.pm
	perl -pe 's/perl\s+\-w/perl /' < Conjugate.pm > treinar.pl
	cat treinar.pod >> treinar.pl
	perl -pe 's/use/import/ if /PT/' < treinar >> treinar.pl
	chmod +x treinar.pl

)
}
