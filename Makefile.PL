
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
#   'MAN3PODS'=>{'Conjugate.pod'=>'$(INST_MAN3DIR)/Conjugate.3',},
    'PM'        => {'Accent_iso_8859_1.pm' => 
                        '$(INST_LIBDIR)/Accent_iso_8859_1.pm',
                    'Conjugate.pm'=>'$(INST_LIBDIR)/Conjugate.pm',
#                   'verb.pt'=>'$(INST_LIBDIR)/verb.pt'
                    },
    'dist'         => { COMPRESS => "gzip -9f",
                        SUFFIX => 'gz',
                      }
);

