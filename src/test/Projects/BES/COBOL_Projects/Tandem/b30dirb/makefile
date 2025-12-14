#
# File: makefile
# Synopsis: Guardian GMAKE Make file for copylib
#
#

TARGET_EXE      = XB30DIRB
TARGET_SYSTYPE  = guardian
TARGET_SOURCES  =
TARGET_OBJS     = XB30DIRB
TARGET_LIBS     = -l cre -l crtl  -l rld
LIB_ROOT        = ${COMP_ROOT}\usr\lib
COBOL_COMP      = $SYSTEM.SYSTEM.xCOBOL


COBOL_OPTS = optimize 0, symbols, suppress, runnable
LINK_OPTS  =  -Wtarget=tns/x -Wcall_shared -Weld=-bLocalized -Weld="-unres_symbols Ignore" -Weld=-Noverbose -Winspect -Whighpin=on -Whighrequesters=on

all: ${TARGET_EXE}


XB30DIRB: B30DIRB
        ${COBOL_COMP} /IN B30DIRB/ XB30DIRB; ${COBOL_OPTS}


.PHONEY: clean
clean:
        $system.system.fup purge o*!
        $system.system.fup purge XB30DIRB!

