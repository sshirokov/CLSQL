# Make shared library for Oracle 9
 
if [ -z "$ORACLE_HOME" ]; then 
  ORACLE_HOME=/9i
fi

EMPTY_LIBS=-lclntst9
LIBS="-lagent9 -lagtsh -lclntsh -lclntst9 -lclient9 -lvsn9 -lcommon9 -lskgxp9 -lmm -lnls9 -lcore9 -lgeneric9 -ltrace9 -ldl -lgcc -lm"
OBJECTS="rdbms/lib/defopt.o"
LIBNAME=oracle

gcc -shared ${ORACLE_HOME}/rdbms/lib/homts.o -L ${ORACLE_HOME}/lib $LIBS -o ${LIBNAME}.so
