if [ -z "$ORACLE_HOME" ]; then 
  ORACLE_HOME=/10g/app/product/10.1.0/db_1
fi

EMPTY_LIBS=-lclntst10
LIBS="-lagent10 -lagtsh -lclntsh -lclient10 -lvsn10 -lcommon10 -lskgxp10 -lmm -lnls10 -lcore10 -lgeneric10 -ldl -lgcc -lm"
OBJECTS="rdbms/lib/defopt.o"
LIBNAME=oracle

gcc -shared -L $ORACLE_HOME -L ${ORACLE_HOME}/lib $ORACLE_HOME/rdbms/lib/homts.o $LIBS -o ${LIBNAME}.so
