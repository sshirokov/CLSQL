#include <stdio.h>
#include "/opt/mysql/include/mysql/mysql.h"

int main (int argc, char** argv)
{
  printf ("Size of MYSQL struct: %ld\n", sizeof (MYSQL));
}
