/****************************************************************************
 * FILE IDENTIFICATION
 *
 *   Name:          clsql-mysql.c
 *   Purpose:       Helper functions for mysql.cl to handle 64-bit parts of API
 *   Programmer:    Kevin M. Rosenberg
 *   Date Started:  Mar 2002
 *
 * $Id: clsql-mysql.c,v 1.2 2002/12/09 10:34:16 kevin Exp $
 *
 * This file, part of CLSQL, is Copyright (c) 2002 by Kevin M. Rosenberg
 *
 * CLSQL users are granted the rights to distribute and use this software
 * as governed by the terms of the Lisp Lesser GNU Public License
 * (http://opensource.franz.com/preamble.html), also known as the LLGPL.
 ***************************************************************************/

#ifdef WIN32
#include <windows.h>

BOOL WINAPI DllEntryPoint(HINSTANCE hinstdll, DWORD fdwReason,
			  LPVOID lpvReserved)
{
        return 1;
}
       
#define DLLEXPORT __declspec(dllexport)

#else
#define DLLEXPORT 
#endif


#include <mysql.h>

/* Need to assemble a 64-bit integer to send to MySQL */
DLLEXPORT
void
clsql_mysql_data_seek (MYSQL_RES* res, unsigned int offset_high32,
		       unsigned int offset_low32)
{
  my_ulonglong offset;

  offset = offset_high32;
  offset = offset << 32;
  offset += offset_low32;
  
  mysql_data_seek (res, offset);
}

/* The following functions are used to return 64-bit integers to Lisp.
   They return the 32-bit low part and store in upper 32-bits in a 
   located sent via a pointer */

static const unsigned int bitmask_32bits = 0xFFFFFFFF;
#define lower_32bits(int64) ((unsigned int) int64 & bitmask_32bits)
#define upper_32bits(int64) ((unsigned int) (int64 >> 32))

DLLEXPORT
unsigned int
clsql_mysql_num_rows (MYSQL_RES* res, unsigned int* pHigh32)
{
  my_ulonglong nRows = mysql_num_rows (res);
  *pHigh32 = upper_32bits(nRows);
  return lower_32bits(nRows);
}

DLLEXPORT
unsigned int
clsql_mysql_affected_rows (MYSQL* res, unsigned int* pHigh32)
{
  my_ulonglong nAffected = mysql_affected_rows (res);
  *pHigh32 = upper_32bits(nAffected);
  return lower_32bits(nAffected);
}

DLLEXPORT
unsigned int
clsql_mysql_insert_id (MYSQL* mysql, unsigned int* pHigh32)
{
  my_ulonglong insert_id = mysql_insert_id (mysql);
  *pHigh32 = upper_32bits(insert_id);
  return lower_32bits(insert_id);
}


