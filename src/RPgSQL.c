/* -*- C -*-
 * $RCSfile: RPgSQL.c,v $
 * $Date: 2001/06/20 19:46:09 $
 * $Revision: 1.7 $
 * Copyright (C) 1999 Timothy H. Keitt
 */

#include <string.h>
#include <libpq-fe.h>

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE (!FALSE)
#endif

/* The current model limits you to one database connection at a time.
   The upside is that you don't have to manage connection and result
   pointers in R, which would be rather a pain. */
static PGconn *connection = NULL;
static PGresult *result = NULL;
static int echo = FALSE;

void
rpgsql_connect(const char **conninfo) {
  if (connection != NULL) PQfinish(connection);
  connection = PQconnectdb(*conninfo);
  return;
}

void
rpgsql_connection_status(int* status) {
  if (connection == NULL) {
    *status = CONNECTION_BAD;
  } else {
    *status = PQstatus(connection);
  }
  return;
}

void
rpgsql_error_message(char **message) {
  if (connection != NULL) *message = PQerrorMessage(connection);
  if (*message == NULL) *message = "";
  return;
}

void
rpgsql_db_name(char **name) {
  if (connection != NULL) *name = PQdb(connection);
  if (*name == NULL) *name = "";
  return;
}

void
rpgsql_host_name(char **name) {
  if (connection != NULL) *name = PQhost(connection);
  if (*name == NULL) *name = "";
  return;
}

rpgsql_db_options(char **options) {
  if (connection != NULL) *options = PQoptions(connection);
  if (*options == NULL) *options = "";
  return;
}


rpgsql_user_name(char **name) {
  if (connection != NULL) *name = PQuser(connection);
  if (*name == NULL) *name = "";
  return;
}

void
rpgsql_password(char **password) {
  if (connection != NULL) *password = PQpass(connection);
  if (*password == NULL) *password = "";
  return;
}

void
rpgsql_port(char **port) {
  if (connection != NULL) *port = PQport(connection);
  if (*port == NULL) *port = "";
  return;
}

void
rpgsql_tty(char **tty) {
  if (connection != NULL) *tty = PQtty(connection);
  if (*tty == NULL) *tty = "";
  return;
}

int
rpgsql_pid() {
  if (connection != NULL) return  PQbackendPID(connection);
  return;
}

void
rpgsql_connection_finish(void) {
  /* Disconnect from backend */
  if (connection != NULL) {
    PQfinish(connection);
    connection = NULL;
  }
  return;
}

void
rpgsql_exec(const char **query) {
  if (echo==TRUE) Rprintf("%s\n", *query);
  /* Free previous results */
  if (result != NULL) PQclear(result);
  /* Query backend */
  result = PQexec(connection, *query);
  return;
}

void
rpgsql_result_status(int *status) {
  if (result == NULL) {
    *status = PGRES_NONFATAL_ERROR;
  } else {
    *status = PQresultStatus(result);
  }
  return;
}

void
rpgsql_result_ntuples(int *ntuples) {
  if (result != NULL) *ntuples = PQntuples(result);
  return;
}

void
rpgsql_result_nfields(int *nfields) {
  if (result != NULL) *nfields = PQnfields(result);
  return;
}

void
rpgsql_result_fname(int * const index , char **fname) {
  /* libpq uses 0-based indexing */
  if (result != NULL) *fname = PQfname(result, --(*index));
  if (*fname == NULL) *fname = "";
  return;
}

void
rpgsql_result_fnumber(const char ** fname, int *index) {
  if (result != NULL) *index = PQfnumber(result, *fname);
  return;
}

void
rpgsql_result_ftype(int * const index, int *type) {
  if (result != NULL) *type = PQftype(result, --(*index));
  return;
}

void
rpgsql_result_fsize(int * const index, int *size) {
  if (result != NULL) *size = PQfsize(result, --(*index));
  return;
}

void
rpgsql_get_value(int * const tuple, int * const field, char **value) {
  const int t = *tuple - 1;
  const int f = *field - 1;
  if (result != NULL) {
#ifdef RPGSQL_BOUNDS_CHECKING
    if (t < 0 ||
	f < 0 ||
	t > PQntuples(result) - 1 ||
	f > PQnfields(result) - 1) {
      fprintf(stderr, "Warning: request for data out of bounds\n");
      return;
    }
#endif
    *value = PQgetvalue(result, t, f);
  }
  return;
}

/* These can be used when returning binary data
void
rpgsql_get_int_value(int * const tuple, int * const field, int *value) {
  if (result != NULL) 
    value = (int *) PQgetvalue(result, --(*tuple), --(*field));
  return;
}

void
rpgsql_get_double_value(int * const tuple, int * const field, double *value) {
  if (result != NULL) 
    value = (double *) PQgetvalue(result, --(*tuple), --(*field));
  return;
}
*/

void
rpgsql_get_field_length(int * const tuple, int * const field,
			int *length) {
  if (result != NULL)
    *length = PQgetlength(result, --(*tuple), --(*field));
  return;
}

void
rpgsql_clear_result(void) {
  if (result != NULL) PQclear(result);
  result = NULL;
  return;
}

void
rpgsql_get_is_null(int * const tuple, int * const field, int *flag) {
  if (result != NULL)
    *flag = PQgetisnull(result, --(*tuple), --(*field));
  return;
}

void
rpgsql_toggle_echo(void) {
  if (echo)
    echo = FALSE;
  else
    echo = TRUE;
  return;
}

