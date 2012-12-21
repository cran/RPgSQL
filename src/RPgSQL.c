/* -*- C -*-
 * $RCSfile: RPgSQL.c,v $
 * $Date: 2000/01/08 20:09:24 $
 * $Revision: 1.1.1.1 $
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
rpgsql_status(int* status) {
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
  /* This is necessary to avoid a core dump when passing a null
     pointer */
  if (*name == NULL) *name = "";
  return;
}

void
rpgsql_host_name(char **name) {
  if (connection != NULL) *name = PQhost(connection);
  /* This is necessary to avoid a core dump when passing a null
     pointer */
  if (*name == NULL) *name = "";
  return;
}

void
rpgsql_finish(void) {
  /* Disconnect from backend */
  if (connection != NULL) {
    PQfinish(connection);
    connection = NULL;
  }
  return;
}

void
rpgsql_exec(const char **query) {
  if (echo==TRUE) printf("%s\n", *query);
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
rpgsql_ntuples(int *ntuples) {
  if (result != NULL) *ntuples = PQntuples(result);
  return;
}

void
rpgsql_nfields(int *nfields) {
  if (result != NULL) *nfields = PQnfields(result);
  return;
}

void
rpgsql_fname(int * const index , char **fname) {
  /* libpq uses 0-based indexing */
  if (result != NULL) *fname = PQfname(result, --(*index));
  return;
}

void
rpgsql_fnumber(const char ** fname, int *index) {
  if (result != NULL) *index = PQfnumber(result, *fname);
  return;
}

void
rpgsql_ftype(int * const index, int *type) {
  /* libpq uses 0-based indexing */
  if (result != NULL) *type = PQftype(result, --(*index));
  return;
}

void
rpgsql_fsize(int * const index, int *size) {
  /* libpq uses 0-based indexing */
  if (result != NULL) *size = PQfsize(result, --(*index));
  return;
}

void
rpgsql_get_value(int * const tuple, int * const field, char **value) {
  /* libpq uses 0-based indexing */
  const int t = *tuple - 1;
  const int f = *field - 1;
  if (result != NULL) {
    /* libpq sometimes dumps core when requests are out of bounds */
    if (t < 0 ||
	f < 0 ||
	t > PQntuples(result) - 1 ||
	f > PQnfields(result) - 1) {
      fprintf(stderr, "Warning: request for data out of bounds\n");
    } else {
      *value = PQgetvalue(result, t, f);
    }
  }
  return;
}

/* These can be used when returning binary data */
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

void
rpgsql_get_length(int * const tuple, int * const field, int *length) {
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
