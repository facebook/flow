/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 */
#ifndef NO_SQLITE3

#include "hh_shared_sqlite.h"

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include <sqlite3.h>

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>

#include "hh_assert.h"

#define ARRAY_SIZE(array) \
    (sizeof(array) / sizeof((array)[0]))

#define UNUSED(x) \
    ((void)(x))

#define UNUSED2(a, b) \
    (UNUSED(a), UNUSED(b))


const char *create_tables_sql[] = {
  "CREATE TABLE IF NOT EXISTS HEADER(" \
  "    MAGIC_CONSTANT INTEGER PRIMARY KEY NOT NULL," \
  "    BUILDINFO TEXT NOT NULL" \
  ");",
  "CREATE TABLE IF NOT EXISTS NAME_INFO(" \
  "    HASH INTEGER PRIMARY KEY NOT NULL," \
  "    NAME TEXT NOT NULL," \
  "    NKIND INTEGER NOT NULL," \
  "    FILESPEC TEXT NOT NULL" \
  ");",
  "CREATE TABLE IF NOT EXISTS DEPTABLE(" \
  "    KEY_VERTEX INT PRIMARY KEY NOT NULL," \
  "    VALUE_VERTEX BLOB NOT NULL" \
  ");",
};

void make_all_tables(sqlite3 *db) {
    assert(db);
    for (int i = 0; i < ARRAY_SIZE(create_tables_sql); ++i) {
        assert_sql(sqlite3_exec(db, create_tables_sql[i], NULL, 0, NULL),
                SQLITE_OK);
    }
    return;
}

void assert_sql_with_line(
  int result,
  int correct_result,
  int line_number
) {
  if (result == correct_result) return;
  fprintf(stderr,
          "SQL assertion failure: Line: %d -> Expected: %d, Got: %d\n",
          line_number,
          correct_result,
          result);
  static value *exn = NULL;
  if (!exn) exn = caml_named_value("sql_assertion_failure");
  caml_raise_with_arg(*exn, Val_long(result));
}

static const char *hhfi_insert_row_sql =                                \
  "INSERT INTO NAME_INFO (HASH, NAME, NKIND, FILESPEC) VALUES (?, ?, ?, ?);";

// insert a row into the name_info table
void hhfi_insert_row(
        sqlite3_ptr db,
        int64_t hash,
        const char *name,
        int64_t kind,
        const char *filespec
) {
    assert(db);
    assert(name);
    assert(filespec);
    const char *sql = hhfi_insert_row_sql;
    sqlite3_stmt *stmt = NULL;
    assert_sql(sqlite3_prepare_v2(db, sql, -1, &stmt, NULL), SQLITE_OK);
    assert_sql(sqlite3_bind_int64(stmt, 1, hash), SQLITE_OK);
    assert_sql(sqlite3_bind_text(stmt, 2, name, -1, SQLITE_TRANSIENT),
            SQLITE_OK);
    assert_sql(sqlite3_bind_int64(stmt, 3, kind), SQLITE_OK);
    assert_sql(sqlite3_bind_text(stmt, 4, filespec, -1, SQLITE_TRANSIENT),
            SQLITE_OK);
    assert_sql(sqlite3_step(stmt), SQLITE_DONE);
    assert_sql(sqlite3_finalize(stmt), SQLITE_OK);
    return;
}

static char *copy_malloc(const char *s) {
    char *d = malloc(1 + strlen(s));
    assert(d);
    return strcpy(d, s);
}

static sqlite3_ptr hhfi_db = NULL;

static const char *hhfi_get_filespec_sql = \
    "SELECT FILESPEC FROM NAME_INFO WHERE (HASH = (?));";

char *hhfi_get_filespec(
        sqlite3_ptr db,
        int64_t hash
) {
    assert(db);
    const char *sql = hhfi_get_filespec_sql;
    sqlite3_stmt *stmt = NULL;
    assert_sql(sqlite3_prepare_v2(db, sql, -1, &stmt, NULL), SQLITE_OK);
    assert_sql(sqlite3_bind_int64(stmt, 1, hash), SQLITE_OK);
    int sqlerr = sqlite3_step(stmt);
    char *out = NULL;
    if (sqlerr == SQLITE_DONE) {
        // do nothing
    } else if (sqlerr == SQLITE_ROW) {
        // sqlite returns const unsigned char*
        out = copy_malloc((char *) sqlite3_column_text(stmt, 0));
        // make sure there are no more rows
        assert_sql(sqlite3_step(stmt), SQLITE_DONE);
    } else {
        // unexpected sqlite status
        assert(0);
    }
    sqlite3_finalize(stmt);
    return out;
}

void hhfi_init_db(const char *path) {
    assert(hhfi_db == NULL);
    assert_sql(sqlite3_open(path, &hhfi_db), SQLITE_OK);
    assert_sql(sqlite3_exec(hhfi_db, "BEGIN TRANSACTION;", 0, 0, 0), SQLITE_OK);
    return;
}

void hhfi_free_db(void) {
    assert(hhfi_db != NULL);
    assert_sql(sqlite3_exec(hhfi_db, "END TRANSACTION;", 0, 0, 0), SQLITE_OK);
    assert_sql(sqlite3_close(hhfi_db), SQLITE_OK);
    return;
}

sqlite3_ptr hhfi_get_db(void) {
    assert(hhfi_db != NULL);
    return hhfi_db;
}

#endif /* NO_SQLITE3 */
