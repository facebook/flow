// #include <stdint.h>
// #include <stdlib.h>
// #include <stdio.h>
// #include <string.h>
// #include "hh_fileinfo.h"
//
// #ifndef NO_SQLITE3
// #include <sqlite3.h>
// #endif
//
// // command used to create this table
//
// // CREATE TABLE IF NOT EXISTS NAME_INFO(
// //     HASH INTEGER PRIMARY KEY NOT NULL,
// //     NAME TEXT NOT NULL,
// //     NKIND INTEGER NOT NULL,
// //     FILESPEC TEXT NOT NULL
// // );
//
// // ignore a specific number of values
// #define UNUSED(x) \
//     ((void)(x))
// #define UNUSED1 UNUSED
// #define UNUSED2(a, b) \
//     (UNUSED(a), UNUSED(b))
// #define UNUSED3(a, b, c) \
//     (UNUSED(a), UNUSED(b), UNUSED(c))
// #define UNUSED4(a, b, c, d) \
//     (UNUSED(a), UNUSED(b), UNUSED(c), UNUSED(d))
// #define UNUSED5(a, b, c, d, e) \
//     (UNUSED(a), UNUSED(b), UNUSED(c), UNUSED(d), UNUSED(e))
//
// #define assert_sql(x, y) (assert_sql_with_line((x), (y), __LINE__))
// static void assert_sql_with_line(
//     int result,
//     int correct_result,
//     int line_number
// ) {
//     if (result == correct_result) return;
//     fprintf(stderr,
//             "SQL assertion failure: Line: %d -> Expecetd: %d, Got: %d\n",
//             line_number,
//             correct_result,
//             result);
//     abort();
// }
//
// #define assert(expr) \
//     ((expr) ? (void)(0) : \
//      ((fprintf(stderr, "%s:%d %s\n", __FILE__, __LINE__, #expr)), abort()))
//
//
// #ifdef NO_SQLITE3
//
// void hhfi_insert_row(
//         void *db,
//         int64_t hash,
//         const char *name,
//         int64_t kind,
//         const char *filespec
// ) {
//     UNUSED5(db, hash, name, kind, filespec);
//     return;
// }
//
// char *hhfi_get_filespec(
//         void *db,
//         int64_t hash
// ) {
//     UNUSED2(db, hash);
//     return NULL;
// }
//
// #else
// static const char *hhfi_insert_row_sql = \
//   "INSERT INTO NAME_INFO (HASH, NAME, NKIND, FILESPEC) VALUES (?, ?, ?, ?);";
//
// static char *copy_malloc(const char *s) {
//     char *d = malloc(1 + strlen(s));
//     assert(d);
//     return strcpy(d, s);
// }
//
// // insert a row into the name_info table
// void hhfi_insert_row(
//         void *db,
//         int64_t hash,
//         const char *name,
//         int64_t kind,
//         const char *filespec
// ) {
//     assert(db);
//     assert(name);
//     assert(filespec);
//     const char *sql = hhfi_insert_row_sql;
//     sqlite3_stmt *stmt = NULL;
//     assert_sql(sqlite3_prepare_v2(db, sql, -1, &stmt, NULL), SQLITE_OK);
//     assert_sql(sqlite3_bind_int64(stmt, 1, hash), SQLITE_OK);
//     assert_sql(sqlite3_bind_text(stmt, 2, name, -1, SQLITE_TRANSIENT),
//             SQLITE_OK);
//     assert_sql(sqlite3_bind_int64(stmt, 3, kind), SQLITE_OK);
//     assert_sql(sqlite3_bind_text(stmt, 4, filespec, -1, SQLITE_TRANSIENT),
//             SQLITE_OK);
//     assert_sql(sqlite3_step(stmt), SQLITE_DONE);
//     assert_sql(sqlite3_finalize(stmt), SQLITE_OK);
//     return;
// }
//
// static const char *hhfi_get_filespec_sql = \
//     "SELECT FILESPEC FROM NAME_INFO WHERE (HASH = (?));";
//
// char *hhfi_get_filespec(
//         void *db,
//         int64_t hash
// ) {
//     assert(db);
//     const char *sql = hhfi_get_filespec_sql;
//     sqlite3_stmt *stmt = NULL;
//     assert_sql(sqlite3_prepare_v2(db, sql, -1, &stmt, NULL), SQLITE_OK);
//     assert_sql(sqlite3_bind_int64(stmt, 1, hash), SQLITE_OK);
//     int sqlerr = sqlite3_step(stmt);
//     char *out = NULL;
//     if (sqlerr == SQLITE_DONE) {
//         // do nothing
//     } else if (sqlerr == SQLITE_ROW) {
//         // sqlite returns const unsigned char*
//         out = copy_malloc((char *) sqlite3_column_text(stmt, 0));
//         // make sure there are no more rows
//         assert_sql(sqlite3_step(stmt), SQLITE_DONE);
//     } else {
//         // unexpected sqlite status
//         assert(0);
//     }
//     sqlite3_finalize(stmt);
//     return out;
// }
// #endif
