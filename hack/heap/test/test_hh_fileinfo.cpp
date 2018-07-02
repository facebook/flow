#include <folly/portability/GTest.h>
#include <sqlite3.h>
#include "../hh_fileinfo.h"

#define CREATE_SQL \
  "CREATE TABLE IF NOT EXISTS NAME_INFO(" \
  "    HASH INTEGER PRIMARY KEY NOT NULL," \
  "    NAME TEXT NOT NULL," \
  "    NKIND INTEGER NOT NULL," \
  "    FILESPEC TEXT NOT NULL" \
  ");"

TEST(HhFileInfoTest, RoundTrip) {
  sqlite3 *db = nullptr;
  ASSERT_EQ(sqlite3_open(":memory:", &db), SQLITE_OK);
  ASSERT_EQ(sqlite3_exec(db, CREATE_SQL, nullptr, nullptr, nullptr), SQLITE_OK);
  hhfi_insert_row(db, 1, "name", 2, "filespec");
  auto res = hhfi_get_filespec(db, 1);
  ASSERT_STREQ(res, "filespec");
  free(res);
  ASSERT_EQ(sqlite3_close(db), SQLITE_OK);
}
