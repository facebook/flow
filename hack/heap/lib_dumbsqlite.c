#ifdef NO_SQLITE3
// nothing
#else
#include <sqlite3.h>
#endif
#include <stddef.h>
#include <inttypes.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#define SQLITE_DBS 100
#define SQLITE_STMTS 1000

#define ERR_SLOT_TAKEN -100
#define ERR_INDEX_OUT_OF_RANGE -101
#define ERR_NULL_PTR -102
#define ERR_INTERNAL -103
#define ERR_DB_NOT_PRESENT -104
#define ERR_STMT_INDEX_OUT_OF_RANGE -105
#define ERR_STMT_SLOT_TAKEN -106
#define ERR_STMT_NOT_PRESENT -107
#define ERR_DB_ALLOC_FAILED -108
#define ERR_PARAM_OUT_OF_RANGE -109
#define ERR_COLUMN_OUT_OF_RANGE -110
#define ERR_GOT_NULL_TEXT_COLUMN -111
#define ERR_WOULD_CLOBBER -112
#define ERR_NO_SQLITE3 -113

#ifdef NO_SQLITE3
// nothing
#else
sqlite3 *dumb_sqlite_dbs[SQLITE_DBS] = { 0 };
sqlite3_stmt *dumb_sqlite_stmts[SQLITE_STMTS] = { 0 };
#endif


#ifdef NO_SQLITE3
int dumb_sqlite_open(int index, const char *path, int readonly)
{
   return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_open(int index, const char *path, int readonly)
{
    if (index < 0)
        return ERR_INDEX_OUT_OF_RANGE;
    if (index >= SQLITE_DBS)
        return ERR_INDEX_OUT_OF_RANGE;
    if (path == NULL)
        return ERR_NULL_PTR;
    if (dumb_sqlite_dbs[index] != NULL)
        return ERR_SLOT_TAKEN;
    int sqlerr = ERR_INTERNAL;
    if (readonly) {
        sqlerr = sqlite3_open_v2(
            path,
            &(dumb_sqlite_dbs[index]),
            SQLITE_OPEN_READONLY,
            NULL
        );
    } else {
        sqlerr = sqlite3_open(
            path,
            &(dumb_sqlite_dbs[index])
        );
    }
    if (dumb_sqlite_dbs[index] == NULL) {
        return ERR_DB_ALLOC_FAILED;
    }
    return sqlerr;
}
#endif //NO_SQLITE3


CAMLprim value caml_dumb_sqlite_open(
        value ml_index, value ml_path, value ml_readonly
) {
    CAMLparam3(ml_index, ml_path, ml_readonly);
    CAMLreturn(
        Val_int(
            dumb_sqlite_open(
                Int_val(ml_index),
                String_val(ml_path),
                Bool_val(ml_readonly)
            )
        )
    );
}


#ifdef NO_SQLITE3
int dumb_sqlite_close(int index) {
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_close(int index) {
    if (index < 0)
        return ERR_INDEX_OUT_OF_RANGE;
    if (index >= SQLITE_DBS)
        return ERR_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_dbs[index] == NULL)
        return ERR_DB_NOT_PRESENT;
    int sqlerr = sqlite3_close(dumb_sqlite_dbs[index]);
    if (sqlerr == SQLITE_OK) {
        dumb_sqlite_dbs[index] = NULL;
    }
    return sqlerr;
}
#endif //NO_SQLITE3


CAMLprim value caml_dumb_sqlite_close(
        value ml_index
) {
    CAMLparam1(ml_index);
    CAMLreturn(
        Val_int(
            dumb_sqlite_close(
                Int_val(ml_index)
            )
        )
    );
}

#ifdef NO_SQLITE3
int dumb_sqlite_prepare(int index, int s_index, const char *sql)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_prepare(int index, int s_index, const char *sql)
{
    if (index < 0)
        return ERR_INDEX_OUT_OF_RANGE;
    if (index >= SQLITE_DBS)
        return ERR_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_dbs[index] == NULL)
        return ERR_DB_NOT_PRESENT;
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] != NULL)
        return ERR_STMT_SLOT_TAKEN;
    if (sql == NULL)
        return ERR_NULL_PTR;
    int sqlerr = sqlite3_prepare_v2(
        dumb_sqlite_dbs[index],
        sql,
        -1,
        &(dumb_sqlite_stmts[s_index]),
        NULL
    );
    return sqlerr;
}
#endif


CAMLprim value caml_dumb_sqlite_prepare(
        value ml_index, value ml_s_index, value ml_sql
) {
    CAMLparam3(ml_index, ml_s_index, ml_sql);
    CAMLreturn(
        Val_int(
            dumb_sqlite_prepare(
                Int_val(ml_index),
                Int_val(ml_s_index),
                String_val(ml_sql)
            )
        )
    );
}

#ifdef NO_SQLITE3
int dumb_sqlite_reset(int s_index)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_reset(int s_index)
{
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] == NULL)
        return ERR_STMT_NOT_PRESENT;
    int sqlerr = sqlite3_reset(
        dumb_sqlite_stmts[s_index]
    );
    return sqlerr;
}
#endif


CAMLprim value caml_dumb_sqlite_reset(
        value ml_s_index
) {
    CAMLparam1(ml_s_index);
    CAMLreturn(
        Val_int(
            dumb_sqlite_reset(
                Int_val(ml_s_index)
            )
        )
    );
}


#ifdef NO_SQLITE3
int dumb_sqlite_finalize(int s_index)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_finalize(int s_index)
{
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] == NULL)
        return ERR_STMT_NOT_PRESENT;
    int sqlerr = sqlite3_finalize(
        dumb_sqlite_stmts[s_index]
    );
    if (sqlerr == SQLITE_OK) {
      dumb_sqlite_stmts[s_index] = NULL;
    }
    return sqlerr;
}
#endif


CAMLprim value caml_dumb_sqlite_finalize(
        value ml_s_index
) {
    CAMLparam1(ml_s_index);
    CAMLreturn(
        Val_int(
            dumb_sqlite_finalize(
                Int_val(ml_s_index)
            )
        )
    );
}


#ifdef NO_SQLITE3
int dumb_sqlite_step(int s_index)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_step(int s_index)
{
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] == NULL)
        return ERR_STMT_NOT_PRESENT;
    int sqlerr = sqlite3_step(
        dumb_sqlite_stmts[s_index]
    );
    return sqlerr;
}
#endif


CAMLprim value caml_dumb_sqlite_step(
        value ml_s_index
) {
    CAMLparam1(ml_s_index);
    CAMLreturn(
        Val_int(
            dumb_sqlite_step(
                Int_val(ml_s_index)
            )
        )
    );
}


#ifdef NO_SQLITE3
int dumb_sqlite_bind_int64(int s_index, int param, int64_t value_)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_bind_int64(int s_index, int param, int64_t value_)
{
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] == NULL)
        return ERR_STMT_NOT_PRESENT;
    if (param < 1)
        return ERR_PARAM_OUT_OF_RANGE;
    int sqlerr = sqlite3_bind_int64(
        dumb_sqlite_stmts[s_index],
        param,
        value_
    );
    return sqlerr;
}
#endif


CAMLprim value caml_dumb_sqlite_bind_int64(
    value ml_s_index, value ml_param, value ml_value
) {
    CAMLparam3(ml_s_index, ml_param, ml_value);
    CAMLreturn(
        Val_int(
            dumb_sqlite_bind_int64(
                Int64_val(ml_s_index),
                Int_val(ml_param),
                Long_val(ml_value)
            )
        )
    );
}


#ifdef NO_SQLITE3
int dumb_sqlite_bind_text(int s_index, int param, const char *value_)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_bind_text(int s_index, int param, const char *value_)
{
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] == NULL)
        return ERR_STMT_NOT_PRESENT;
    if (param < 1)
        return ERR_PARAM_OUT_OF_RANGE;
    int sqlerr = sqlite3_bind_text(
        dumb_sqlite_stmts[s_index],
        param,
        value_,
        -1,
        SQLITE_TRANSIENT
    );
    return sqlerr;
}
#endif


CAMLprim value caml_dumb_sqlite_bind_text(
    value ml_s_index, value ml_param, value ml_value
)
{
    CAMLparam3(ml_s_index, ml_param, ml_value);
    CAMLreturn(
        Val_int(
            dumb_sqlite_bind_text(
                Int_val(ml_s_index),
                Int_val(ml_param),
                String_val(ml_value)
            )
        )
    );
}

#ifdef NO_SQLITE3
int dumb_sqlite_column_int64(int s_index, int column, /*out*/ int64_t *out_value)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_column_int64(int s_index, int column, /*out*/ int64_t *out_value)
{
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] == NULL)
        return ERR_STMT_NOT_PRESENT;
    if (column < 0)
        return ERR_COLUMN_OUT_OF_RANGE;
    if (out_value == NULL)
        return ERR_NULL_PTR;
    int64_t res = sqlite3_column_int64(
        dumb_sqlite_stmts[s_index],
        column
    );
    *out_value = res;
    return SQLITE_OK;
}
#endif


CAMLprim value caml_dumb_sqlite_column_int64(
    value ml_s_index, value ml_column
) {
    CAMLparam2(ml_s_index, ml_column);
    CAMLlocal3(ml_pair, ml_first, ml_second);
    int64_t out = 0;
    ml_first =
        Val_int(
            dumb_sqlite_column_int64(
                Int_val(ml_s_index),
                Int_val(ml_column),
                &out
            )
        );
    ml_second = caml_copy_int64(out);
    ml_pair = caml_alloc_tuple(2);
    Store_field(ml_pair, 0, ml_first);
    Store_field(ml_pair, 1, ml_second);
    CAMLreturn(ml_pair);
}

#ifdef NO_SQLITE3
int dumb_sqlite_column_text(int s_index, int column, /*out*/ char **out_value)
{
    return ERR_NO_SQLITE3;
}
#else
int dumb_sqlite_column_text(int s_index, int column, /*out*/ char **out_value)
{
    if (s_index < 0)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (s_index >= SQLITE_DBS)
        return ERR_STMT_INDEX_OUT_OF_RANGE;
    if (dumb_sqlite_stmts[s_index] == NULL)
        return ERR_STMT_NOT_PRESENT;
    if (column < 0)
        return ERR_COLUMN_OUT_OF_RANGE;
    if (out_value == NULL)
        return ERR_NULL_PTR;
    if (*out_value != NULL)
        return ERR_WOULD_CLOBBER;
    char *res = (char *) sqlite3_column_text(
        dumb_sqlite_stmts[s_index],
        column
    );
    *out_value = res;
    return SQLITE_OK;
}
#endif


CAMLprim value caml_dumb_sqlite_column_text(
    value ml_s_index, value ml_column
) {
    CAMLparam2(ml_s_index, ml_column);
    CAMLlocal3(ml_pair, ml_first, ml_second);
    char *out = NULL;
    ml_first =
        Val_int(
            dumb_sqlite_column_text(
                Int_val(ml_s_index),
                Int_val(ml_column),
                &out
            )
        );
    if (out == NULL) {
        out = "";
    }
    ml_second = caml_copy_string(out);
    ml_pair = caml_alloc_tuple(2);
    Store_field(ml_pair, 0, ml_first);
    Store_field(ml_pair, 1, ml_second);
    CAMLreturn(ml_pair);
}
