#include <gmodule.h>

#include "intmap_test.h"

GHashTable *_tbl;

int im_lookup(int key)
{
    gpointer p = g_hash_table_lookup(_tbl, GINT_TO_POINTER(key));
    return GPOINTER_TO_INT(p);
}

int im_put(int key, int val)
{
    int res = im_lookup(key);

    g_hash_table_insert(_tbl,
                        GINT_TO_POINTER(key),
                        GINT_TO_POINTER(val));

    return res;
}

void im_init()
{
    _tbl = g_hash_table_new(g_direct_hash, g_direct_equal);
}


int im_delete(int key)
{
    int res = im_lookup(key);
    g_hash_table_remove(_tbl, GINT_TO_POINTER(key));
    return res;
}
