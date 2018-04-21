gcc -shared `pkg-config --cflags glib-2.0` intmap_test.c `pkg-config --libs glib-2.0` -o libintmap.so
