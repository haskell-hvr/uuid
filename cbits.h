

#ifndef __HASKELL_UUID_H
#define __HASKELL_UUID_H

#include <uuid/uuid.h>

int c_compare(uuid_t *a, uuid_t *b);

void c_generate(uuid_t *out);
void c_generate_time(uuid_t *out);
void c_generate_random(uuid_t *out);

int c_null(uuid_t *u);

int c_read(uuid_t *out,char *in);

void c_show(uuid_t *in, char *out);
void c_show_lower(uuid_t *in, char *out);
void c_show_upper(uuid_t *in, char *out);

int c_type(uuid_t *u);
int c_variant(uuid_t *u);

#endif //__HASKELL_UUID_H
