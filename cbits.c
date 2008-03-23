#include <uuid/uuid.h>
#include "cbits.h"

int c_compare(uuid_t *a, uuid_t *b)
{
  return uuid_compare(a,b);
}


void c_generate(uuid_t *out)
{
  return uuid_generate(out);
}

void c_generate_time(uuid_t *out)
{
  return uuid_generate_time(out);
}

void c_generate_random(uuid_t *out)
{
  return uuid_generate_random(out);
}


int c_null(uuid_t *u)
{
  return uuid_is_null(u);
}


int c_read(uuid_t *out,char *in)
{
  return uuid_parse(*in, out);
}

void c_show(uuid_t *in, char *out)
{
  return uuid_unparse(in,out);
}

void c_show_lower(uuid_t *in, char *out)
{
  return uuid_unparse_lower(in,out);
}

void c_show_upper(uuid_t *in, char *out)
{
  return uuid_unparse_upper(in,out);
}


int c_type(uuid_t *u)
{
  return uuid_type(u);
}

int c_variant(uuid_t *u)
{
  return uuid_variant(u);
}
