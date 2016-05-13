#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "bbattery.h"
#include "unif01.h"
#include "util.h"

#include "caml/mlvalues.h"
#include "caml/callback.h"

#define BUFSIZE 1000000

static int fidx = BUFSIZE;
static int bidx = BUFSIZE;

static double *fbuf;
static unsigned long *bbuf;

static double OCamlGen_U01 (void *state, void *param)
{
  if (fidx + 1> BUFSIZE){
    value f = *caml_named_value ("get_random_floats");
    value buf = caml_callback (f, Val_long (BUFSIZE));
    assert (Tag_val (buf) == Double_array_tag);
    fbuf = &Double_field (buf, 0);
    fidx = 0;
  }
  return fbuf[fidx++];
}

static unsigned long OCamlGen_Bits (void *state, void *param)
{
  unsigned long res;

  if (bidx + 2 > BUFSIZE){
    value b = *caml_named_value ("get_random_bits");
    value buf = caml_callback (b, Val_long (BUFSIZE));
    assert (Tag_val (buf) == 0);
    bbuf = (unsigned long *) &Field (buf, 0);
    bidx = 0;
  }
  res = (unsigned long) Long_val (bbuf[bidx]);
  ++ bidx;
  return res;
}

char const *generator_name = "OCaml stdlib generator";

static void WrOCamlGen (void *junk)
{
  printf ("   %s\n", generator_name);
}

unif01_Gen * unif01_CreateOCamlGen (void)
{
   unif01_Gen *gen;
   size_t len;

   gen = util_Malloc (sizeof (unif01_Gen));
   len = strlen (generator_name);
   gen->name    = util_Calloc (len + 1, sizeof (char));
   strncpy (gen->name, generator_name, len);
   gen->param   = NULL;
   gen->state   = NULL;
   gen->Write   = &WrOCamlGen;
   gen->GetBits = &OCamlGen_Bits;
   gen->GetU01  = &OCamlGen_U01;
   return gen;
}

void unif01_DeleteOCamlGen (unif01_Gen *gen)
{
   if (NULL == gen) return;
   gen->name = util_Free (gen->name);
   util_Free (gen);
}

CAMLprim value smallcrush (value unit)
{
   unif01_Gen *gen;
   gen = unif01_CreateOCamlGen ();
   bbattery_SmallCrush (gen);
   unif01_DeleteOCamlGen (gen);
   return Val_unit;
}

CAMLprim value crush (value unit)
{
   unif01_Gen *gen;
   gen = unif01_CreateOCamlGen ();
   bbattery_Crush (gen);
   unif01_DeleteOCamlGen (gen);
   return Val_unit;
}

CAMLprim value bigcrush (value unit)
{
   unif01_Gen *gen;
   gen = unif01_CreateOCamlGen ();
   bbattery_BigCrush (gen);
   unif01_DeleteOCamlGen (gen);
   return Val_unit;
}
