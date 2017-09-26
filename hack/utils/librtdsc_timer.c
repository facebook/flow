#define CAML_NAME_SPACE
#include <stdint.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define IGNORE_UNUSED(x)  ( (void)(x) )

static inline
uint64_t
RTDSC()
{
    unsigned int high, low;
    uint64_t high1;
    uint64_t low1;

    /* perform RTDSC instruction
     * it shoves the low bits into EAX
     * and the high bits into EDX
     * we have to put them together ourselves */
    __asm__ volatile(
            "RDTSC":
            "=a" (low),
            "=d" (high)
            );
    high1 = (uint64_t) high;
    low1  = (uint64_t) low;
    high1 *= 0X100000000; /* 2^32 */
    return high1 + low1;
}


CAMLprim value
ocaml_rtdsc(value unit)
{
    IGNORE_UNUSED(unit);
    CAMLlocal1(ml_f);

    uint64_t time;
    double conv_time;

    time = RTDSC();
    conv_time = (double) time;
    ml_f = caml_copy_double(conv_time);
    return ml_f;
}
