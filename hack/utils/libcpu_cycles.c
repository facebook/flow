#define CAML_NAME_SPACE
#include <stdint.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define IGNORE_UNUSED(x)  ( (void)(x) )

static inline
uint64_t
CPU_CYCLES()
{
#ifdef __x86_64__
    uint32_t high, low;
    uint64_t high1, low1;
    asm volatile("rdtsc": "=a"((low)), "=d"((high)));
    high1 = (uint64_t) high;
    low1 = (uint64_t) low;
    return low1 | (high1 * 0x100000000 /* 2^32 */);
#elif __powerpc64__
    uint64_t tb;
    asm volatile("mfspr %0, 268" : "=r"((tb)));
    return tb;
#elif _MSC_VER
    return (uint64_t) __rstdc();
#elif __aarch64__
    uint64_t arm_tb;
    asm volatile("mrs %0, cntvct_el0" : "=r"((arm_tb)));
    return arm_tb;
#else
    return 0;
    /* default value when not implemented, don't want the complexity of
                 throwing from OCaml */
#endif
}

CAMLprim value
ocaml_cpu_cycles(value unit)
{
    IGNORE_UNUSED(unit);
    return Val_int(CPU_CYCLES());
}
