#include <stdint.h>
#include <xmmintrin.h>

uint64_t reverseBytes(uint64_t value) {
  return __builtin_bswap64(value);
}

unsigned long bsf64(const uint64_t mask) {
  unsigned long index;
  __asm__("bsf %[mask], %[index]" : [index] "=r" (index) : [mask] "mr" (mask));
  return index;
}

unsigned long bsr64(const uint64_t mask) {
  unsigned long index;
  __asm__("bsr %[mask], %[index]" : [index] "=r" (index) : [mask] "mr" (mask));
  return index;
}

void prefetch(const char * ptr) {
  _mm_prefetch(ptr, _MM_HINT_T0);
}
