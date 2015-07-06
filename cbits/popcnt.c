#include <stdint.h>

void popcnt_w64_prim() {
  __asm__
    ("popcnt %%rbx, %%rbx\n\t"
     "jmp * (%%rbp)\n\t"
     :::
     );
}

int popcnt_w64(uint64_t a) {
  return __builtin_popcountll(a);
}

int popcnt_512(uint64_t *p, int64_t off) {
  p += off;
  return __builtin_popcountll(p[0]) + __builtin_popcountll(p[1])
       + __builtin_popcountll(p[2]) + __builtin_popcountll(p[3])
       + __builtin_popcountll(p[4]) + __builtin_popcountll(p[5])
       + __builtin_popcountll(p[6]) + __builtin_popcountll(p[7]);
}

// From here: http://dalkescientific.com/writings/diary/popcnt.cpp
inline
uint64_t popcnt_words0(uint64_t *buf, int64_t n) {
  uint64_t cnt = 0;
  uint64_t i;
  for (i = 0; i < (n & ~3) /* n - n % 4 */; i += 4) {
    cnt += (__builtin_popcountll(buf[i+0]) + __builtin_popcountll(buf[i+1])) +
           (__builtin_popcountll(buf[i+2]) + __builtin_popcountll(buf[i+3]));
  }
  for (; i < n; ++i) {
    cnt += __builtin_popcountll(buf[i]);
  }
  return cnt;
}

uint64_t popcnt_words(uint64_t *buf, int64_t off, int64_t n) {
  return popcnt_words0(buf + off, n);
}

uint64_t popcnt_bits(uint64_t *buf, int64_t off, int64_t n) {
  buf += off;
  uint64_t cnt = popcnt_words0(buf, n >> 6);
  int b = n & 63;
  if (b > 0) {
    cnt += __builtin_popcountll(buf[n >> 6] & ((UINT64_C(1) << b) - 1));
  }
  return cnt;
}
