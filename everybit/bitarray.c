/**
 * Copyright (c) 2012 MIT License by 6.172 Staff
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 **/

 // Implements the ADT specified in bitarray.h as a packed array of bits; a bit
 // array containing bit_sz bits will consume roughly bit_sz/8 bytes of
 // memory.


#include "./bitarray.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <sys/types.h>


// ********************************* Types **********************************

// Concrete data type representing an array of bits.
struct bitarray {
  // The number of bits represented by this bit array.
  // Need not be divisible by 8.
  size_t bit_sz;

  // The underlying memory buffer that stores the bits in
  // packed form (8 per byte).
  char* buf;
};


// ******************** Prototypes for static functions *********************

// Rotates a subarray left by an arbitrary number of bits.
//
// bit_offset is the index of the start of the subarray
// bit_length is the length of the subarray, in bits
// bit_left_amount is the number of places to rotate the
//                    subarray left
//
// The subarray spans the half-open interval
// [bit_offset, bit_offset + bit_length)
// That is, the start is inclusive, but the end is exclusive.
/**
 * @brief 左旋转
 *
 * @param bitarray 旋转数组
 * @param bit_offset 子数组开始索引
 * @param bit_length 子数组长度 单位是位
 * @param bit_left_amount 旋转多少位
 *
 * 子数组范围：[bit_offset, bit_offset + bit_length)
 */
static void bitarray_rotate_left (bitarray_t* const bitarray,
  const size_t bit_offset,
  const size_t bit_length,
  const size_t bit_left_amount);

// Rotates a subarray left by one bit.
//
// bit_offset is the index of the start of the subarray
// bit_length is the length of the subarray, in bits
//
// The subarray spans the half-open interval
// [bit_offset, bit_offset + bit_length)
// That is, the start is inclusive, but the end is exclusive.
/**
 * @brief 左旋一位
 *
 * @param bitarray
 * @param bit_offset
 * @param bit_length
 */
static void bitarray_rotate_left_one (bitarray_t* const bitarray,
  const size_t bit_offset,
  const size_t bit_length);

// Portable modulo operation that supports negative dividends.
//
// Many programming languages define modulo in a manner incompatible with its
// widely-accepted mathematical definition.
// http://stackoverflow.com/questions/1907565/c-python-different-behaviour-of-the-modulo-operation
// provides details; in particular, C's modulo
// operator (which the standard calls a "remainder" operator) yields a result
// signed identically to the dividend e.g., -1 % 10 yields -1.
// This is obviously unacceptable for a function which returns size_t, so we
// define our own.
//
// n is the dividend and m is the divisor
//
// Returns a positive integer r = n (mod m), in the range
// 0 <= r < m.
/**
 * @brief 重新定义模运算 返回正数
 *
 * @param n
 * @param m
 * @return size_t
 */
static size_t modulo (const ssize_t n, const size_t m);

// Produces a mask which, when ANDed with a byte, retains only the
// bit_index th byte.
//
// Example: bitmask(5) produces the byte 0b00100000.
//
// (Note that here the index is counted from right
// to left, which is different from how we represent bitarrays in the
// tests.  This function is only used by bitarray_get and bitarray_set,
// however, so as long as you always use bitarray_get and bitarray_set
// to access bits in your bitarray, this reverse representation should
// not matter.
/**
 * @brief 返回一个bit掩码 仅保留bit_index这一位的值为1 其余为0 （bit_index是从右向左计数的）
 *
 * @param bit_index
 * @return char
 */
static char bitmask (const size_t bit_index);

/**
 * @brief bit串以翻转的方式旋转
 *
 * @param bitarray bit串
 * @param bit_offset 子串起始位置索引
 * @param bit_length 子串长度
 * @param right_count 右旋位数
 */
static void bitarray_rotate_reverse (bitarray_t* const bitarray, const size_t bit_offset, const size_t bit_length, const size_t right_count);



// ******************************* Functions ********************************

bitarray_t* bitarray_new (const size_t bit_sz)
{
  // Allocate an underlying buffer of ceil(bit_sz/8) bytes.
  char* const buf = calloc (1, (bit_sz + 7) / 8);
  if(buf == NULL) {
    return NULL;
  }

  // Allocate space for the struct.
  bitarray_t* const bitarray = malloc (sizeof (struct bitarray));
  if(bitarray == NULL) {
    free (buf);
    return NULL;
  }

  bitarray->buf = buf;
  bitarray->bit_sz = bit_sz;
  return bitarray;
}

void bitarray_free (bitarray_t* const bitarray)
{
  if(bitarray == NULL) {
    return;
  }
  free (bitarray->buf);
  bitarray->buf = NULL;
  free (bitarray);
}

size_t bitarray_get_bit_sz (const bitarray_t* const bitarray)
{
  return bitarray->bit_sz;
}

bool bitarray_get (const bitarray_t* const bitarray, const size_t bit_index)
{
  assert (bit_index < bitarray->bit_sz);

  // We're storing bits in packed form, 8 per byte.  So to get the nth
  // bit, we want to look at the (n mod 8)th bit of the (floor(n/8)th)
  // byte.
  //
  // In C, integer division is floored explicitly, so we can just do it to
  // get the byte; we then bitwise-and the byte with an appropriate mask
  // to produce either a zero byte (if the bit was 0) or a nonzero byte
  // (if it wasn't).  Finally, we convert that to a boolean.

  // 例子
  // index:1    index:0
  // 0110 1100 0111 1100
  // bit_index = 10 => buf[1] == 0110 1100
  // bitmask = 0000 0100
  // 0110 1100
  // 0000 0100
  // 0000 0100

  return (bitarray->buf[bit_index / 8] & bitmask (bit_index)) ?
    true : false;
}

void bitarray_set (bitarray_t* const bitarray,
  const size_t bit_index,
  const bool value)
{
  assert (bit_index < bitarray->bit_sz);

  // We're storing bits in packed form, 8 per byte.  So to set the nth
  // bit, we want to set the (n mod 8)th bit of the (floor(n/8)th) byte.
  //
  // In C, integer division is floored explicitly, so we can just do it to
  // get the byte; we then bitwise-and the byte with an appropriate mask
  // to clear out the bit we're about to set.  We bitwise-or the result
  // with a byte that has either a 1 or a 0 in the correct place.

  // 1.先清除要修改的位
  // 2.根据value的值，利用bitmask对位进行修改

  bitarray->buf[bit_index / 8] =
    (bitarray->buf[bit_index / 8] & ~bitmask (bit_index)) |
    (value ? bitmask (bit_index) : 0);
}

void bitarray_randfill (bitarray_t* const bitarray)
{
  int32_t* ptr = (int32_t*)bitarray->buf;
  for(int64_t i = 0; i < bitarray->bit_sz / 32 + 1; i++) {
    ptr[i] = rand ();
  }
}

void bitarray_rotate (bitarray_t* const bitarray,
  const size_t bit_offset,
  const size_t bit_length,
  const ssize_t bit_right_amount)
{
  assert (bit_offset + bit_length <= bitarray->bit_sz);

  if(bit_length == 0) {
    return;
  }

  // Convert a rotate left or right to a left rotate only, and eliminate
  // multiple full rotations.
  // 把右旋和左旋都转化成了来处理了
  // bitarray_rotate_left (bitarray, bit_offset, bit_length,
  //   modulo (-bit_right_amount, bit_length));

  // 使用优化后的函数
  size_t a = modulo (-bit_right_amount, bit_length);
  if(a == 0) return;
  bitarray_rotate_reverse (bitarray, bit_offset, bit_length, a);
}

static void bitarray_rotate_left (bitarray_t* const bitarray,
  const size_t bit_offset,
  const size_t bit_length,
  const size_t bit_left_amount)
{
  for(size_t i = 0; i < bit_left_amount; i++) {
    bitarray_rotate_left_one (bitarray, bit_offset, bit_length);
  }
}

/**
 * @brief 将目标子串左移一位 然后放到这个子串的最后
 *
 * @param bitarray 父串
 * @param bit_offset 子串起始索引
 * @param bit_length 子串长度
 * 
 * 1100 1000 
 * 1000 1001
 */

static void bitarray_rotate_left_one (bitarray_t* const bitarray,
  const size_t bit_offset,
  const size_t bit_length)
{
  // Grab the first bit in the range, shift everything left by one, and
  // then stick the first bit at the end.
  const bool first_bit = bitarray_get (bitarray, bit_offset);
  size_t i;
  for(i = bit_offset; i + 1 < bit_offset + bit_length; i++) {
    bitarray_set (bitarray, i, bitarray_get (bitarray, i + 1));
  }
  bitarray_set (bitarray, i, first_bit);
}

// n % m
// -1 % 8 + 8  = 7 % 8 = 7
static size_t modulo (const ssize_t n, const size_t m)
{
  const ssize_t signed_m = (ssize_t)m;
  assert (signed_m > 0);
  const ssize_t result = ((n % signed_m) + signed_m) % signed_m; // 转成正数
  assert (result >= 0);
  return (size_t)result;
}


// eg : bit_index = 10
//    bitmask = 1 << (10 % 8）== 0000 0100
// 这里用静态数组直接做映射会更快

// bit_index = 0
// mask ==  0000 0001
// 
static char bitmask (const size_t bit_index)
{
  static const unsigned char mask[8] = { 1,2,4,8,16,32,64,128 };
  return mask[bit_index % 8];
}
  




// ******************************* 优化函数 ********************************

// 核心思路
// ab -> ba 其中a与b都是一个bit串
// 1. 反转 a->a1, 反转 b->b1得到 a1b1
// 2. 反转 a1b1 -> ba

// 

// 如何反转？
// 以字节为单位反转
// 只需要将256个char字节 分别映射成 对应的反转字节即可
// 静态数组，缓存友好、省去了计算的性能损耗

/* 存储每个字节对应的反转字节 */
// 0000 0000 - 00
// 0000 0001 - 1000 0000
//               8    0
// 0000 0010 - 0100 0000
//               4     0
static const unsigned char reverse_byte_map[256] = {
0x00, 0x80, 0x40, 0xC0, 0x20, 0xA0, 0x60, 0xE0, 0x10, 0x90, 0x50, 0xD0, 0x30, 0xB0, 0x70, 0xF0,
0x08, 0x88, 0x48, 0xC8, 0x28, 0xA8, 0x68, 0xE8, 0x18, 0x98, 0x58, 0xD8, 0x38, 0xB8, 0x78, 0xF8,
0x04, 0x84, 0x44, 0xC4, 0x24, 0xA4, 0x64, 0xE4, 0x14, 0x94, 0x54, 0xD4, 0x34, 0xB4, 0x74, 0xF4,
0x0C, 0x8C, 0x4C, 0xCC, 0x2C, 0xAC, 0x6C, 0xEC, 0x1C, 0x9C, 0x5C, 0xDC, 0x3C, 0xBC, 0x7C, 0xFC,
0x02, 0x82, 0x42, 0xC2, 0x22, 0xA2, 0x62, 0xE2, 0x12, 0x92, 0x52, 0xD2, 0x32, 0xB2, 0x72, 0xF2,
0x0A, 0x8A, 0x4A, 0xCA, 0x2A, 0xAA, 0x6A, 0xEA, 0x1A, 0x9A, 0x5A, 0xDA, 0x3A, 0xBA, 0x7A, 0xFA,
0x06, 0x86, 0x46, 0xC6, 0x26, 0xA6, 0x66, 0xE6, 0x16, 0x96, 0x56, 0xD6, 0x36, 0xB6, 0x76, 0xF6,
0x0E, 0x8E, 0x4E, 0xCE, 0x2E, 0xAE, 0x6E, 0xEE, 0x1E, 0x9E, 0x5E, 0xDE, 0x3E, 0xBE, 0x7E, 0xFE,
0x01, 0x81, 0x41, 0xC1, 0x21, 0xA1, 0x61, 0xE1, 0x11, 0x91, 0x51, 0xD1, 0x31, 0xB1, 0x71, 0xF1,
0x09, 0x89, 0x49, 0xC9, 0x29, 0xA9, 0x69, 0xE9, 0x19, 0x99, 0x59, 0xD9, 0x39, 0xB9, 0x79, 0xF9,
0x05, 0x85, 0x45, 0xC5, 0x25, 0xA5, 0x65, 0xE5, 0x15, 0x95, 0x55, 0xD5, 0x35, 0xB5, 0x75, 0xF5,
0x0D, 0x8D, 0x4D, 0xCD, 0x2D, 0xAD, 0x6D, 0xED, 0x1D, 0x9D, 0x5D, 0xDD, 0x3D, 0xBD, 0x7D, 0xFD,
0x03, 0x83, 0x43, 0xC3, 0x23, 0xA3, 0x63, 0xE3, 0x13, 0x93, 0x53, 0xD3, 0x33, 0xB3, 0x73, 0xF3,
0x0B, 0x8B, 0x4B, 0xCB, 0x2B, 0xAB, 0x6B, 0xEB, 0x1B, 0x9B, 0x5B, 0xDB, 0x3B, 0xBB, 0x7B, 0xFB,
0x07, 0x87, 0x47, 0xC7, 0x27, 0xA7, 0x67, 0xE7, 0x17, 0x97, 0x57, 0xD7, 0x37, 0xB7, 0x77, 0xF7,
0x0F, 0x8F, 0x4F, 0xCF, 0x2F, 0xAF, 0x6F, 0xEF, 0x1F, 0x9F, 0x5F, 0xDF, 0x3F, 0xBF, 0x7F, 0xFF
};

/**
 * @brief 获取目标字节的反转字节
 *
 * @param byte 反转字节的源字节
 * @return unsigned char
 */
unsigned char get_reverse_byte (unsigned char byte)
{
  return (unsigned char)reverse_byte_map[byte];
}

/* 获取bitarray中的某个字节 */
unsigned char bitarray_get_byte (const bitarray_t* const  bitarray, const size_t index)
{
  assert (index < bitarray->bit_sz);
  return  (unsigned char)bitarray->buf[index];
}

void bitarray_set_byte (const bitarray_t* const bitarray, const size_t index, const unsigned char byte)
{
  assert (index < bitarray->bit_sz);
  bitarray->buf[index] = (char)byte;
}


/**
 * @brief 以字节为单位翻转目标索引区间内的所有字节
 * 
 * @param bitarray 原bit串
 * @param start_byte_index 开始索引
 * @param end_byte_index 结束索引
 */
void bitarray_reverse_byByte (bitarray_t* const bitarray, const size_t start_byte_index, const size_t end_byte_index)
{
  // 中点位置索引
  size_t mid_index = (start_byte_index + end_byte_index) / 2;
  size_t temp_index;
  unsigned char temp;
  // 先不考虑多出来的bit 直接以字节为单位翻转所有待翻转区间内的所有字节
  for(size_t i = start_byte_index; i <= mid_index; i++) {
    // i-a  j = b-(i-a)
    temp_index = end_byte_index + start_byte_index - i; // 对称位置的索引
    temp = get_reverse_byte (bitarray_get_byte (bitarray, temp_index));
    bitarray_set_byte (bitarray, temp_index, get_reverse_byte (bitarray_get_byte (bitarray, i)));
    bitarray_set_byte (bitarray, i, temp);
  }
}

/**
 * @brief 对某一个子串进行反转
 *
 * @param bitarray 原来串
 * @param bit_offset 子串起始索引
 * @param bit_length 子串长度
 */
void subarray_rotate_reverse (bitarray_t* const bitarray, const size_t bit_offset, const size_t bit_length)
{
  assert (bit_offset + bit_length <= bitarray->bit_sz);
  if(bit_length < 2) return;

  // 计算起始字节和终止字节的索引
  size_t start_byte_index = bit_offset / 8;
  size_t end_bit_index = bit_offset + bit_length - 1;
  size_t end_byte_index = end_bit_index / 8;

  // 获取第一个和最后一个字节的反转字节
  unsigned char first_byte_reverse = get_reverse_byte (bitarray_get_byte (bitarray, start_byte_index));
  unsigned char last_byte_reverse = get_reverse_byte (bitarray_get_byte (bitarray, end_byte_index));

  // 以字节为单位翻转目标串
  bitarray_reverse_byByte (bitarray, start_byte_index, end_byte_index);


  // 计算偏移量 （start_bit那里多了偏移了多少，end_bit偏移了多少）
  // 1~8
  size_t shift_start_bit = modulo (8 - bit_offset, 8);
  size_t shift_end_bit = (end_bit_index + 1) % 8;

  if(shift_end_bit == 0) shift_end_bit += 8;
  if(shift_start_bit == 0) shift_start_bit += 8;


  unsigned char a, b, reverse;
  ssize_t shift = shift_end_bit - shift_start_bit; // 这个差值必须是有符号数
  // 处理偏移
  if(shift > 0) { // end的偏移要大于start的偏移 此时需要整体将bit向着end的方向移动
    for(size_t i = end_byte_index; i >= start_byte_index; i--) {
      if(i == start_byte_index) {
        reverse = get_reverse_byte (bitarray_get_byte (bitarray, i)) >> shift;
        bitarray_set_byte (bitarray, i, get_reverse_byte (reverse));
        break;
      } else {
        a = get_reverse_byte (bitarray_get_byte (bitarray, i)) >> shift;
        b = get_reverse_byte (bitarray_get_byte (bitarray, i - 1)) << (8 - shift);
        bitarray_set_byte (bitarray, i, get_reverse_byte (a | b));
      }
    }
  } else if(shift < 0) {
    for(size_t i = start_byte_index; i <= end_byte_index; i++) {
      if(i == end_byte_index) {
        reverse = get_reverse_byte (bitarray_get_byte (bitarray, i)) << (-shift);
        bitarray_set_byte (bitarray, i, get_reverse_byte (reverse));
        break;
      } else {
        a = get_reverse_byte (bitarray_get_byte (bitarray, i)) << (-shift);
        b = get_reverse_byte (bitarray_get_byte (bitarray, i + 1)) >> (8 + shift);
        bitarray_set_byte (bitarray, i, get_reverse_byte (a | b));
      }
    }
  }

  // 此时 目标子串已经翻转到正确的位置 接下来就是处理
  // 因为翻转导致的子串的起始字节和尾部字节的那些不应该翻转的bit发生改变的问题
  // 这里就要用到一开始保存的 first_byte_reverse 和 last_byte_reverse 两个字节

  // 对于start端 要将start索引处的这个字节的 第1～(8-shift_start)个bit复原
  // 与上述类似的处理 首先要用到之前保存的 first_byte_reverse 去掉其低位的shift_start 
  // 拿到翻转后的start位置的字节 翻转后去掉其高位的（8-shift_start）
  // 1111 1111 >> 2 << 2
  // 1111 1100

  // 1111 1111 << 2 >> 2
  // 1111 1100 >> 2
  // 0011 1111
  unsigned char start_right = (unsigned char)(first_byte_reverse >> shift_start_bit) << shift_start_bit;
  unsigned char start_left = (unsigned char)(get_reverse_byte ((unsigned char)bitarray_get_byte (bitarray, start_byte_index))) << (8 - shift_start_bit) >> (8 - shift_start_bit);
  bitarray_set_byte (bitarray, start_byte_index, get_reverse_byte (start_right | start_left));

  // end端同理
  unsigned char end_left = (unsigned char)(last_byte_reverse << shift_end_bit) >> shift_end_bit;
  unsigned char end_right = (unsigned char)(get_reverse_byte ((unsigned char)bitarray_get_byte (bitarray, end_byte_index))) >> (8 - shift_end_bit) << (8 - shift_end_bit);
  bitarray_set_byte (bitarray, end_byte_index, get_reverse_byte (end_right | end_left));

}


 /**
  * @brief 反转一个bit串的任意子串
  *
  * @param bitarray bit串
  * @param bit_offset 子串起始索引
  * @param bit_length 子串长度
  * @param right_count 子串两部分 右侧长度
  */
static void bitarray_rotate_reverse (bitarray_t* const bitarray, const size_t bit_offset, const size_t bit_length, const size_t right_count)
{
  assert (bit_offset + bit_length <= bitarray->bit_sz); // 防止越界
  if(bitarray->bit_sz < 2) return;

  // 反转第一部分
  subarray_rotate_reverse (bitarray, bit_offset, right_count);
  // 反转第二部分
  subarray_rotate_reverse (bitarray, bit_offset + right_count, bit_length - right_count);
  // 整体反转
  subarray_rotate_reverse (bitarray, bit_offset, bit_length);
}


