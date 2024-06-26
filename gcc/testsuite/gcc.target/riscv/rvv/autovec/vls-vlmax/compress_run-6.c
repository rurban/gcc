/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */

#include <assert.h>
#include "compress-6.c"

int
main (void)
{
  vnx128i test_1_x
    = {0,   1,	 2,   3,   4,	5,   6,	  7,   8,   9,	 10,  11,  12,
       13,  14,	 15,  16,  17,	18,  19,  20,  21,  22,	 23,  24,  25,
       26,  27,	 28,  29,  30,	31,  32,  33,  34,  35,	 36,  37,  38,
       39,  40,	 41,  42,  43,	44,  45,  46,  47,  48,	 49,  50,  51,
       52,  53,	 54,  55,  56,	57,  58,  59,  60,  61,	 62,  63,  64,
       65,  66,	 67,  68,  69,	70,  71,  72,  73,  74,	 75,  76,  77,
       78,  79,	 80,  81,  82,	83,  84,  85,  86,  87,	 88,  89,  90,
       91,  92,	 93,  94,  95,	96,  97,  98,  99,  100, 101, 102, 103,
       104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
       117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127};
  vnx128i test_1_y
    = {128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140,
       141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153,
       154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166,
       167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
       180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192,
       193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205,
       206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218,
       219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231,
       232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244,
       245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255};
  vnx128i test_1_except
    = {1,   3,	 4,   5,   6,	7,   8,	  10,  12,  14,	 15,  16,  17,
       18,  22,	 25,  28,  29,	30,  31,  36,  37,  40,	 41,  42,  43,
       44,  46,	 52,  54,  55,	58,  61,  62,  64,  67,	 68,  69,  70,
       71,  76,	 77,  78,  80,	82,  83,  84,  86,  87,	 88,  91,  94,
       95,  99,	 102, 104, 106, 110, 112, 115, 116, 125, 126, 127, 144,
       145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157,
       158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170,
       171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183,
       184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196,
       197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207};
  vnx128i test_1_real;
  test_1_real = test_1 (test_1_x, test_1_y);
  for (int i = 0; i < 128; i++)
    assert (test_1_real[i] == test_1_except[i]);

  vnx64i test_2_x
    = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
       32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63};
  vnx64i test_2_y
    = {64,  65,	 66,  67,  68,	69,  70,  71,  72,  73,	 74,  75,  76,
       77,  78,	 79,  80,  81,	82,  83,  84,  85,  86,	 87,  88,  89,
       90,  91,	 92,  93,  94,	95,  96,  97,  98,  99,	 100, 101, 102,
       103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
       116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127};
  vnx64i test_2_except
    = {0,  2,  3,  4,  5,  7,  11, 13, 14, 16, 17, 19, 20, 22, 23, 24,
       27, 28, 30, 31, 35, 37, 39, 40, 44, 45, 46, 53, 54, 56, 61, 63,
       68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
       84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99};
  vnx64i test_2_real;
  test_2_real = test_2 (test_2_x, test_2_y);
  for (int i = 0; i < 64; i++)
    assert (test_2_real[i] == test_2_except[i]);

  vnx32i test_3_x
    = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx32i test_3_y
    = {32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63};
  vnx32i test_3_except
    = {0,  1,  3,  4,  7,  8,  12, 13, 14, 19, 21, 22, 23, 27, 29, 31,
       41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56};
  vnx32i test_3_real;
  test_3_real = test_3 (test_3_x, test_3_y);
  for (int i = 0; i < 32; i++)
    assert (test_3_real[i] == test_3_except[i]);

  vnx16i test_4_x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  vnx16i test_4_y
    = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx16i test_4_except
    = {2, 3, 4, 6, 7, 8, 9, 12, 20, 21, 22, 23, 24, 25, 26, 27};
  vnx16i test_4_real;
  test_4_real = test_4 (test_4_x, test_4_y);
  for (int i = 0; i < 16; i++)
    assert (test_4_real[i] == test_4_except[i]);

  vnx128ui test_5_x
    = {0,   1,	 2,   3,   4,	5,   6,	  7,   8,   9,	 10,  11,  12,
       13,  14,	 15,  16,  17,	18,  19,  20,  21,  22,	 23,  24,  25,
       26,  27,	 28,  29,  30,	31,  32,  33,  34,  35,	 36,  37,  38,
       39,  40,	 41,  42,  43,	44,  45,  46,  47,  48,	 49,  50,  51,
       52,  53,	 54,  55,  56,	57,  58,  59,  60,  61,	 62,  63,  64,
       65,  66,	 67,  68,  69,	70,  71,  72,  73,  74,	 75,  76,  77,
       78,  79,	 80,  81,  82,	83,  84,  85,  86,  87,	 88,  89,  90,
       91,  92,	 93,  94,  95,	96,  97,  98,  99,  100, 101, 102, 103,
       104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
       117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127};
  vnx128ui test_5_y
    = {128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140,
       141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153,
       154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166,
       167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
       180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192,
       193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205,
       206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218,
       219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231,
       232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244,
       245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255};
  vnx128ui test_5_except
    = {1,   3,	 4,   5,   6,	7,   8,	  10,  12,  14,	 15,  16,  17,
       18,  22,	 25,  28,  29,	30,  31,  36,  37,  40,	 41,  42,  43,
       44,  46,	 52,  54,  55,	58,  61,  62,  64,  67,	 68,  69,  70,
       71,  76,	 77,  78,  80,	82,  83,  84,  86,  87,	 88,  91,  94,
       95,  99,	 102, 104, 106, 110, 112, 115, 116, 125, 126, 127, 144,
       145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157,
       158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170,
       171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183,
       184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196,
       197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207};
  vnx128ui test_5_real;
  test_5_real = test_5 (test_5_x, test_5_y);
  for (int i = 0; i < 128; i++)
    assert (test_5_real[i] == test_5_except[i]);

  vnx64ui test_6_x
    = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
       32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63};
  vnx64ui test_6_y
    = {64,  65,	 66,  67,  68,	69,  70,  71,  72,  73,	 74,  75,  76,
       77,  78,	 79,  80,  81,	82,  83,  84,  85,  86,	 87,  88,  89,
       90,  91,	 92,  93,  94,	95,  96,  97,  98,  99,	 100, 101, 102,
       103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
       116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127};
  vnx64ui test_6_except
    = {0,  2,  3,  4,  5,  7,  11, 13, 14, 16, 17, 19, 20, 22, 23, 24,
       27, 28, 30, 31, 35, 37, 39, 40, 44, 45, 46, 53, 54, 56, 61, 63,
       68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
       84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99};
  vnx64ui test_6_real;
  test_6_real = test_6 (test_6_x, test_6_y);
  for (int i = 0; i < 64; i++)
    assert (test_6_real[i] == test_6_except[i]);

  vnx32ui test_7_x
    = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx32ui test_7_y
    = {32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63};
  vnx32ui test_7_except
    = {0,  1,  3,  4,  7,  8,  12, 13, 14, 19, 21, 22, 23, 27, 29, 31,
       41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56};
  vnx32ui test_7_real;
  test_7_real = test_7 (test_7_x, test_7_y);
  for (int i = 0; i < 32; i++)
    assert (test_7_real[i] == test_7_except[i]);

  vnx16ui test_8_x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  vnx16ui test_8_y
    = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx16ui test_8_except
    = {2, 3, 4, 6, 7, 8, 9, 12, 20, 21, 22, 23, 24, 25, 26, 27};
  vnx16ui test_8_real;
  test_8_real = test_8 (test_8_x, test_8_y);
  for (int i = 0; i < 16; i++)
    assert (test_8_real[i] == test_8_except[i]);

  vnx64f test_9_x
    = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
       32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63};
  vnx64f test_9_y
    = {64,  65,	 66,  67,  68,	69,  70,  71,  72,  73,	 74,  75,  76,
       77,  78,	 79,  80,  81,	82,  83,  84,  85,  86,	 87,  88,  89,
       90,  91,	 92,  93,  94,	95,  96,  97,  98,  99,	 100, 101, 102,
       103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
       116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127};
  vnx64f test_9_except
    = {0,  2,  3,  4,  5,  7,  11, 13, 14, 16, 17, 19, 20, 22, 23, 24,
       27, 28, 30, 31, 35, 37, 39, 40, 44, 45, 46, 53, 54, 56, 61, 63,
       68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
       84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99};
  vnx64f test_9_real;
  test_9_real = test_9 (test_9_x, test_9_y);
  for (int i = 0; i < 64; i++)
    assert (test_9_real[i] == test_9_except[i]);

  vnx32f test_10_x
    = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx32f test_10_y
    = {32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63};
  vnx32f test_10_except
    = {0,  1,  3,  4,  7,  8,  12, 13, 14, 19, 21, 22, 23, 27, 29, 31,
       41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56};
  vnx32f test_10_real;
  test_10_real = test_10 (test_10_x, test_10_y);
  for (int i = 0; i < 32; i++)
    assert (test_10_real[i] == test_10_except[i]);

  vnx16f test_11_x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  vnx16f test_11_y
    = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx16f test_11_except
    = {2, 3, 4, 6, 7, 8, 9, 12, 20, 21, 22, 23, 24, 25, 26, 27};
  vnx16f test_11_real;
  test_11_real = test_11 (test_11_x, test_11_y);
  for (int i = 0; i < 16; i++)
    assert (test_11_real[i] == test_11_except[i]);

  return 0;
}
