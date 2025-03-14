/* do not edit automatically generated by mc from StringConvert.  */
/* StringConvert.def provides functions to convert numbers to and from strings.

Copyright (C) 2001-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#if !defined (_StringConvert_H)
#   define _StringConvert_H

#include "config.h"
#include "system.h"
#   ifdef __cplusplus
extern "C" {
#   endif
#include <stdbool.h>
#   if !defined (PROC_D)
#      define PROC_D
       typedef void (*PROC_t) (void);
       typedef struct { PROC_t proc; } PROC;
#   endif

#   include "GDynamicStrings.h"

#   if defined (_StringConvert_C)
#      define EXTERN
#   else
#      define EXTERN extern
#   endif


/*
   IntegerToString - converts INTEGER, i, into a String. The field with
                     can be specified if non zero. Leading characters
                     are defined by padding and this function will
                     prepend a + if sign is set to TRUE.
                     The base allows the caller to generate binary,
                     octal, decimal, hexidecimal numbers.
                     The value of lower is only used when hexidecimal
                     numbers are generated and if TRUE then digits
                     abcdef are used, and if FALSE then ABCDEF are used.
*/

EXTERN DynamicStrings_String StringConvert_IntegerToString (int i, unsigned int width, char padding, bool sign, unsigned int base, bool lower);

/*
   CardinalToString - converts CARDINAL, c, into a String. The field
                      width can be specified if non zero. Leading
                      characters are defined by padding.
                      The base allows the caller to generate binary,
                      octal, decimal, hexidecimal numbers.
                      The value of lower is only used when hexidecimal
                      numbers are generated and if TRUE then digits
                      abcdef are used, and if FALSE then ABCDEF are used.
*/

EXTERN DynamicStrings_String StringConvert_CardinalToString (unsigned int c, unsigned int width, char padding, unsigned int base, bool lower);

/*
   StringToInteger - converts a string, s, of, base, into an INTEGER.
                     Leading white space is ignored. It stops converting
                     when either the string is exhausted or if an illegal
                     numeral is found.
                     The parameter found is set TRUE if a number was found.
*/

EXTERN int StringConvert_StringToInteger (DynamicStrings_String s, unsigned int base, bool *found);

/*
   StringToCardinal - converts a string, s, of, base, into a CARDINAL.
                      Leading white space is ignored. It stops converting
                      when either the string is exhausted or if an illegal
                      numeral is found.
                      The parameter found is set TRUE if a number was found.
*/

EXTERN unsigned int StringConvert_StringToCardinal (DynamicStrings_String s, unsigned int base, bool *found);

/*
   LongIntegerToString - converts LONGINT, i, into a String. The field with
                         can be specified if non zero. Leading characters
                         are defined by padding and this function will
                         prepend a + if sign is set to TRUE.
                         The base allows the caller to generate binary,
                         octal, decimal, hexidecimal numbers.
                         The value of lower is only used when hexidecimal
                         numbers are generated and if TRUE then digits
                         abcdef are used, and if FALSE then ABCDEF are used.
*/

EXTERN DynamicStrings_String StringConvert_LongIntegerToString (long int i, unsigned int width, char padding, bool sign, unsigned int base, bool lower);

/*
   StringToLongInteger - converts a string, s, of, base, into an LONGINT.
                         Leading white space is ignored. It stops converting
                         when either the string is exhausted or if an illegal
                         numeral is found.
                         The parameter found is set TRUE if a number was found.
*/

EXTERN long int StringConvert_StringToLongInteger (DynamicStrings_String s, unsigned int base, bool *found);

/*
   LongCardinalToString - converts LONGCARD, c, into a String. The field
                          width can be specified if non zero. Leading
                          characters are defined by padding.
                          The base allows the caller to generate binary,
                          octal, decimal, hexidecimal numbers.
                          The value of lower is only used when hexidecimal
                          numbers are generated and if TRUE then digits
                          abcdef are used, and if FALSE then ABCDEF are used.
*/

EXTERN DynamicStrings_String StringConvert_LongCardinalToString (long unsigned int c, unsigned int width, char padding, unsigned int base, bool lower);

/*
   StringToLongCardinal - converts a string, s, of, base, into a LONGCARD.
                          Leading white space is ignored. It stops converting
                          when either the string is exhausted or if an illegal
                          numeral is found.
                          The parameter found is set TRUE if a number was found.
*/

EXTERN long unsigned int StringConvert_StringToLongCardinal (DynamicStrings_String s, unsigned int base, bool *found);

/*
   ShortCardinalToString - converts SHORTCARD, c, into a String. The field
                           width can be specified if non zero. Leading
                           characters are defined by padding.
                           The base allows the caller to generate binary,
                           octal, decimal, hexidecimal numbers.
                           The value of lower is only used when hexidecimal
                           numbers are generated and if TRUE then digits
                           abcdef are used, and if FALSE then ABCDEF are used.
*/

EXTERN DynamicStrings_String StringConvert_ShortCardinalToString (short unsigned int c, unsigned int width, char padding, unsigned int base, bool lower);

/*
   StringToShortCardinal - converts a string, s, of, base, into a SHORTCARD.
                           Leading white space is ignored. It stops converting
                           when either the string is exhausted or if an illegal
                           numeral is found.
                           The parameter found is set TRUE if a number was found.
*/

EXTERN short unsigned int StringConvert_StringToShortCardinal (DynamicStrings_String s, unsigned int base, bool *found);

/*
   stoi - decimal string to INTEGER
*/

EXTERN int StringConvert_stoi (DynamicStrings_String s);

/*
   itos - integer to decimal string.
*/

EXTERN DynamicStrings_String StringConvert_itos (int i, unsigned int width, char padding, bool sign);

/*
   ctos - cardinal to decimal string.
*/

EXTERN DynamicStrings_String StringConvert_ctos (unsigned int c, unsigned int width, char padding);

/*
   stoc - decimal string to CARDINAL
*/

EXTERN unsigned int StringConvert_stoc (DynamicStrings_String s);

/*
   hstoi - hexidecimal string to INTEGER
*/

EXTERN int StringConvert_hstoi (DynamicStrings_String s);

/*
   ostoi - octal string to INTEGER
*/

EXTERN int StringConvert_ostoi (DynamicStrings_String s);

/*
   bstoi - binary string to INTEGER
*/

EXTERN int StringConvert_bstoi (DynamicStrings_String s);

/*
   hstoc - hexidecimal string to CARDINAL
*/

EXTERN unsigned int StringConvert_hstoc (DynamicStrings_String s);

/*
   ostoc - octal string to CARDINAL
*/

EXTERN unsigned int StringConvert_ostoc (DynamicStrings_String s);

/*
   bstoc - binary string to CARDINAL
*/

EXTERN unsigned int StringConvert_bstoc (DynamicStrings_String s);

/*
   StringToLongreal - returns a LONGREAL and sets found to TRUE
                      if a legal number is seen.
*/

EXTERN long double StringConvert_StringToLongreal (DynamicStrings_String s, bool *found);

/*
   LongrealToString - converts a LONGREAL number, Real, which has,
                      TotalWidth, and FractionWidth into a string.

                      So for example:

                      LongrealToString(1.0, 4, 2)  -> '1.00'
                      LongrealToString(12.3, 5, 2) -> '12.30'
                      LongrealToString(12.3, 6, 2) -> ' 12.30'
                      LongrealToString(12.3, 6, 3) -> '12.300'

                      if total width is too small then the fraction
                      becomes truncated.

                      LongrealToString(12.3, 5, 3) -> '12.30'

                      If TotalWidth is 0 then the function
                      will return the value of x which is converted
                      into as a fixed point number with exhaustive
                      precision.
*/

EXTERN DynamicStrings_String StringConvert_LongrealToString (long double x, unsigned int TotalWidth, unsigned int FractionWidth);

/*
   stor - returns a REAL given a string.
*/

EXTERN double StringConvert_stor (DynamicStrings_String s);

/*
   stolr - returns a LONGREAL given a string.
*/

EXTERN long double StringConvert_stolr (DynamicStrings_String s);

/*
   ToSigFig - returns a floating point or base 10 integer
              string which is accurate to, n, significant
              figures.  It will return a new String
              and, s, will be destroyed.


              So:  12.345

              rounded to the following significant figures yields

              5      12.345
              4      12.34
              3      12.3
              2      12
              1      10
*/

EXTERN DynamicStrings_String StringConvert_ToSigFig (DynamicStrings_String s, unsigned int n);

/*
   ToDecimalPlaces - returns a floating point or base 10 integer
                     string which is accurate to, n, decimal
                     places.  It will return a new String
                     and, s, will be destroyed.
                     Decimal places yields, n, digits after
                     the .

                     So:  12.345

                     rounded to the following decimal places yields

                     5      12.34500
                     4      12.3450
                     3      12.345
                     2      12.34
                     1      12.3
*/

EXTERN DynamicStrings_String StringConvert_ToDecimalPlaces (DynamicStrings_String s, unsigned int n);
#   ifdef __cplusplus
}
#   endif

#   undef EXTERN
#endif
