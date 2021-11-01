/* Two confusables that both map to a third (LATIN CAPITAL LETTER H).  */

#include <stdio.h>

/* CYRILLIC CAPITAL LETTER EN (U+041D), with UTF-8 encoding 0xD0 0x9D.  */

void sayНello() { /* { dg-message "\\.\\.\\.confusable with non-equal identifier 'sayНello' \\('say\\\\u041dello'\\) here" } */
  printf("Hello, World!\n");
}

/* GREEK CAPITAL LETTER ETA (U+0397).  */

void sayΗello() { /* { dg-warning "identifier 'sayΗello' \\('say\\\\u0397ello'\\)\\.\\.\\." } */
  printf("Goodbye, World!\n");
}

int main() {
    sayНello();
    return 0;
}
