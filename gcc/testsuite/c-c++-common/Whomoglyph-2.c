#include <stdio.h>

/* CYRILLIC CAPITAL LETTER EN (U+041D), with UTF-8 encoding 0xD0 0x9D.  */

void sayНello() { /* { dg-message '\\.\\.\\.confusable with non-equal identifier 'sayНello' \\('say\\\\u041dell\\o'\\) here" } */
  printf("Hello, World!\n");
}

/* ASCII 'H' (U+0048).  */

void sayHello() { /* { dg-warning "identifier 'sayHello'\\.\\.\\." } */
  printf("Goodbye, World!\n");
}

int main() {
    sayНello();
    return 0;
}
