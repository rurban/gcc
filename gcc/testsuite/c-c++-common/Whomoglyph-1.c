#include <stdio.h>

/* ASCII 'H' (U+0048).  */

void sayHello() { /* { dg-message "\\.\\.\\.confusable with non-equal identifier 'sayHello' here" } */
  printf("Hello, World!\n");
}

/* CYRILLIC CAPITAL LETTER EN (U+041D), with UTF-8 encoding 0xD0 0x9D.  */

void sayНello() { /* { dg-warning "identifier 'sayНello' \\('say\\\\u041dello'\\)\\.\\.\\." } */
  printf("Goodbye, World!\n");
}

int main() {
    sayНello();
    return 0;
}
