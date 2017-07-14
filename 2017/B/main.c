#include <stdio.h>
#include <string.h>

#define MAX_LEN  200

#define TRUE     1
#define FALSE    0

#define IDENTICAL 0
#define CLOSE     1
#define DIFFERENT 2
char *label[] = { "IDENTICAL", "CLOSE", "DIFFERENT" };

int solve(char *s1, char *s2)
{
  int status = IDENTICAL;
  int inStr  = FALSE;

  for ( ; *s1 != '\0'; s1++, s2++) {
    if (*s1 != *s2) {
      if (! inStr || status != IDENTICAL) { return DIFFERENT; }
      while (*s1 != '"') { s1++; }  /* skip to next '"' */
      while (*s2 != '"') { s2++; }  /* skip to next '"' */
      status = CLOSE;
    }
    if (*s1 == '"') { inStr = ! inStr; }
  }
  if (*s2 != '\0') { return DIFFERENT; }
  return status;
}

int main()
{
  static char s1[MAX_LEN + 1];
  static char s2[MAX_LEN + 1];

  for (;;) {
    scanf("%s", s1);
    if (strcmp(s1, ".") == 0) break;
    scanf("%s", s2);
    printf("%s\n", label[solve(s1, s2)]);
  }
  return 0;
}
