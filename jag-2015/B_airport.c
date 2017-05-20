#include <stdio.h>
#include <string.h>

#define MAX_N 50
#define MAX_NAME_LEN 50

#define isVowel(c) (strchr("aiueo", (c)) != NULL && (c) != '\0')
typedef char name_t[MAX_NAME_LEN + 1];

/* Shorten a name: ex) "haneda" -> "hnd" */
void shorten(char name[]) {
  int len = strlen(name);
  int i, j;
  for (i = len - 1; i > 0; i--) {
    if (! isVowel(name[i - 1])) {
      name[i] = '*'; /* delete */
    }
  }
  for (i = j = 0; i < len; i++) {
    if (name[i] != '*') {
      name[j++] = name[i];
    }
  }
  name[j] = '\0';
}

int distinctPrefixLen(int n, name_t name[])
{
  int i, j, k;
  int maxk = -1;
  for (i = 0; i < n; i++) {
    for (j = i + 1; j < n; j++) {
      for (k = 0; name[i][k] != '\0' && name[j][k] != '\0'; k++) {
        if (name[i][k] != name[j][k]) {
          break;
        }
      }
      if (name[i][k] == name[j][k]) return -1;
      if (maxk < k) maxk = k;
    }
  }
  return maxk + 1;
}

int main()
{
  static name_t name[MAX_N];
  int n, i, k;
  for (;;) {
    scanf("%d", &n);
    if (n == 0) break;
    for (i = 0; i < n; i++) {
      scanf("%s", name[i]);
      shorten(name[i]);
    }
    k = distinctPrefixLen(n, name);
    printf("%d\n", k);
  }
  return 0;
}
