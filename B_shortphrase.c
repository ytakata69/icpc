#include <stdio.h>
#include <string.h>

#define MAX_N 40
#define MAX_WORD_LEN 10

/* the number of prefix words that matches the target length */
int nMatchingWords(const int *wordLen, int targetLen)
{
  int i;
  for (i = 0; wordLen[i] > 0; i++) {
    targetLen -= wordLen[i];
    if (targetLen <= 0) break;
  }
  return targetLen == 0 ? i + 1 : 0;
}

int isValid(const int *wordLen)
{
  static const int targetLen[] = { 5, 7, 5, 7, 7 };
  int i, nword;
  for (i = 0; i < 5; i++) {
    nword = nMatchingWords(wordLen, targetLen[i]);
    if (nword == 0) return 0; /* false */
    wordLen += nword;
  }
  return 1; /* true */
}

int solve(const int *wordLen)
{
  int i; /* the number of skipped prefix words */
  for (i = 0; wordLen[i] > 0; i++) {
    if (isValid(wordLen + i)) return i + 1;
  }
  return 0;
}

int main()
{
  static int  wordLen[MAX_N + 1];
  static char word[MAX_WORD_LEN + 1];
  int n, i;
  for (;;) {
    scanf("%d", &n);
    if (n == 0) break;
    for (i = 0; i < n; i++) {
      scanf("%s", word);
      wordLen[i] = strlen(word);
    }
    wordLen[n] = 0;  /* end mark */
    printf("%d\n", solve(wordLen));
  }
  return 0;
}
