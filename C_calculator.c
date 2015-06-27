#include <stdio.h>
#include <ctype.h>

typedef struct {
  int depth;
  int content;
} line_t;

void skipToLF()
{
  int c;
  while ((c = getchar()) != '\n' && c != EOF) {}
}

line_t *parse()
{
  static line_t result;
  int depth = 0;
  int c;
  skipToLF();
  while ((c = getchar()) == '.') depth++;
  result.depth   = depth;
  result.content = c;
  return &result;
}

line_t *cache = NULL;

line_t *getLine()
{
  line_t *result = (cache == NULL ? parse() : cache);
  cache = NULL;
  return result;
}

/**
 * Return the depth and the content of the next line,
 * without consuming the line.
 */
line_t *testLine()
{
  if (cache == NULL) { cache = parse(); }
  return cache;
}

int nLine;  /* the number of remaining lines */

/**
 * Parse one tree of an expression and return the value of the expression.
 */
int compute()
{
  line_t *head;
  int acc, sub, depth, content;
  head = getLine();
  depth   = head->depth;
  content = head->content;
  nLine--;

  if (isdigit(content)) return content - '0';
  acc = (content == '+' ? 0 : 1); /* the unit */
  while (nLine > 0 && testLine()->depth > depth) {
    sub = compute();
    acc = (content == '+' ? acc + sub : acc * sub);
  }
  return acc;
}

int main()
{
  int answer;
  for (;;) {
    scanf("%d", &nLine);
    if (nLine == 0) break;
    answer = compute();
    printf("%d\n", answer);
  }
  return 0;
}
