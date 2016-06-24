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

/**
 * Parse one tree of an expression and return the value of it.
 * @param *n the number of remaining lines (modified by this function)
 */
int compute(int *n)
{
  line_t *head;
  int acc, sub, depth, content;
  head = getLine();
  depth   = head->depth;
  content = head->content;
  (*n)--;

  if (isdigit(content)) return content - '0';
  acc = (content == '+' ? 0 : 1); /* the unit */
  while (*n > 0 && testLine()->depth > depth) {
    sub = compute(n);
    acc = (content == '+' ? acc + sub : acc * sub);
  }
  return acc;
}

int main()
{
  int n, answer;
  for (;;) {
    scanf("%d", &n);
    if (n == 0) break;
    answer = compute(&n);
    printf("%d\n", answer);
  }
  return 0;
}
