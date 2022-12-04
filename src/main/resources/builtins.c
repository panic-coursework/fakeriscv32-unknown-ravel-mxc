typedef struct {
  int length;
  char content[0];
} MxString;

typedef struct {
  int length;
  void *content[0];
} MxArray;

#ifdef HOST
typedef unsigned long size_t;
#else
typedef unsigned size_t;
#endif

int printf (const char *pattern, ...);
int sprintf (char *dest, const char *pattern, ...);
int scanf (const char *pattern, ...);
int sscanf (const char *src, const char *pattern, ...);
size_t strlen (const char *str);
int strcmp (const char *s1, const char *s2);
void *memcpy (void *dest, const void *src, size_t n);
void *malloc (size_t n);

void print (MxString *str) {
  printf("%s", str->content);
}
void println (MxString *str) {
  printf("%s\n", str->content);
}
void printInt (int x) {
  printf("%d", x);
}
void printlnInt (int x) {
  printf("%d\n", x);
}

MxString *getString () {
  MxString *buf = (MxString *) malloc(4096);
  scanf("%s", buf->content);
  buf->length = strlen(buf->content);
  return buf;
}
int getInt () {
  int x;
  scanf("%d", &x);
  return x;
}
MxString *toString (int x) {
  MxString *buf = (MxString *) malloc(16);
  sprintf(buf->content, "%d", x);
  buf->length = strlen(buf->content);
  return buf;
}

int string_length (MxString *str) {
  return str->length;
}

MxString *string_substring (MxString *str, int left, int right) {
  int len = right - left;
  MxString *buf = (MxString *) malloc(5 + len);
  buf->length = len;
  memcpy(buf->content, str->content + left, len);
  buf->content[len] = '\0';
  return buf;
}

int string_parseInt (MxString *str) {
  int x;
  sscanf(str->content, "%d", &x);
  return x;
}

int string_ord (MxString *str, int ix) {
  return str->content[ix];
}


int array_size (MxArray *array) {
  return array->length;
}


MxString *string_add (MxString *lhs, MxString *rhs) {
  MxString *buf = (MxString *) malloc(5 + lhs->length + rhs->length);
  memcpy(buf->content, lhs->content, lhs->length);
  memcpy(buf->content + lhs->length, rhs->content, rhs->length);
  buf->content[lhs->length + rhs->length] = '\0';
  buf->length = lhs->length + rhs->length;
  return buf;
}

int string_compare (MxString *lhs, MxString *rhs) {
  return strcmp(lhs->content, rhs->content);
}
