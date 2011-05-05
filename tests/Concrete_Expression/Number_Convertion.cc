void reverse(char *reversed) {
  char tmp;
  for (int i = 0, j = strlen(reversed)-1; i<j; i++, j--) {
    tmp = reversed[i];
    reversed[i] = reversed[j];
    reversed[j] = tmp;
  }
}

char *itoa(int number) {
  bool neg=number<0?true:false;
  number=number<0?-number:number;
  int length = (int)(log10(number))+1;
  int i=0;
  char *s = new char[length];
  do {
    s[i++] = number % 10 + '0';
  } while ((number /= 10) > 0);
  if (neg){
    s[i] = '-';
    s[++i] = '\0';
  }
  else
    s[i] = '\0';
  reverse(s);
  return s;
}

