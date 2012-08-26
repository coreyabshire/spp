/*
 * This program creates a file which represents the shortest possible character
 * length of a given word list, for every possible combination of letters. The
 * intent of this data is to feed a heuristic to find the shortest possible
 * palindromic pangram as part of the ITA software puzzle on their job listing
 * section of their website.
 */

#include <time.h>

#include <stdio.h>
#include <string.h>

#define NLETTERS 26
#define SIZE 67108864
#define DEFAULT 255
#define MAX_WORD_LEN 80
#define RULER "---- --zy xwvu tsrq ponm lkji hgfe dcba"

unsigned char cost[SIZE];

/* bit count routines (from bit twiddling website) */

static const unsigned char bits256[] = 
{
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4, 
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5, 
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6, 
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7, 
  4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
};

int bit_count(int v)
{
  static unsigned char * p;
  p = (unsigned char *) &v;
  return bits256[p[0]] + bits256[p[1]] + bits256[p[2]] + bits256[p[3]];
}

/* bit print routines (from some website) */

void bin_prnt_byte(int x)
{
   int n;
   for(n=0; n<8; n++)
   {
      if((x & 0x80) !=0)
      {
         printf("1");
      }
      else
      {
         printf("0");
      }
      if (n==3)
      {
         printf(" "); /* insert a space between nybbles */
      }
      x = x<<1;
   }
}

void bin_prnt_int(int x)
{
   int hi, lo;
   hi=(x>>8) & 0xff;
   lo=x&0xff;
   bin_prnt_byte(hi);
   printf(" ");
   bin_prnt_byte(lo);
}

/* my addition to the bit print routines to print a 32 bit int */

void bin_prnt_int32(int x)
{
  int hi, lo;
  hi = (x>>16) & 0xffff;
  lo = x & 0xffff;
  bin_prnt_int(hi);
  printf(" ");
  bin_prnt_int(lo);
}

/* another print routine */

void print_sig(int sig, int ruler)
{
  bin_prnt_int32(sig);
  if (ruler) {
    printf("\n%s\n", RULER);
  }
}

/* main portion */

/* Compute the signature for the given word. */
unsigned int word_sig(char *s)
{
  char *c;
  unsigned int sig = 0;
  for (c = s; *c; c++) {
    sig |= 1 << (*c - 'a');
  }
  return sig;
}

/* Initialize the cost table. */
void init_costs()
{
  printf("initializing costs... ");
  int i;
  for (i = 0; i < SIZE; i++) {
    cost[i] = DEFAULT;
  }
  cost[0] = 0;
  printf("done.\n");
}

/* Initialize the costs for the words in the given file. */
void do_word_file()
{
  printf("processing word file... ");
  int sig = 0;
  int len = 0;
  FILE *fh = fopen("data/WORD.LST", "r");
  char s[MAX_WORD_LEN];
  while (!feof(fh)) {
    fscanf(fh, "%s", s);
    len = strlen(s);
    sig = word_sig(s);
    if (len < cost[sig]) {
      cost[sig] = len;
    }
  }
  printf("done.\n");
}

/* Find the shortest words for the single character constraint signatures. */
void do_single_letters()
{
  int i, j, sig;
  printf("processing single letters... \n");
  for (i = 0; i < SIZE; i++) {
    for (j = 0; j < NLETTERS; j++) {
      sig = 1 << j;
      if (i & sig) {
	if (cost[i] < cost[sig]) {
	  printf("reducing %c from %d to %d\n", j + 'a', cost[sig], cost[i]);
	  cost[sig] = cost[i];
	}
      }
    }
  }
  for (j = 0; j < NLETTERS; j++) {
    i = 1 << j;
    printf("%c final cost is %d\n", j + 'a', cost[i]);
  }
  printf("done.\n");
}

/* Distribute the given number n across the given mask, sig. */
static inline int logdist(int n, int sig)
{
  int i = 0;
  int c = 0;
  int d = 0;
  int b = 0;
  for (i = 0; i < NLETTERS; i++) {
    b = 1 << i;
    if (sig & b) {
      if ((1 << c) & n) {
	d |= b;
      }
      c++;
    }
  }
  return d;
}

void do_multi_letters(int n)
{
  clock_t start, end;
  double elapsed;
  int bits[n];

  int i, j, k, max, d, pcost, a, b, bit, m, mi;
  start = clock();
  max = 1 << (n - 1);
  //printf("max: %d\n", max);
  printf("  ");
  printf("processing letters at bit count %d... ", n);
  for (i = 0; i < SIZE; i++) {
    if (bit_count(i) == n) {
      //print_sig(i, 1);
      mi = 0;
      for (m = 0; m < 26; m++) {
	if ((1 << m) & i) {
	  bits[mi++] = 1 << m;
	}
      }
      //for (k = 0; k < n; k++) {
        //printf("%d ", bits[k]);
      //}
      //printf("\n");
      for (j = 1; j < max; j++) {
	a = 0;
	for (k = 0; k < n; k++) {
	  if (j & (1 << k)) {
	    a |= bits[k];
	  }
	}
	b = a ^ i;
	//print_sig(a, 0);
	//printf(" : ");
	//print_sig(b, 0);
	//printf("\n");
	pcost = cost[a] + cost[b];
	if (pcost < cost[i]) {
	  //printf("reducing ");
	  //print_sig(i, 0);
	  //printf(" from %d to %d\n", cost[i], pcost);
	  cost[i] = pcost;
	}
      }
    }
  }
  end = clock();
  elapsed = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("done. (took %f seconds)\n", elapsed);
}

void do_all_multi_letters()
{
  int i;
  printf("processing multi letters: \n");
  for (i = 2; i <= 26; i++) {
    do_multi_letters(i);
  }
  printf("all done.\n");
}

void write_cost_file()
{
  int i;
  printf("writing file... ");
  FILE *fh = fopen("hcost.dat", "w");
  fwrite(cost, 1, SIZE, fh);
  fclose(fh);
  printf("done.\n");
}

int main(int argc, char **argv)
{
  init_costs();
  do_word_file();
  do_single_letters();
  do_all_multi_letters();
  write_cost_file();
  return 0;
}
