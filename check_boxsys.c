#include <stdio.h>
#include <stdlib.h>


/* Utility */

void print_intlist(char* p, int n, int* perm) {
  int i;
  printf("%s: [", p);
  for(i = 0; i < n; i++) {
    if (i != 0) printf(", ");
    printf("%d", perm[i]);
  }
  printf("]\n");
}

long long int fact(long long int n) {
  if (n == 0)
    return 1;
  else
    return n * fact(n-1);
}

/* Permutations */

typedef long long int id;

id id_of_perm(const int n, int* perm) {
  int i, j;
  int lehmer[n];
  id res = 0;

  for (i = 0; i < n; i++) lehmer[i] = perm[i];

  for (i = 0; i < n; i++) {
    res += lehmer[i];
    if (i != n-1) res *= n - 1 - i;

    for (j = i + 1; j < n; j++) {
      if (lehmer[j] > lehmer[i]) lehmer[j]--;
    }
  }
  // print_intlist("Lehmer (id_of_perm)", n, lehmer);

  return res;
}

void perm_of_id(id x, const int n, int* perm) {
  int i, j, t;
  int lehmer[n];

  for (i = n-1; i >= 0; i--) {
    lehmer[i] = x % (n-i);
    x /= (n-i);
  }

  // print_intlist("Lehmer (perm_of_id)", n, lehmer);

  for (i = 0; i < n; i++) perm[i] = i;

  for (i = 0; i < n; i++) {
    t = perm[i + lehmer[i]];
    for (j = i + lehmer[i]; j > i; j--) perm[j] = perm[j-1];
    perm[i] = t;
  }
}

/* Bitsets */

typedef unsigned long long int bits;
#define NBITS (8*(sizeof(bits)))

int bitset_get(bits* bitset, id x) {
  id pos = x / NBITS;
  id sh = x % NBITS;
  return (bitset[pos] >> sh) & 1;
}

int bitset_set(bits* bitset, id x) {
  id pos = x / NBITS;
  id sh = x % NBITS;
  bitset[pos] |= ((bits)1 << sh);
}

int bitset_unset(bits* bitset, id x) {
  id pos = x / NBITS;
  id sh = x % NBITS;
  bitset[pos] &= ~((bits)1 << sh);
}

id bitset_count(bits *bitset, id max) {
	id i;
	id count = 0;
	id posmax = max / NBITS;

	for (i = 0; i < posmax; i++) {
		bits x = bitset[i];
		if (NBITS == 64) {
			x = (x & 0x5555555555555555) + 
				((x >> 1) & 0x5555555555555555);
			x = (x & 0x3333333333333333) +
				((x >> 2) & 0x3333333333333333);
			x = (x & 0x0F0F0F0F0F0F0F0F) +
				((x >> 4) & 0x0F0F0F0F0F0F0F0F);
			x = (x & 0x00FF00FF00FF00FF) +
				((x >> 8) & 0x00FF00FF00FF00FF);
			x = (x & 0x0000FFFF0000FFFF) +
				((x >> 16) & 0x0000FFFF0000FFFF);
			x = (x & 0xFFFFFFFF) +
				((x >> 32) & 0xFFFFFFFF);
			count += x;
		} else {
			fprintf(stderr, "CANNOT COUNT!\n");
		}
	}
	for (i = posmax * NBITS; i < max; i++) {
		count += bitset_get(bitset, i);
	}
	return count;
}

/* Boxsys checking */

/*		n = nombre de fils
		p = nombre de boites
		l = fil gauche de la boite #i
		r = fil droite de la boite #i
*/

int check_boxsys(const int n, const int p, const int* l, const int* r) {
	id x;
	int i, t;

	id perms = fact(n), nperms;
	bits *T = malloc((perms/NBITS+1)*sizeof(bits));
	for (x = 0; x < perms/NBITS+1; x++) T[x] = 0;
	bitset_set(T, 0);

	for (i = 0; i < p; i++) {
		fprintf(stderr, "Counting... "); fflush(stderr);
		nperms = bitset_count(T, perms);
		fprintf(stderr, "%ld \\ %ld = %ld\n", nperms, perms, perms / nperms);
		fprintf(stderr, "Adding box %d: (%d, %d)\n", i, l[i], r[i]);
		if (nperms < perms / 2) {
			for (x = 0; x < perms; x++) {
				if (bitset_get(T, x)) {
					int perm[n];
					perm_of_id(x, n, perm);
					t = perm[l[i]];
					perm[l[i]] = perm[r[i]];
					perm[r[i]] = t;
					bitset_set(T, id_of_perm(n, perm));
				}
			}
		} else {
			for (x = 0; x < perms; x++) {
				if (!bitset_get(T, x)) {
					int perm[n];
					perm_of_id(x, n, perm);
					t = perm[l[i]];
					perm[l[i]] = perm[r[i]];
					perm[r[i]] = t;
					if (bitset_get(T, id_of_perm(n, perm)))
						bitset_set(T, x);
				}
			}
		}
	}

	for (x = 0; x < perms; x++) {
		if (!bitset_get(T, x)) return 0;
	}

	free(T);

	return 1;
}


int main() {
  int i;
  int n, p;

  scanf(" %d %d", &n, &p);
  int l[p], r[p];

  fprintf(stderr, "Checking permset n=%d p=%d\n", n, p);
	
  for(i = 0; i < p; i++) {
    scanf(" %d %d", &l[i], &r[i]);
  }
	
  printf ("%d\n", check_boxsys(n, p, l, r));

  return 0;
}
