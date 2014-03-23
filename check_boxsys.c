#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>


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

#define PARA 4		// number of parallel threads to launch

struct task {
	id begin, end;
	bits *prev_T;
	bits *my_T;
	int n;
	int l, r;
	int way;		// 0 : propagate ones ; 1 : erase zeroes
};

void* do_task(void* p) {
	struct task *t = (struct task*) p;

	id x;
	int temp;
	const int n = t->n, l = t->l, r = t->r;

	if (t->way == 0) {
		for (x = t->begin; x < t->end; x++) {
			if (bitset_get(t->prev_T, x)) {
				int perm[n];
				perm_of_id(x, n, perm);
				temp = perm[l];
				perm[l] = perm[r];
				perm[r] = temp;
				bitset_set(t->my_T, id_of_perm(n, perm));
			}
		}
	} else {
		for (x = t->begin; x < t->end; x++) {
			if (!bitset_get(t->prev_T, x)) {
				int perm[n];
				perm_of_id(x, n, perm);
				temp = perm[l];
				perm[l] = perm[r];
				perm[r] = temp;
				if (bitset_get(t->prev_T, id_of_perm(n, perm)))
					bitset_set(t->my_T, x);
			}
		}
	}
	fprintf(stderr, "%ld-%ld.", t->begin, t->end);
	fflush(stderr);

	return 0;
}

int check_boxsys(const int n, const int p, const int* l, const int* r) {
	id x;
	int i, j;

	id perms = fact(n), nperms;
	bits *T = malloc((perms/NBITS+1)*sizeof(bits));
	for (x = 0; x < perms/NBITS+1; x++) T[x] = 0;
	bitset_set(T, 0);

	struct task t[PARA];
	for (j = 0; j < PARA; j++) {
		t[j].begin = ((id)j * perms) / PARA;
		t[j].end = (((id)j+1) * perms) / PARA;
		t[j].prev_T = T;
		t[j].my_T = malloc((perms/NBITS+1)*sizeof(bits));
		for (x = 0; x < perms/NBITS+1; x++) t[j].my_T[x] = 0;
		t[j].n = n;
	}
	pthread_t thread[PARA];

	for (i = 0; i < p; i++) {
		fprintf(stderr, "Counting... "); fflush(stderr);
		nperms = bitset_count(T, perms);
		fprintf(stderr, "%ld \\ %ld = %ld\n", nperms, perms, perms / nperms);
		fprintf(stderr, "Adding box %d: (%d, %d)\n", i, l[i], r[i]);

		for (j = 0; j < PARA; j++) {
			t[j].l = l[i];
			t[j].r = r[i];
			t[j].way = (nperms > perms / 2 ? 1 : 0);
			pthread_create(&thread[j], NULL, do_task, &t[j]);
		}
		for (j = 0; j < PARA; j++) {
			pthread_join(thread[j], NULL);
			for (x = 0; x < perms/NBITS+1; x++)
				T[x] |= t[j].my_T[x];
		}
		fprintf(stderr, "OK.\n");
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
