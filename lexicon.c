#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
* Array a = [Tom\0Jerry\0...] -> English character words only (upper-case or lower-case)
* Quadratic Probing h(k,i) = (h(k) + c0 +c1*i + c2*i*i) % N
* k = key(x) = h'(x) = (Sum(ASCII/Unicode values)-4) % N
* p = h(k,i) = (h('x) + i*i) % N
* Hash Table t = [memorylocation,memorylocation,...]-> collision resolution by
*   collision-resolution by open-addressing
* Lexicon l =  (t, a)
*
* Operations
* Primary Operations
* Insertion
* Deletion
* Search
*
* Auxiliary Operations
* Print
* Create
* Empty/Full/Batch
*
* n =			# of Keys
* N = 			# of Slots of T
* a = n/N		Load Factor
* x			Word
* k = h'(x) = key(x)	Numeric Representation of a Word
* docID = 		# of B for docID
* s =			Size of statistics
* p =			# of B for pointer to A
*|T| =			# of MiB for T
*|A| +			# of MiB for A
*/

#define UNASSIGNED -1
#define DELETED -2
#define A_BIG_N_MULTIPLIER 2

typedef struct {
	int big_n;
	int *t;
	char *a;
	int a_last_index;
} lexicon;

void Batch(char *filename);
bool Empty();
bool Full();
int Insert(lexicon *l, char *x);		// 10
int Delete(lexicon *l, char *x);		// 11
int Search(lexicon *l, char *x, bool do_print);	// 12
void Print(lexicon *l);				// 13
lexicon* Create(int big_n);			// 14

// Derive key(x)/h'(x)
int calc_key(char *x, int big_n) {

	// CALCULATE PROBE INDEX K
	// k = key(x) = (Sum(x[0], ... , x[sizeof(x-1)]) - 4) % N
	int k = -4;
	for (int i = 0; x[i] != '\0'; i++) {
		k += (int) x[i];
	}
	k = k % big_n;

	return k;
}

// calc h(k)
int calc_quad_probe(int k, int i, int big_n) {
	return (k + (i*i)) % big_n;
}

// Free Array Memory
void free_array(char *a) {
	if (a != NULL) {
		free(a);
	}
}

// Free Table Memory
void free_table(int *t) {
	if (t != NULL) {
		free(t);
	}
}

// Free Lexicon Memory
void free_lexicon(lexicon *l) {
	if (l != NULL) {
		free(l);
	}
}

lexicon* recreate(lexicon *l, int a_big_n_multiplier) {
	int *a_last_index = &l->a_last_index;

	// Create Bigger Lexicon
	lexicon *l_new = Create(a_big_n_multiplier*l->big_n);

	for (int i=0; i<*a_last_index; i++) {
		char *x = &l->a[i];
		int p = Search(l, x, false);
		Insert(l_new, x);
		if(p==DELETED) {
			Delete(l_new, x);
		}
		do {
			if( l->a[i] == '\0' ){
				break;
			} else {
				i++;
			}
		} while (i<*a_last_index);
	}

	free_array(l->a);
	free_table(l->t);
	l->big_n=l_new->big_n;
	l->t=l_new->t;
	l->a=l_new->a;
	l->a_last_index=l_new->a_last_index;
}

int file_skip_line(FILE *file_pointer) {
	int character;
	while ((character = fgetc(file_pointer)) != '\n') {
		if (character == EOF) {
			return -1;
		}
	}
	return 0;
}

int parse_word(FILE *file_pointer, int character, char **return_x) {
	if ((character = fgetc(file_pointer)) == EOF) {
	        return -2;
	} else if (character == '\n') {
	        return -1;
	} else if (character != ' ') {
		file_skip_line(file_pointer);
		return -1;
	}

	// Get String
	int x_length = 1;
	while ((character = fgetc(file_pointer)) != '\n') {
		if (character == EOF) {
			return -2;
		} else if (!isalpha(character)) {
			file_skip_line(file_pointer);
			return -1;
		}
		x_length += 1;
	}

	*return_x = (char *)malloc((size_t)x_length * sizeof(char));
	if (*return_x == NULL) {
		fprintf(stderr, "Failed to allocate memory for string");
	}
	fseek(file_pointer, -1 * x_length, SEEK_CUR);
	fgets(*return_x, x_length, file_pointer);
	fgetc(file_pointer);
	return 0;
}

int parse_num(FILE *file_pointer, int character) {
	char *x;
	int return_n;
	if ((character = fgetc(file_pointer)) == EOF) {
	        return -2;
	} else if (character == '\n') {
	        return -1;
	} else if (character != ' ') {
		file_skip_line(file_pointer);
		return -1;
	}

	// Get Number
	int x_length = 1;
	while ((character = fgetc(file_pointer)) != '\n') {
		if (character == EOF) {
			return -2;
		} else if (!isdigit(character)) {
			file_skip_line(file_pointer);
			return -1;
		}
		x_length += 1;
	}
	x = (char *)malloc((size_t)x_length * sizeof(char));
	if (x == NULL) {
		fprintf(stderr, "Failed to allocate memory for string");
	}
	fseek(file_pointer, -1 * x_length, SEEK_CUR);
	fgets(x, x_length, file_pointer);
	fgetc(file_pointer);
	if((return_n = atoi(x)) == 0) {
		free(x);
		return -1;
	}
	free(x);
	return return_n;
}

void Batch(char *filename) {
	// Open File
	FILE *file_pointer = fopen(filename, "r");
	if (file_pointer == NULL) {
		printf("Error: Could not open file \"%s\" for reading\n", filename);
		return;
	}

	// Do Parsing
	int character;
	int code;
	char *x;
	lexicon *l;
	FILE *curr_word;
	bool l_inited = false;
	while ((character = fgetc(file_pointer)) != EOF) {
		//if (l_inited) {
		//	printf("Empty: %d\n", Empty(l));
		//	printf("Full: %d\n", Full(l));
		//}
		if (character!='1') {
			//Garbage line, skip
			if (file_skip_line(file_pointer) == -1) {
				fclose(file_pointer);
				return;
			}
			continue;
		}
		character = fgetc(file_pointer);
		switch (character) {

			// EOF found ->  End program
			case EOF:
			fclose(file_pointer);
			return;

			// Command 10: Insert
			case '0':
				if (!l_inited) {printf("Lexicon not created\n");break;}
				code = parse_word(file_pointer, character, &x);
				if(code == -2) {
					fclose(file_pointer);
					return;
				} else if (code == -1) {
					break;
				}

				Insert(l, x);
				free(x);
                                break;

			// Command 11: Deletion
			case '1':
				if (!l_inited) {printf("Lexicon not created\n");break;}
				code = parse_word(file_pointer, character, &x);
				if(code == -2) {
					fclose(file_pointer);
					return;
				} else if (code == -1) {
					break;
				}

				Delete(l, x);
				free(x);
				break;

			// Command 12: Search
			case '2':
				if (!l_inited) {printf("Lexicon not created\n");break;}
				code = parse_word(file_pointer, character, &x);
				if(code == -2) {
					fclose(file_pointer);
					return;
				} else if (code == -1) {
					break;
				}

				Search(l, x, true);
				free(x);
				break;

			// Command 13: Print
			case '3':
				if (!l_inited) {printf("Lexicon not created\n");break;}
				if ((character = fgetc(file_pointer)) == EOF) {
					Print(l);
					fclose(file_pointer);
					return;
				} else if (character == '\n') {
					Print(l);
				}
				break;

			// Command 14: Create
			case '4':
				if (l_inited) {
					free_array(l->a);
					free_table(l->t);
					free_lexicon(l);
				}
				code = parse_num(file_pointer, character);
				if(code == -2) {
					fclose(file_pointer);
					return;
				} else if (code == -1) {
					break;
				}
				l = Create(code);
				l_inited = true;
				break;

			// Command 15: Comment -> skip
			case '5':
				if (file_skip_line(file_pointer) == -1) {
					fclose(file_pointer);
					return;
				}
				break;
			// Garbage line, special case new line -> no need to skip
			case '\n':
				break;
			// Garbage line, skip
			default:
				if (file_skip_line(file_pointer) == -1) {
					fclose(file_pointer);
					return;
				}
				break;
		}
	}

	// Done with file, closing
	fclose(file_pointer);
}

bool Empty(lexicon *l) {
	for (int i = 0; i < l->big_n; i++) {
		if (l->t[i] != UNASSIGNED) {
			return false;
		}
	}
	return true;
}

bool Full(lexicon *l) {
	if (l->big_n * 15 <= l->a_last_index) {
		return true;
	}
	for (int i = 0; i < l->big_n; i++) {
		if (l->t[i] == UNASSIGNED) {
			return false;
		}
	}
	return true;
}

// Try to insert word x in lexicon l, return position inserted at.
int Insert(lexicon *l, char *x) {
	int big_n = l->big_n;
	int *t = l->t;
	char *a = l->a;
	int k = calc_key(x, big_n);
	int p;
	int i= 0;
	int a_last_index = l->a_last_index;

	do {
		// p = h(k,i)
		p = calc_quad_probe(k, i, big_n);

		// Check if empty
		if (t[p]==UNASSIGNED) {
			size_t x_length = strlen(x)+1;
			if (x_length+a_last_index>15*big_n) {
				int m = A_BIG_N_MULTIPLIER;
				while(x_length+a_last_index>m*15*big_n) {
					m=m*m;
				}
				recreate(l, m);
				return Insert(l,x);
			}
			t[p] = a_last_index;
			memcpy(&a[a_last_index], x, x_length);
			l->a_last_index = a_last_index + x_length;
			return p;
		} else {
			// Check if word is at t[p]
			if (t[p]!=DELETED && strcmp(x, &a[t[p]]) == 0) {
				// Word already in t[p]
				return p;
			}
			i += 1;
		}
	} while (i<big_n);
	recreate(l, A_BIG_N_MULTIPLIER);
	return Insert(l, x);
}

// Delete word x from Lexicon l
int Delete(lexicon *l, char *x) {
	int big_n = l->big_n;
	int *t = l->t;
	char *a = l->a;
	int k = calc_key(x, big_n);
	int p;
	int i = 0;

	do {
		// j = h(k,i)
		p = calc_quad_probe(k, i, big_n);

		// Check if word is at t[p]
		if (t[p] != DELETED &&  strcmp(x, &a[t[p]]) == 0) {
			// Word found at t[p] -> delete and return p
			t[p] = DELETED;
			printf("%s deleted from slot %d\n",x, p);
			return p;
		}
		i += 1;
	}
	while (i<big_n && t[p] != UNASSIGNED);

	printf("%s not found for delete\n", x);
	// Exits if word not found -> return -1
	return -1;
}

// Search Lexicon l's a for word and return pointer/index if present or -1 if not
int Search(lexicon *l, char *x, bool do_print) {
	int big_n = l->big_n;
	int *t = l->t;
	char *a = l->a;
	int k = calc_key(x, big_n);
	int p;
	int i = 0;

	do {
		// j = h(k,i)
		p = calc_quad_probe(k, i, big_n);

		// Check if word is at t[p]
		if (t[p] != DELETED && strcmp(x, &a[t[p]]) == 0) {
			// Word found at t[p] -> return j
			if (do_print) {
				printf("%s found at slot %d\n", x, p);
			}
			return p;
		}
		i += 1;
	}
	while (i<big_n && t[p] != UNASSIGNED);
	if (do_print) {
		printf("%s not found\n", x);
	}

	// Exits if word not found -> return -1
	return -1;
}

// Print Entirety of array a and table t of lexicon l
void Print(lexicon *l) {
	int *t = l->t;
	char *a = l->a;
	int *big_n = &l->big_n;
	int *a_last_index = &l->a_last_index;

	printf("    T                  A: ");
	for (int i=0; i<*a_last_index; i++) {
		char *x = &a[i];
		bool existent = (0<=Search(l, x, false));
		do {
			if( a[i] == '\0' ){
				printf("\\");
				break;
			} else if(existent) {
				printf("%c", a[i]);
				i++;
			} else {
				printf("*");
				i++;
			}
		} while (i<*a_last_index);
	}
	printf("\n");

	for (int i = 0; i<*big_n; i++) {
		printf("%d: ", i);
		if( t[i]>=0 ) {
			printf("%d", t[i]);
		}
		printf("\n");
	}
}

// Create and return a lexicon of size big_n
lexicon* Create(int big_n) {
	// Allocate Lexicon Size
	lexicon* l = (lexicon*)malloc(sizeof(lexicon));
	// Lexicon Allocation Failed
	if (l == NULL) {
		fprintf(stderr, "Failed to allicate memory for lexicon.");
		exit(EXIT_FAILURE);
	}

	l->big_n = big_n;

	// Allocate Table Size
	l->t = (int*)malloc(big_n * sizeof(int));
	// Table Allocation Failed
	if (l->t == NULL) {
		fprintf(stderr, "Failed to allicate memory for table.");
		free(l);
		exit(EXIT_FAILURE);
	}

	// Allocate Array Size
	l->a = (char*)malloc(15 * big_n * sizeof(char));
	// Array Allocation Failed
	if (l->a == NULL) {
		fprintf(stderr, "Failed to allicate memory for lexicon.");
		free(l->t);
		free(l);
		exit(EXIT_FAILURE);
	}

	// Initialize T to -1
	for (int i = 0; i < big_n; i++ ) {
		l->t[i]=-1;
	}

	// Initialize A to space
	memset(l->a, ' ', 15 * big_n * sizeof(char));

	l->a_last_index = 0;

	return l;
}

int main (int argc, char *argv[]) {
	// Insufficient arguments, Error Message and return -1
	if (argc < 2) {
		fprintf(stderr, "Error: Usage: %s <input_file>\n", argv[0] );
		return (-1);
	}

	Batch(argv[1]);

	return(0);
}
