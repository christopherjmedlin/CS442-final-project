#ifndef HASHMAPH
#define HASHMAPH

#include <math.h>
#include <stdlib.h>

typedef struct Node {
    int* key;
    int val;
    struct Node* next;
} Node;

typedef struct HashMap {
    int size;
    struct Node** nodes;    
} HashMap;

HashMap* hm_init(int size);

int hm_lookup(HashMap* m, int* key, int keySize);

void hm_insert(HashMap* m, int* key, int keySize, int val);

void hm_free(HashMap* m);

#endif
