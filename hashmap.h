#include <math.h>

typedef struct Node {
    int* key;
    int val;
    Node* next;
} Node;

typedef struct HashMap {
    int size;
    Node** nodes;    
} HashMap;

HashMap* hm_init(int size);

int hm_lookup(HashMap* m, int size, int* key, int keySize, int val);

void hm_insert(HashMap* m, int size, int* key, int keySize, int val);

void hm_free(HashMap* m);
