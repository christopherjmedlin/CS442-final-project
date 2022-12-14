#ifndef IOH
#define IOH

#include <mpi.h>
#include <stdlib.h>
#include "hashmap.h"

HashMap* load_rule_map(char* path);

HashMap* parse_rule_map(int* buf, int count, int neighborhood_size);

int* load_init_state(char* path, int size);

void write_state(char* path, int size, int* state);

#endif
