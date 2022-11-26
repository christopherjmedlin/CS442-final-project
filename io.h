#ifndef IOH
#define IOH

#include <mpi.h>
#include "hashmap.h"

HashMap* load_rule_map(char* path);

HashMap* parse_rule_map(int* buf, int count, int neighborhood_size);

#endif
