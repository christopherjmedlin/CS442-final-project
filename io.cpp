#include <mpi.h>
#include "io.h"

HashMap* load_rule_map(char* path) {
    int count;
    // size of the neighboorhood, 5 for von neumann and 9 for moore
    int neighborhood_size;
    int* buf;
    MPI_Status stat;
    MPI_File fh;
    MPI_File_open(MPI_COMM_WORLD, path, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
    MPI_File_read_all(fh, &count, 1, MPI_INTEGER, &stat);
    MPI_File_read_all(fh, &neighborhood_size, 1, MPI_INTEGER, &stat);
    MPI_File_read_all(fh, buf, count*(neighborhood_size+1), MPI_INTEGER, &stat);
    return parse_rule_map(buf, count, neighborhood_size);
}

HashMap* parse_rule_map(int* buf, int count, int neighborhood_size) {
    HashMap* hm = hm_init(count);
    for (int i = 0; i < count; i++) {
        int* neighborhood = (int*) malloc(sizeof(int)*neighborhood_size); 
        for (int j = 0; i < neighborhood_size; i++) {
            neighborhood[j] = buf[i*(neighborhood_size+1)+j];
        }
        int val = buf[i*(neighborhood_size+1)+neighborhood_size];
        hm_insert(hm, count, neighborhood, neighborhood_size, val);
    }
    return hm;
}
