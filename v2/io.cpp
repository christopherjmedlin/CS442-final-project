#include <mpi.h>
#include <stdio.h>
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
    buf = (int*) malloc(sizeof(int) * count * (neighborhood_size+1));
    printf("%x", buf);
    MPI_File_read_all(fh, buf, count*(neighborhood_size+1), MPI_INTEGER, &stat);
    return parse_rule_map(buf, count, neighborhood_size);
}

HashMap* parse_rule_map(int* buf, int count, int neighborhood_size) {
    HashMap* hm = hm_init(count);
    for (int i = 0; i < count; i++) {
        int* neighborhood = (int*) malloc(sizeof(int)*neighborhood_size); 
        for (int j = 0; j < neighborhood_size; j++) {
            neighborhood[j] = buf[i*(neighborhood_size+1)+j];
        }
        int val = buf[i*(neighborhood_size+1)+neighborhood_size];
        hm_insert(hm, neighborhood, neighborhood_size, val);
    }
    return hm;
}

MPI_Datatype get_subarray_type(int size, int local_size, int row, int col) {
    MPI_Datatype dt;
    int sizes[2] = {size, size};
    int subsizes[2] = {local_size, local_size};
    int starts[2] = {row * local_size, col * local_size};
    MPI_Type_create_subarray(2, sizes, subsizes, starts,
                             MPI_ORDER_C, MPI_INTEGER, &dt);
    MPI_Type_commit(&dt);
    return dt;
}

int* load_init_state(char* path, int size) {
    int n, row, col, local_size;
    int* buf;
    int procs, rank;
    MPI_Status stat;
    MPI_File fh;
    MPI_Datatype subarr;

    MPI_Comm_size(MPI_COMM_WORLD, &procs);
    n = sqrt(procs);
    local_size = size / n;
    buf = (int*) malloc(sizeof(int) * local_size);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    row = rank / n;
    col = rank % n;
    subarr = get_subarray_type(size, local_size, row, col);
    MPI_File_open(MPI_COMM_WORLD, path, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
    //int disp = 4 * (local_size * local_size * n * row + local_size * col);
    MPI_File_set_view(fh, 0, MPI_INTEGER, subarr, "native", MPI_INFO_NULL);
    MPI_File_read_all(fh, buf, local_size*local_size, MPI_INTEGER, &stat); 
    return buf;
}

void write_state(char* path, int size, int* state) {
    int n, row, col, local_size;
    int procs, rank;
    MPI_Status stat;
    MPI_File fh;
    MPI_Datatype subarr;

    MPI_Comm_size(MPI_COMM_WORLD, &procs);
    n = sqrt(procs);
    local_size = size / n;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    row = rank / n;
    col = rank % n;
    subarr = get_subarray_type(size, local_size, row, col);
    MPI_File_open(MPI_COMM_WORLD, path, MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);

    MPI_File_set_view(fh, 0, MPI_INTEGER, subarr, "native", MPI_INFO_NULL);
    MPI_File_write_all(fh, state, local_size * local_size, MPI_INTEGER, &stat);
}

int* load_init_state_rows(char* path, int size) {
    int n, row, col, local_size;
    int procs, rank;
    int* buf;
    MPI_Status stat;
    MPI_File fh;
    MPI_Datatype rows;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    int rows_per_proc = size / procs;

    MPI_Type_contiguous(size*rows_per_proc, MPI_INTEGER, &rows);
    MPI_Type_commit(&rows);
    buf = (int*) malloc(sizeof(int)*size*rows_per_proc);

    MPI_File_open(MPI_COMM_WORLD, path, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
    MPI_File_set_view(fh, rank*size*rows_per_proc, MPI_INTEGER, rows, "native", MPI_INFO_NULL);
    MPI_File_read_all(fh, buf, size*rows_per_proc, MPI_INTEGER, &stat); 
    return buf;
}
