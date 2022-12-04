#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <mpi.h>
#include <time.h>
#include <algorithm>
#include "io.h"
#include "hashmap.h"

#define GRID_WIDTH 27
#define TOTAL_GRID_SIZE GRID_WIDTH*GRID_WIDTH
#define GEN_DELAY_MS 100
#define START_RANDOM 0
#define COLOR_SUB_GRIDS 1
#define N_GENERATIONS 1000

const char *ARR_COLORS[] = {// Background colors to use for coloring sub grids 
    "\033[48;5;1m",         // RED     
    "\033[48;5;2m",         // GREEN
    "\033[48;5;3m",         // YELLOW
    "\033[48;5;4m",         // BLUE
    "\033[48;5;5m",         // PINK
    "\033[48;5;6m",         // TURQ
    "\033[48;5;9m",         // ORANGE
    "\033[48;5;87m",        // CYAN
    "\033[48;5;218m"        // ROSE
};

#define NUM_COLORS (sizeof(ARR_COLORS) / sizeof(const char *)) // Get the size of ARR_COLORS

#define S_TOPLEFT "\033[H"        // Set cursor to top left
#define C_RST     "\033[0;39m"    // Reset color code to default
#define C_B_BLACK "\033[0;40m"    // Set background color black
#define C_B_WHITE "\033[0;47m"

#define TAG_U  10 // Receiving value for upper left corner
#define TAG_D  20 // Receiving value for upper right corner

int get(int* s, int* up_buf, int* down_buf, int n, int local_n, int i, int j) {
    // wrap around j
    if (j == -1) {
        j = n-1;    
    }
    j %= n;

    // use appropriate buffer depending on which row
    if (i == -1) {
        return up_buf[j];
    } else if (i == local_n) {
        return down_buf[j];
    }
    return s[i*n+j];
}

// returns all of the values of the cell's neighbors (including itself)
void get_cell_neighbors(int* s, int* up_buf, int* down_buf, int n, int local_n, int i, int j, int* neighbors) {
    neighbors[0] = get(s, up_buf, down_buf, n, local_n, i, j-1);
    neighbors[1] = get(s, up_buf, down_buf, n, local_n, i-1, j-1);
    neighbors[2] = get(s, up_buf, down_buf, n, local_n, i-1, j);
    neighbors[3] = get(s, up_buf, down_buf, n, local_n, i-1, j+1);
    neighbors[4] = get(s, up_buf, down_buf, n, local_n, i, j+1);
    neighbors[5] = get(s, up_buf, down_buf, n, local_n, i+1, j+1);
    neighbors[6] = get(s, up_buf, down_buf, n, local_n, i+1, j);
    neighbors[7] = get(s, up_buf, down_buf, n, local_n, i+1, j-1);
}

/*
Updates local grid. This just makes the most sense in my brain, it feels sloppy though. 
Need to think of a way to consider the corners and edges without needing to copy the whole graph.
*/
void update_state(int* s, int n, int local_n, int* up_buf, int* down_buf, HashMap* rt, int* neighbors) {
    for (int i = 0; i < local_n; i++) {
        for (int j = 0; j < local_n; j++) {
            get_cell_neighbors(s, up_buf, down_buf, n, local_n, i, j, neighbors);                    
            s[i*n+j] = hm_lookup(rt, neighbors, 8);
        }
    }
}

/*
Gets neighboring processes with with capability to wrap the grid.
*/

void get_neighbors(int* neighbors, int rank, int n_procs)
{
    const int ppl = (int)sqrt(n_procs);

    int row = rank/ppl;
    int col = rank%ppl;

    neighbors[0] = (row && col)? rank-ppl-1 : (row? rank-1: (col? (ppl*(ppl-1)+col-1) : n_procs-1)); // up left
    neighbors[1] = (row)? rank-ppl: (ppl*(ppl-1)+col); // up
    neighbors[2] = (row && (col<(ppl-1)))? rank-ppl+1 : (row? (row-1)*ppl : ((col<(ppl-1))? ppl*(ppl-1)+col+1  : ppl*(ppl-1)));  // up right
    neighbors[7] = (col)? rank-1 : rank+ppl-1 ; // left
    neighbors[3] = (col<(ppl-1))? rank+1 : rank-ppl+1; // right
    neighbors[6] = (col && (row<(ppl-1)))? rank+ppl-1 : (col? col-1 : ((row<(ppl-1))? ((row+2)*ppl)-1  : ppl-1 )); // down left
    neighbors[5] = (row<(ppl-1))? rank+ppl : col; // down
    neighbors[4] = (row<(ppl-1) && col<(ppl-1))? rank+ppl+1 : ((row<(ppl-1))? rank+1 : ((col<(ppl-1))? col+1: 0));// down right

    for (int i = 0; i < 8; i++) {
        printf("Rank: %d, Neighbors: %d %d", rank, i, neighbors[i]);
    }
}

void start(int n, int i) {
    int size, rank;
    int n_up, n_down;
    MPI_Status stat;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    HashMap* rt = load_rule_map("gol.table");
    int* state = load_init_state_rows("test2.bin", n);
    write_state_rows("out.bin", n, state);
    int current_iter = 0;
    int* cell_neighbors = (int*) malloc(sizeof(int) * 8);
    int local_n = n / size;
    int* up_buf = (int*) malloc(sizeof(int) * n);
    int* down_buf = (int*) malloc(sizeof(int) * n);
    
    // init datatypes
    MPI_Datatype row;
    MPI_Type_contiguous(n, MPI_INTEGER, &row);
    MPI_Type_commit(&row);

    if (rank == 0) {
        n_up = size-1;
    } else {
        n_up = rank-1;
    }

    if (rank == size-1) {
        n_down = 0;
    } else {
        n_down = rank+1;
    }
    
    while (current_iter < i) {
        if (rank % 2) {
            MPI_Send(state, 1, row, n_up, TAG_U, MPI_COMM_WORLD);
            MPI_Send(state + (local_n-1)*n, 1, row, n_down, TAG_D, MPI_COMM_WORLD);
            MPI_Recv(down_buf, 1, row, n_down, TAG_U, MPI_COMM_WORLD, &stat);
            MPI_Recv(up_buf, 1, row, n_up, TAG_D, MPI_COMM_WORLD, &stat);
        } else {
            MPI_Recv(down_buf, 1, row, n_down, TAG_U, MPI_COMM_WORLD, &stat);
            MPI_Recv(up_buf, 1, row, n_up, TAG_D, MPI_COMM_WORLD, &stat);
            MPI_Send(state, 1, row, n_up, TAG_U, MPI_COMM_WORLD);
            MPI_Send(state + (local_n-1)*n, 1, row, n_down, TAG_D, MPI_COMM_WORLD);
        }
        
        update_state(state, n, local_n, up_buf, down_buf, rt, cell_neighbors);
        current_iter++;
    }

    hm_free(rt); 
    free(state);
    free(cell_neighbors);
    free(up_buf);
    free(down_buf);
}

int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    int hasN = 0, hasI = 0;
    int n, i;
    char c = 0;
    
    while ((c = getopt(argc, argv, "n:i:")) != -1) {
        switch (c) {
            case 'n':
                n = atoi(optarg);
                hasN = true;
                break;
            case 'i':
                i = atoi(optarg);
                hasI = true;
                break;
        }
    }

    if (!hasN || !hasI) {
        printf("Missing arguments.");
        return 1;
    }

    start(n, i);
    MPI_Finalize();
    return 0;
}
