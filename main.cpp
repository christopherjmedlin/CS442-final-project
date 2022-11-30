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

struct State {
    int* main_grid;
    int* top;
    int* right;
    int* bottom;
    int* left;
};

#define NUM_COLORS (sizeof(ARR_COLORS) / sizeof(const char *)) // Get the size of ARR_COLORS

#define S_TOPLEFT "\033[H"        // Set cursor to top left
#define C_RST     "\033[0;39m"    // Reset color code to default
#define C_B_BLACK "\033[0;40m"    // Set background color black
#define C_B_WHITE "\033[0;47m"

#define TAG_UL  10 // Receiving value for upper left corner
#define TAG_UR  20 // Receiving value for upper right corner
#define TAG_DL  30 // Receiving value for down left corner
#define TAG_DR  40 // Receiving value for down right corner
#define TAG_UP  50 // Receiving values for upper border
#define TAG_DO  60 // Receiving values for downside border
#define TAG_RI  70 // Receiving values for right border
#define TAG_LE  80 // Receiving values for left border

/*
Transform the grid to allow each process to more easily access the data they want.
For example:
Original Grid: 
   [0  1  2  3
    4  5  6  7
    8  9 10 11
   12 13 14 15]

Split per process:
 rank 1   rank 2
    |0  1| |2  3|                 rank 1   rank 2   rank 3     rank 4
    |4  5| |6  7|  reorganize >> [0 1 4 5  2 3 6 7  8 9 12 13  10 11 14 15]
    
   |8   9| |10 11|
   |12 13| |14 15|
 rank 3    rank 4

 This has the potential for adjustment by using MPI_Datatypes
*/
void to_array(int* grid, int edge_length)
{
    int row, col, ng_row, ng_col, proc_offset, inner_offset;
    int* copy_grid = new int[TOTAL_GRID_SIZE]{0};
    std::copy(grid, grid+TOTAL_GRID_SIZE, copy_grid);
   
    for (int i = 0; i < TOTAL_GRID_SIZE; i++)
    {
        row = i/GRID_WIDTH;
        col = i%GRID_WIDTH;
        ng_row = row/edge_length;
        ng_col = col/edge_length;

        /* Get the offset for particular process */
        proc_offset = edge_length * (ng_row * GRID_WIDTH + ng_col * edge_length);
        /* Get the interior offset for particular process */
        inner_offset = edge_length * (row % edge_length) + (col % edge_length);

        grid[proc_offset + inner_offset] = copy_grid[i];
    }
}

/*
This is to revert the changes made from the above method to print the results. 
In theory, this only needs to be called any time we want to display what is happening in the result.

For Example, our previous "array" implementation:

 rank 0   rank 1   rank 2     rank 3                           [0  1  2  3
[0 1 4 5  2 3 6 7  8 9 12 13  10 11 14 15] >> reverts back to   4  5  6  7
                                                                8  9 10 11
                                                               12 13 14 15]
Currently, this gets called every iteration for display purposes, but if we will only
be polling the results every once and awhile, we can limit the calls to this. I'm
not sure what your plan was regarding the results, but we could potentially get an 
idea of the expected behavior of, for example, a glider, and poll it at a certain
iteration when we expect the glider to be in a certian location to ensure it's taking the
desired path. 
*/

void to_grid(int* grid, int edge_length)
{
    int box_index, box_col, box_row, inbox_offset, inbox_col, inbox_row, new_col, new_row;
    int* copy_grid = new int[TOTAL_GRID_SIZE]{0};
    std::copy(grid, grid+TOTAL_GRID_SIZE, copy_grid);

    int box_size = edge_length*edge_length;
    int boxes_per_row = GRID_WIDTH / edge_length;

    for (int i = 0; i < TOTAL_GRID_SIZE; i++)
    {
        box_index = i / box_size;
        box_col = box_index % boxes_per_row;
        box_row = box_index / boxes_per_row;

        inbox_offset = i % box_size;
        inbox_col = inbox_offset % edge_length;
        inbox_row = inbox_offset / edge_length;

        new_col = box_col * edge_length + inbox_col;
        new_row = (box_row * edge_length + inbox_row) * GRID_WIDTH;

        grid[new_row + new_col] = copy_grid[i];
    }

}

/*
Prints out the grid
*/
void draw_grid(int* grid, int edge_length)
{
    printf(S_TOPLEFT);
    for (int y = 0; y < GRID_WIDTH; y++)
    {
        for (int x = 0; x < GRID_WIDTH; x++)
        {
            if (COLOR_SUB_GRIDS)
            {
                int pi = ((int) (y/edge_length))*(GRID_WIDTH/edge_length)+((int)(x/edge_length));
                printf("%s  %s", grid[y*GRID_WIDTH+x] ? C_B_BLACK : ARR_COLORS[pi%NUM_COLORS], ARR_COLORS[pi%NUM_COLORS]);
            }
            else
            {
                printf("%s  %s", grid[y*GRID_WIDTH+x] ? C_B_BLACK : C_B_WHITE, C_B_WHITE);
            }
        }
        printf("\n");
    }
    printf(C_RST);
}

State* new_state(int n, int* main_grid) {
    State* s = (State*) malloc(sizeof(State));
    s->main_grid = main_grid;
    s->top = (int*) malloc(sizeof(int)*(n+2));
    s->bottom = (int*) malloc(sizeof(int)*(n+2));
    s->left = (int*) malloc(sizeof(int)*(n));
    s->right = (int*) malloc(sizeof(int)*(n));
    return s;
}

void free_state(State* s) {
    free(s->main_grid);
    free(s->top);
    free(s->bottom);
    free(s->left);
    free(s->right);
}

int get(State* s, int n, int i, int j) {
    if (i == -1) return s->top[j];
    if (i == n) return s->bottom[j];
    if (j == -1) return s->left[i-1];
    if (j == n) return s->right[i-1];
    return s->main_grid[i*n+j];
}

void get_cell_neighbors(State* s, int n, int i, int j, int* neighbors) {
    neighbors[0] = get(s, n, i, j-1);
    neighbors[1] = get(s, n, i-1, j-1);
    neighbors[2] = get(s, n, i-1, j);
    neighbors[3] = get(s, n, i-1, j+1);
    neighbors[4] = get(s, n, i, j+1);
    neighbors[5] = get(s, n, i+1, j+1);
    neighbors[6] = get(s, n, i+1, j);
    neighbors[7] = get(s, n, i+1, j-1);
}

/*
Updates local grid. This just makes the most sense in my brain, it feels sloppy though. 
Need to think of a way to consider the corners and edges without needing to copy the whole graph.
*/

void update_state(State* s, int local_n, HashMap* rt, int* neighbors) {
    for (int i = 0; i < local_n; i++) {
        for (int j = 0; j < local_n; j++) {
            get_cell_neighbors(s, local_n, i, j, neighbors);                    
            s->main_grid[i*local_n+j] = hm_lookup(rt, neighbors, 8);
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

int* unembed(int* old, int n) {
    int* new_grid = (int*) malloc(sizeof(int) * n * n);
    int size = (n+2) * (n+2);
    int new_size = n * n;
    for (int i = 1; i < n+1; i++) {
        for (int j = 1; j < n+1; j++) {
            new_grid[(i-1)*new_size + (j-1)] = old[i*size+j]; 
        }
    }
    free(old);
    return new_grid;
}

// embed the n*n state into a larger one (n+2)*(n+2) array (for the boundaries)
int* embed(int* old, int n) {
    int* new_grid = (int*) malloc(sizeof(int) * (n+2) * (n+2));
    int size = n * n;
    int new_size = (n+2) * (n+2);
    for (int i = 1; i < n+1; i++) {
        for (int j = 1; j < n+1; j++) {
            new_grid[i*(n+2) + j] = old[(i-1)*(n+2) + j-1];
        }
    }

    free(old);
    return new_grid;
}

void start(int n, int i) {
    int size, rank;
    MPI_Status stat;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    HashMap* rt = load_rule_map("utils/genlife/gol.table");
    int* init_state = load_init_state("test2.bin", n);
    int current_iter = 0;
    int* neighbors = (int*) malloc(sizeof(int) * 8);
    int* cell_neighbors = (int*) malloc(sizeof(int) * 8);
    int local_n = n / sqrt(size);
    State* state = new_state(local_n, init_state);
    get_neighbors(neighbors, rank, size);

    MPI_Request requests[] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
    MPI_Status stats[] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
    
    // init datatypes
    MPI_Datatype row, col;
    MPI_Type_contiguous(local_n, MPI_INTEGER, &row);
    MPI_Type_vector(local_n, 1, local_n, MPI_INTEGER, &col);
    MPI_Type_commit(&row);
    MPI_Type_commit(&col);
    
    while (current_iter < i) {
        MPI_Isend(state->main_grid, 1, row, neighbors[1], TAG_UP,
                  MPI_COMM_WORLD, &(requests[0]));
        MPI_Isend(state->main_grid+local_n-1, 1, MPI_INTEGER, neighbors[2], TAG_UR,
                  MPI_COMM_WORLD, &(requests[1]));
        MPI_Isend(state->main_grid+local_n-1, 1, col, neighbors[3], TAG_RI,
                  MPI_COMM_WORLD, &(requests[2]));
        MPI_Isend(state->main_grid+(local_n*local_n)-1, 1, MPI_INTEGER, neighbors[4], TAG_DR,
                  MPI_COMM_WORLD, &(requests[3]));
        MPI_Isend(state->main_grid+local_n*(local_n-1), 1, row, neighbors[5], TAG_DO,
                  MPI_COMM_WORLD, &(requests[4]));
        MPI_Isend(state->main_grid+local_n*(local_n-1), 1, MPI_INTEGER, neighbors[6], TAG_DL,
                  MPI_COMM_WORLD, &(requests[5]));
        MPI_Isend(state->main_grid, 1, col, neighbors[7], TAG_LE,
                  MPI_COMM_WORLD, &(requests[6]));
        MPI_Isend(state->main_grid, 1, MPI_INTEGER, neighbors[0], TAG_UL,
                  MPI_COMM_WORLD, &(requests[7]));

        MPI_Recv(state->top+1, 1, row, neighbors[1],
                 TAG_DO, MPI_COMM_WORLD, &stat);
        MPI_Recv(state->top+local_n+1, 1, MPI_INTEGER, neighbors[2],
                 TAG_DL, MPI_COMM_WORLD, &stat);
        MPI_Recv(state->right, 1, row, neighbors[3],
                 TAG_LE, MPI_COMM_WORLD, &stat);
        MPI_Recv(state->bottom+local_n+1, 1, MPI_INTEGER, neighbors[4],
                 TAG_UL, MPI_COMM_WORLD, &stat);
        MPI_Recv(state->bottom+1, 1, row, neighbors[5],
                 TAG_UP, MPI_COMM_WORLD, &stat);
        MPI_Recv(state->bottom, 1, MPI_INTEGER, neighbors[6],
                 TAG_UR, MPI_COMM_WORLD, &stat);
        MPI_Recv(state->left, 1, col, neighbors[7],
                 TAG_RI, MPI_COMM_WORLD, &stat);
        MPI_Recv(state->top, 1, MPI_INTEGER, neighbors[0],
                TAG_DR, MPI_COMM_WORLD, &stat);

        MPI_Waitall(8, requests, stats);
        update_state(state, local_n, rt, cell_neighbors);
        current_iter++;
    }

    
    printf("how did I get here?");
    return;
/*
    hm_free(rt); 
    free(state);
    free(neighbors);
*/
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
