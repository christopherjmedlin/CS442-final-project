#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <mpi.h>
#include <time.h>
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

#define TAG_UL  10 // Receiving value for upper left corner
#define TAG_UR  20 // Receiving value for upper right corner
#define TAG_DL  30 // Receiving value for down left corner
#define TAG_DR  40 // Receiving value for down right corner
#define TAG_UP  50 // Receiving values for upper border
#define TAG_DO  60 // Receiving values for downside border
#define TAG_RI  70 // Receiving values for right border
#define TAG_LE  80 // Receiving values for left border

int get(int* main_grid, int n, int i, int j,
        int* top,
        int* ur,
        int* right,
        int* br,
        int* bottom,
        int* bl,
        int* left,
        int* ul) {
    if (i == -1 && j == -1) return *ul;
    if (i == -1 && j == n) return *ur;
    if (i == n && j == -1) return *bl;
    if (i == n && j == n) return *br;
    if (i == -1) return top[j];
    if (i == n) return bottom[j];
    if (j == -1) return left[i];
    if (j == n) return right[i];
    return main_grid[i*n+j];
}

void get_cell_neighbors(int* main_grid, int n, int i, int j, int* neighbors,
                        int* top,
                        int* ur,
                        int* right,
                        int* br,
                        int* bottom,
                        int* bl,
                        int* left,
                        int* ul) {
    neighbors[0] = get(main_grid, n, i, j-1,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[1] = get(main_grid, n, i-1, j-1,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[2] = get(main_grid, n, i-1, j,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[3] = get(main_grid, n, i-1, j+1,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[4] = get(main_grid, n, i, j+1,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[5] = get(main_grid, n, i+1, j+1,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[6] = get(main_grid, n, i+1, j,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[7] = get(main_grid, n, i+1, j-1,
                       top,
                       ur,
                       right,
                       br,
                       bottom,
                       bl,
                       left,
                       ul);
    neighbors[8] = main_grid[i*n+j];
}

/*
Updates local grid. This just makes the most sense in my brain, it feels sloppy though. 
Need to think of a way to consider the corners and edges without needing to copy the whole graph.
*/

void update_state(int* local_grid, int local_n, HashMap* rt, int* neighbors,
                  int* top,
                  int* ur,
                  int* right,
                  int* br,
                  int* bottom,
                  int* bl,
                  int* left,
                  int* ul) {
    for (int i = 0; i < local_n; i++) {
        for (int j = 0; j < local_n; j++) {
            get_cell_neighbors(local_grid, local_n, i, j, neighbors,
                               top,
                               ur,
                               right,
                               br,
                               bottom,
                               bl,
                               left,
                               ul);                    
            local_grid[i*local_n+j] = hm_lookup(rt, neighbors, 8);
        }
    }
}

/* Gets neighboring processes with with capability to wrap the grid.
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
    int rank, size;
    int* neighbors = malloc(sizeof(int) * 8);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    get_neighbors(neighbors, rank, size);
    int local_n = n / sqrt(size);
    int* local_grid = load_init_state("test2.bin", n);
    HashMap* rt = load_rule_map("gol.table");
    int* cell_neighbors = malloc(sizeof(int)*9);

    MPI_Comm graph_comm;
    MPI_Dist_graph_create_adjacent(
            MPI_COMM_WORLD, 8, neighbors, MPI_UNWEIGHTED, 
                            8, neighbors, MPI_UNWEIGHTED,
                            MPI_INFO_NULL, 0, &graph_comm);
    int counts[] = {1,local_n,1,local_n,1,local_n,1,local_n};
    int displs[] = {0,1,local_n+1,local_n+2,2*local_n+2,2*local_n+3,3*local_n+3,3*local_n+4};
    int* send = malloc(sizeof(int)*(4+4*local_n));
    int* recv = malloc(sizeof(int)*(4+4*local_n));
   
    for (int curr_iter = 0; curr_iter < i; curr_iter++) {
        // ul
        send[0] = local_grid[0];
        // ur
        send[1+local_n] = local_grid[local_n-1];
        // dr
        send[2+2*local_n] = local_grid[local_n*local_n-1];
        // dl
        send[2+2*local_n] = local_grid[local_n*local_n-1];
        for (int i = 0; i < local_n; i++) {
            // top
            send[1+i] = local_grid[i];
            // right
            send[1+local_n+1+i] = local_grid[local_n*(i+1) - 1];
            // bottom
            send[3+2*local_n+i] = local_grid[local_n*(local_n-1)+i];
            // left
            send[4+3*local_n+i] = local_grid[local_n*i];
        }
        
        MPI_Neighbor_alltoallv(send, counts, displs, MPI_INTEGER,
                               recv, counts, displs, MPI_INTEGER,
                               graph_comm);
        update_state(local_grid, local_n, rt, cell_neighbors,
                     recv + displs[1],
                     recv + displs[2],
                     recv + displs[3],
                     recv + displs[4],
                     recv + displs[5],
                     recv + displs[6],
                     recv + displs[7],
                     recv + displs[0]);
    }
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
                hasN = 1;
                break;
            case 'i':
                i = atoi(optarg);
                hasI = 1;
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
