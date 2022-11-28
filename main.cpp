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

/*
Updates local grid. This just makes the most sense in my brain, it feels sloppy though. 
Need to think of a way to consider the corners and edges without needing to copy the whole graph.
*/

void update_local_grid(int* g, int width, int ul, int ur, int dl, int dr, int* ups, int* downs, int* lefts, int* rights)
{
    int size = (width+2)*(width+2);
    int* cg = new int[size];

    /* Builds local portion of grid while considering the neighboring processes corner/edge values */
    for (int y = 0; y < (width + 2); y++)
    {
        for (int x = 0; x < (width + 2); x++)
        {
            if (x == 0 && y == 0) // Upper left corner
                cg[0] = ul;
            else if (x == 0 && y == width + 1)         // Lower left corner
                cg[size-(width+2)] = dl;
            else if (x == 0)                           // Left edge
                cg[y*(width+2)] = lefts[y-1];
            else if (y == 0 && x == width + 1)         // Upper right corner
                cg[width+1] = ur; 
            else if (x == width + 1 && y == width + 1) // Lower right corner
                cg[size-1] = dr;
            else if (y == width + 1)                   // Lower edge
                cg[size-(width+2)+x] = downs[x-1];
            else if (y == 0)                           // Upper edge
                cg[x] = ups[x-1];
            else if (x == width + 1)                   // Right edge
                cg[(y+1)*(width+2)-1] = rights[y-1];
            else                                       // Local grid
                cg[y*(width+2)+x] = g[(y-1)*width+(x-1)];
        }
    }
    /* checks each individual cell in local portion to see if it should die or live. */
    int s;
    for (int y = 1; y <= width; y++)
    {
        for (int x = 1; x <= width; x++)
        {
            /* Sum up values of neighboring cells to check for death */
            s = cg[(y-1)*(width+2)+x-1] + // Upper left corner
                cg[(y-1)*(width+2)+x] +   // Upper edge
                cg[(y-1)*(width+2)+x+1] + // Upper right corner
                cg[y*(width+2)+x-1] +     // Left edge
                cg[y*(width+2)+x+1] +     // Right edge
                cg[(y+1)*(width+2)+x-1] + // Lower left corner
                cg[(y+1)*(width+2)+x] +   // Lower edge
                cg[(y+1)*(width+2)+x+1];  // Lower right corner
            /* Apply GOL rules */
            if (s < 2 || s > 3 || (s == 2 && cg[y*(width+2)+x] == 0))
                g[(y-1)*width+(x-1)] = 0; // kill the cell
            else
                g[(y-1)*width+(x-1)] = 1; // let the cell live
        }
    }
    delete [] cg;
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
    neighbors[3] = (col)? rank-1 : rank+ppl-1 ; // left
    neighbors[4] = (col<(ppl-1))? rank+1 : rank-ppl+1; // right
    neighbors[5] = (col && (row<(ppl-1)))? rank+ppl-1 : (col? col-1 : ((row<(ppl-1))? ((row+2)*ppl)-1  : ppl-1 )); // down left
    neighbors[6] = (row<(ppl-1))? rank+ppl : col; // down
    neighbors[7] = (row<(ppl-1) && col<(ppl-1))? rank+ppl+1 : ((row<(ppl-1))? rank+1 : ((col<(ppl-1))? col+1: 0));// down right
}

int main(int argc, char* argv[])
{
    int size, rank;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    const int local_grid_size = TOTAL_GRID_SIZE / size;
    const int local_edge_length = (int)sqrt(local_grid_size); // Length of one "edge" of the local grid

    int* local_grid = new int[local_grid_size];
    int* grid = new int[TOTAL_GRID_SIZE]{0};

    HashMap* rt = load_rule_map("utils/genlife/gol.table");
    int* init_state = load_init_state("test2.bin", 4);

    if (rank == 0)
    {
        if (!START_RANDOM) // Start with a glider if we don't want a random start
        {
            grid[GRID_WIDTH+3] = 1;
            grid[GRID_WIDTH*2+1] = 1;
            grid[GRID_WIDTH*2+3] = 1;
            grid[GRID_WIDTH*3+2] = 1;
            grid[GRID_WIDTH*3+3] = 1;
        } 
        else // Randomly fill the grid in with either 1's or 0's
        {
            srand(time(NULL));
            for (int i = 0; i < TOTAL_GRID_SIZE; i++)
            {
                grid[i] = rand() % 2;
            }
        }

        to_array(grid, local_edge_length); // Reorganizes the "grid" contents to more easily distribute the data.
    }

    /* 
    Distribute the "local" portions of the grid to all the processes. 
    This and the above "to_array" call can likely be negated with MPI_datatype implementaions. 
    */
    MPI_Scatter(grid, local_grid_size, MPI_INT, local_grid, local_grid_size, MPI_INT, 0, MPI_COMM_WORLD);

    printf("[%d][%d]: Local grid size = %dx%d\n",rank,size,local_edge_length,local_edge_length);
    MPI_Barrier(MPI_COMM_WORLD);

    int* neighbors = new int[8]{0};
    get_neighbors(neighbors, rank, size); // Determine neighboring processes

    /* Upper left corner, Upper right corner, lower left corner, lower right corner*/
    int u_left, u_right, d_left, d_right; 
    int* u_edge = new int[local_edge_length]; // Upper edge to send
    int* d_edge = new int[local_edge_length]; // Lower edge to send
    int* l_edge = new int[local_edge_length]; // Left edge to send
    int* r_edge = new int[local_edge_length]; // Right edge to send
    int* u_edge_recv = new int[local_edge_length]; // Recv buffer for Upper edge
    int* d_edge_recv = new int[local_edge_length]; // Recv buffer for lower edge
    int* l_edge_recv = new int[local_edge_length]; // Recv buffer for left edge
    int* r_edge_recv = new int[local_edge_length]; // Recv buffer for right edge

    /* Super lax timing stuff just to have it */
    double start, end;
    MPI_Barrier(MPI_COMM_WORLD);
    start = MPI_Wtime();

    for (int gen = 0; gen < N_GENERATIONS; gen++) // Loop through number of generations
    {
        MPI_Barrier(MPI_COMM_WORLD);
        /* Gather portions of grid */
        MPI_Gather(local_grid, local_grid_size, MPI_INT, grid, local_grid_size, MPI_INT, 0, MPI_COMM_WORLD);

        if (rank == 0 && gen%81 == 0) // Have grid printed out. Once again, could poll this less often
        {
            to_grid(grid, local_edge_length);
            draw_grid(grid, local_edge_length);
            printf("Generation %d|%d\n",gen, N_GENERATIONS-1);
        }
        
        /* Get edge values from local grid */
        for (int i = 0; i < local_edge_length; i++)
        {
            u_edge[i] = local_grid[i];
            l_edge[i] = local_grid[i*local_edge_length];
            r_edge[i] = local_grid[(i+1)*local_edge_length-1];
            d_edge[i] = local_grid[local_grid_size-local_edge_length+i];
        }

        /* Send local corners to other processes */
        MPI_Send(&local_grid[0], 1, MPI_INT, neighbors[0], TAG_DR, MPI_COMM_WORLD); // Send upper left corner to lower right neighbor
        MPI_Send(&local_grid[local_edge_length-1], 1, MPI_INT, neighbors[2], TAG_DL, MPI_COMM_WORLD);  // Send upper right corner to lower left neighbor
        MPI_Send(&local_grid[local_grid_size-local_edge_length], 1, MPI_INT, neighbors[5], TAG_UR, MPI_COMM_WORLD); // Send lower left corner to upper right neighbor
        MPI_Send(&local_grid[local_grid_size - 1], 1, MPI_INT, neighbors[7], TAG_UL, MPI_COMM_WORLD); // Send lower right corner to upper left neighbor
/*
        for (int i = 0; i < local_edge_length; i++)
        {
            u_edge[i] = local_grid[i];
            l_edge[i] = local_grid[i*local_edge_length];
            r_edge[i] = local_grid[(i+1)*local_edge_length-1];
            d_edge[i] = local_grid[local_grid_size-local_edge_length+i];
        }
*/
        /* Send edge values to other processes */
        MPI_Send(u_edge, local_edge_length, MPI_INT, neighbors[1], TAG_DO, MPI_COMM_WORLD); // Send upper edge to lower neighbor
        MPI_Send(l_edge, local_edge_length, MPI_INT, neighbors[3], TAG_RI, MPI_COMM_WORLD); // Send left edge to right neighbor
        MPI_Send(r_edge, local_edge_length, MPI_INT, neighbors[4], TAG_LE, MPI_COMM_WORLD); // Send right edge to left neighbor
        MPI_Send(d_edge, local_edge_length, MPI_INT, neighbors[6], TAG_UP, MPI_COMM_WORLD); // Send lower edge to upper neighbor

        /* Recv corner values from other processes */
        MPI_Recv(&u_left, 1, MPI_INT, neighbors[0], TAG_UL, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Upper left corner value
        MPI_Recv(&u_right, 1, MPI_INT, neighbors[2], TAG_UR, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Upper right corner value
        MPI_Recv(&d_left, 1, MPI_INT, neighbors[5], TAG_DL, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Lower left corner value
        MPI_Recv(&d_right, 1, MPI_INT, neighbors[7], TAG_DR, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Lower right corner value
        
        MPI_Recv(u_edge_recv, local_edge_length, MPI_INT, neighbors[1], TAG_UP, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Upper edge
        MPI_Recv(d_edge_recv, local_edge_length, MPI_INT, neighbors[6], TAG_DO, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Lower edge
        MPI_Recv(l_edge_recv, local_edge_length, MPI_INT, neighbors[3], TAG_LE, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Left edge
        MPI_Recv(r_edge_recv, local_edge_length, MPI_INT, neighbors[4], TAG_RI, MPI_COMM_WORLD, MPI_STATUS_IGNORE); // Recv Right edge

        /* Copy recv buffer into send buffer for use in next iteration */
        std::copy(u_edge_recv, u_edge_recv+local_edge_length, u_edge);
        std::copy(d_edge_recv, d_edge_recv+local_edge_length, d_edge);
        std::copy(r_edge_recv, r_edge_recv+local_edge_length, r_edge);
        std::copy(l_edge_recv, l_edge_recv+local_edge_length, l_edge);

        /* Set local grid up for next iteration of game of life based on neighboring edges/corners */
        update_local_grid(local_grid, local_edge_length, u_left, u_right, d_left, d_right, u_edge, d_edge, l_edge, r_edge);

        /* Delay for printing the results more slowly */
        usleep(GEN_DELAY_MS*1000);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    end = MPI_Wtime(); // end timer

    printf("Runtime: %f\n",end-start);

    delete [] local_grid;
    delete [] grid;
    delete [] neighbors;
    delete [] u_edge;
    delete [] d_edge;
    delete [] r_edge;
    delete [] l_edge;
    delete [] u_edge_recv;
    delete [] d_edge_recv;
    delete [] r_edge_recv;
    delete [] l_edge_recv;

    MPI_Finalize();
    return 0;
}
