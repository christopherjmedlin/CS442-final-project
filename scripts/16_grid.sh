#!/bin/bash

#SBATCH --job-name=cellautomata
#SBATCH --nodes 2
#SBATCH --ntasks-per-node 8
#SBATCH --cpus-per-task 1

srun v1/a.out -n 16 -i 100 -f tests/state.16.out
srun v1/a.out -n 64 -i 100 -f tests/state.64.out 
srun v1/a.out -n 128 -i 100 -f tests/state.128.out 
srun v1/a.out -n 256 -i 100 -f tests/state.256.out 
srun v1/a.out -n 320 -i 100 -f tests/state.320.out 
