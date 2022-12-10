import matplotlib.pyplot as plot
import re

def get_ns(f):
    f.seek(0)
    ls = f.readlines()
    ns = []
    for line in ls[::2]:
        nums = re.findall("\d+", line)
        n = int(nums[0])
        ns.append(n)
    return ns;

def plot_timings(f, name):
    ls = f.readlines()
    ns = get_ns(f)
    times = []
    for line in ls[::2]:
        nums = re.findall("(\d+\.\d+)|(\d+)", line)
        time = nums[1][0]
        times.append(time)
    plot.xlabel("n")
    plot.ylabel("time/iteration")
    plot.plot(ns, list(map(float, times)), 
              '-o', label=name)

def plot_io(f, name):
    ls = f.readlines()
    ns = get_ns(f)
    times = []
    for line in ls[1::2]:
        nums = re.findall("\d+\.\d+", line)
        times.append(nums[0])
    plot.xlabel("n")
    plot.ylabel("i/o time")
    plot.plot(ns, list(map(float, times)),
              '-o', label=name);
    
if __name__ == "__main__":
    plot_timings(open("timings/64_grid.txt"), "grid")    
    plot_timings(open("timings/64_row.txt"), "row")
    plot.legend()
    plot.savefig("coms64.png")
    plot.close()
    plot_io(open("timings/64_grid.txt"), "grid")
    plot_io(open("timings/64_row.txt"), "row")
    plot.legend()
    plot.savefig("io64.png")
    plot.close()
    plot_timings(open("timings/16_grid.txt"), "grid")    
    plot_timings(open("timings/16_row.txt"), "row")
    plot.legend()
    plot.savefig("coms16.png")
    plot.close()
    plot_io(open("timings/16_grid.txt"), "grid")
    plot_io(open("timings/16_row.txt"), "row")
    plot.legend()
    plot.savefig("io16.png")
    plot.close()
