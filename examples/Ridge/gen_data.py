from numpy import *

# Produce between -0.5 and 0.5
def rand1():
    return random.random() - 0.5

def main():
    n = 40
    xs = [rand1() * 20 for i in range(n)]
    ys = [rand1() * 10 for i in range(n)]
    zs = []
    for i in range(n):
        k1 = 0.5
        k2 = 2.4
        k0 = 7.8
        zs.append(k1*xs[i] + k2*ys[i] + k0 + 0.2*rand1())

    zs = array(zs)

    x = reshape(array(xs + ys), (2, n))
    x1 = x.transpose()

    savetxt("in.txt", x1)
    savetxt("out.txt", zs)

if __name__ == '__main__':
    main()
