import java.util.*;

/**
 * AOJ #1120: Pile Up!
 *
 * ACM ICPC Kanazawa Contest 2002, Problem B, Oct 2002.
 * http://www.kitnet.jp/icpc/j/kakomondai/Kanazawa-yosen-problems/B.htm
 */
class Main {
    /**
     * Input format:
     *   m         -- number of cubes
     *   I1 J1     -- "put cube Ii on to cube Ji"
     *   I2 J2
     *   ...
     *   0 0
     */
    public static void main(String[] arg) {
        Scanner scanner = new Scanner(System.in);
        while (true) {
            int m = scanner.nextInt();
            if (m == 0) break;
            Masato masato = new Masato(m);
            while (true) {
                int i = scanner.nextInt();
                int j = scanner.nextInt();
                if (i == 0 && j == 0) break;
                masato.pile(i, j);
            }
            masato.printResult();
        }
    }
}

class Masato {
    /**
     * @param m  The number of cubes.
     */
    Masato(int m) {
        cube = new Cube[m + 1];
        for (int i = 1; i <= m; i++) {
            cube[i] = new Cube(i);
        }
    }

    Cube[] cube;

    /**
     * Pile up cube i onto cube j.
     * @param i  The id of the cube to move (1 to m).
     * @param j  The id of the cube on which cube i should be put.
     *           Zero means the floor.
     */
    void pile(int i, int j) {
        if (i == j) { return; } // do nothing

        if (j != 0 && cube[i].inSamePile(cube[j])) {
            if (cube[i].isAbove(cube[j])) { return; } // do nothing
        }
        if (j == 0 && cube[i].isBottom()) { return; } // do nothing

        cube[i].takeOffThisAndAboveCubes();
        if (j == 0) {
            cube[i].putOntoFloor();
        } else {
            cube[i].putOnto(cube[j]);
        }
    }

    void printResult() {
        List<Integer> heights = new ArrayList<>();
        for (int i = 1; i < cube.length; i++) {
            if (cube[i].isTop()) {
                heights.add(cube[i].pos + 1);
            }
        }
        Collections.sort(heights);
        for (int h: heights) {
            System.out.println(h);
        }
        System.out.println("end");
    }
}

class Cube {
    int id;
    int pos;        // position from the bottom (the bottom == 0)
    Cube bottom;    // the bottom of the pile that this cube belongs to
    Cube top;       // the top of ---
    Cube above;     // the next cube
    Cube below;     // the previous cube

    Cube(int id) {
        this.id     = id;
        this.bottom = this;    // on the floor
        this.top    = this;    // a singleton
        this.pos    = 0;
    }

    boolean inSamePile(Cube that) {
        return this.bottom == that.bottom;
    }

    boolean isAbove(Cube that) {
        return this.pos > that.pos;
    }

    boolean isTop() {
        return this.top == this;
    }

    boolean isBottom() {
        return this.bottom == this;
    }

    void takeOffThisAndAboveCubes() {
        for (Cube p = this; p != null; p = p.above) {
            if (p.below != null) {      // not on the floor
                p.below.above = null;   // take off
                p.below.top   = p.below;
            }
            p.below  = null;
            p.bottom = p;       // on the floor
            p.pos    = 0;
        }
    }

    void putOntoFloor() {
        this.bottom = this;
        this.below  = null;
        this.pos    = 0;
    }

    void putOnto(Cube that) {
        that.top.above = this;
        this.below     = that.top;
        this.pos       = that.top.pos + 1;
        this.bottom    = that.bottom;
        for (Cube p = this; p != null; p = p.below) {
            p.top = this;
        }
    }

}
