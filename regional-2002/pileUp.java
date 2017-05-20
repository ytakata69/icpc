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
        cubeSet = new CubeSet(m);
    }

    CubeSet cubeSet;

    /**
     * Pile up cube i onto cube j.
     * @param i  The id of the cube to move (1 to m).
     * @param j  The id of the cube on which cube i should be put.
     *           Zero means the floor.
     */
    void pile(int i, int j) {
        if (i == j) { return; } // do nothing

        if (j != 0 && cubeSet.inSamePile(i, j)) {
            if (cubeSet.isAbove(i, j)) { return; } // do nothing
        }
        if (j == 0 && cubeSet.isBottom(i)) { return; } // do nothing

        cubeSet.takeOffThisAndAboveCubes(i);
        if (j != 0) {
            cubeSet.putOnto(i, j);
        }
    }

    void printResult() {
        List<Integer> heights = new ArrayList<>();
        for (int i = 1; i <= cubeSet.nCube(); i++) {
            if (cubeSet.isTop(i)) {
                heights.add(cubeSet.position(i) + 1);
            }
        }
        Collections.sort(heights);
        for (int h: heights) {
            System.out.println(h);
        }
        System.out.println("end");
    }
}

class CubeSet {
    int[] position; // position from the bottom (the bottom == 0)
    int[] bottom;   // the bottom of the pile that this cube belongs to
    int nCube;

    CubeSet(int nCube) {
        this.nCube = nCube;
        position = new int[nCube + 1];
        bottom   = new int[nCube + 1];
        for (int i = 1; i <= nCube; i++) {
            position[i] = 0;
            bottom  [i] = i;    // itself
        }
    }

    int nCube() { return nCube; }
    int position(int i) { return position[i]; }

    boolean inSamePile(int i, int j) {
        return bottom[i] == bottom[j];
    }

    boolean isAbove(int i, int j) {
        return position[i] > position[j];
    }

    boolean isTop(int i) {
        return topOfPileContaining(i) == i;
    }

    boolean isBottom(int i) {
        return position[i] == 0;
    }

    int topOfPileContaining(int i) {
        int top = i;
        for (int j = 1; j <= nCube; j++) {
            if (inSamePile(j, top) && isAbove(j, top)) {
                top = j;
            }
        }
        return top;
    }

    void takeOffThisAndAboveCubes(int i) {
        int pile = bottom[i];
        int pos  = position[i];
        for (int j = 1; j <= nCube; j++) {
            if (bottom[j] == pile && position[j] >= pos) {
                bottom  [j] = j;
                position[j] = 0;
            }
        }
    }

    void putOnto(int i, int j) {
        bottom  [i] = bottom[j];
        position[i] = position[topOfPileContaining(j)] + 1;
    }

}
