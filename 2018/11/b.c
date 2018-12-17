#include <stdio.h>

struct record {
    int x;
    int y;
    int size;
    int total;
};

int cells[300][300];
struct record records[300 * 300 * 150];

int main() {
    for (int y = 0; y < 298; y++) {
        for (int x = 0; x < 298; x++) {
            int rackId = x + 10;
            int start = rackId * y;
            int processed = (start + 6392) * rackId;
            int hundreds = processed > 99 ? (processed % 1000) / 100 : 0;
            cells[y][x] = hundreds - 5;
        }
    }

    long c = 0;

    for (int y = 0; y < 1; y++) {
        for (int x = 0; x < 1; x++) {
            int max = x > y ? x : y;
            int sum = 0;

            for (int s = 1; s < 300 - max + 1; s++) {
                sum += cells[x][y];

                for (int j = 0; j < s - 1; j++) {
                    sum += cells[j][x + s];
                }

                for (int i = 0; i < s - 1; i++) {
                    sum += cells[y + s][i];
                }

                records[c].x = x;
                records[c].y = y;
                records[c].size = s;
                records[c].total = sum;

                c++;
            }

            printf("point %d %d \n", x, y);
        }
    }

    struct record max;
    max.total = 0;

    for (int i = 0; i < 300 * 300 * 150; i++) {
        max = max.total > records[i].total ? max : records[i];
    }

    printf("x %d y %d s %d t %d", max.x, max.y, max.size, max.total);
}
