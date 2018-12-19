#include <stdio.h>

int cells[300][300];
int totals[300 * 300];

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

    int c = 0;

    for (int y = 0; y < 298; y++) {
        for (int x = 0; x < 298; x++) {
            for (int i = 0; i < 3; i++) {
                for (int j = 0; j < 3; j++) {
                    totals[c] += cells[y + i][x + j];
                }
            }

            c++;
        }
    }

    int max = 0;

    for (int i = 0; i < 300 * 300; i++) {
        max = max > totals[i] ? max : totals[i];
    }

    printf("%d", max);
}
