#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct vec2 {
    int x;
    int y;
};

struct point_list {
    struct vec2 data[1000];
    int length;
};

struct area {
    int value;
    struct vec2 from;
};

struct map {
    int width;
    int height;
    struct area **areas;
};

struct unit {
    char tag;
    struct vec2 position;
};

struct unit_list {
    struct unit data[1000];
    int length;
};

struct cell {
    char tag;
    int hp;
    int attack;
};

struct vec2 measure();
struct cell **load_map(int width, int height);
void render(struct cell **map, int width, int height);
void render_canvas(struct map *canvas);
void clean_canvas(struct cell **main_map, struct map *canvas);
void points_out(struct map *canvas, int mark, struct point_list *src, struct point_list *dst);
void points_out(struct map *canvas, int mark, struct point_list *src, struct point_list *dst);
void list_copy(struct point_list *src, struct point_list *dst);
int touches(struct cell **main_map, char chr, struct vec2 point);
int any_touches(struct cell **main_map, char chr, struct point_list *points);
void filter_touches(struct cell **main_map, char chr, struct point_list *src, struct point_list *dst);
struct vec2 minimum_point(struct point_list *points);
struct vec2 move(struct cell **main_map, struct unit this_unit, struct map *canvas);
void load_units(struct cell ** main_map, int width, int height, struct unit_list *units);
void delete_unit(int index, struct unit_list *units);
int has_enemies(struct unit u, struct unit_list *units);
struct vec2 get_target(struct cell **main_map, struct unit this_unit, int *success);
int index_of_unit(struct vec2 position, struct unit_list *units);

int main() {
    // Load the map
    struct vec2 size = measure();
    int width = size.x;
    int height = size.y;
    struct cell **main_map = load_map(width, height);

    struct unit_list units;

    // Allocate the canvas
    // A util matrix for the shortest path algorithm
    struct map canvas;
    canvas.width = width;
    canvas.height = height;
    canvas.areas = malloc(sizeof(struct area *) * height);
    for (int y = 0; y < height; y++)
        canvas.areas[y] = malloc(sizeof(struct area) * width);

    int rounds = 0;

    int i = 0;
    load_units(main_map, width, height, &units);

    for (;;) {
        struct unit current_unit = units.data[i];

        if (!has_enemies(current_unit,  &units))
            break;

        clean_canvas(main_map, &canvas);
        struct vec2 new_position = move(main_map, current_unit, &canvas);

        // Swap
        struct cell cell_a = main_map[current_unit.position.y][current_unit.position.x];
        struct cell cell_b = main_map[new_position.y][new_position.x];
        main_map[current_unit.position.y][current_unit.position.x] = cell_b;
        main_map[new_position.y][new_position.x] = cell_a;

        struct unit moved_unit;
        moved_unit.tag = main_map[new_position.y][new_position.x].tag;
        moved_unit.position = new_position;

        // Attack
        int success = 0;
        struct vec2 target = get_target(main_map, moved_unit, &success);

        if (success) {
            main_map[target.y][target.x].hp -= main_map[new_position.y][new_position.x].attack;

            if (main_map[target.y][target.x].hp <= 0) {
                main_map[target.y][target.x].tag = '.';
                int index = index_of_unit(target, &units);
                delete_unit(index, &units);

                // Item removed from list, so go back a step
                if (index <= i)
                    i--;
            }
        }

        i++;

        if (i >= units.length) {
            i = 0;
            load_units(main_map, width, height, &units);
            rounds++;
        }
    }
    
    render(main_map, width, height);

    int hp = 0;
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (main_map[y][x].tag == 'E' || main_map[y][x].tag == 'G') {
                hp += main_map[y][x].hp;
                printf("%d ", main_map[y][x].hp);
            }
        }
    }

    int elves = 0;
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (main_map[y][x].tag == 'E') {
                elves++;
            }
        }
    }

    printf("\n");
    printf("%d rounds, %d hp, %d total, %d elves", rounds, hp, rounds * hp, elves);

    return 0;
}

struct vec2 measure() {
    FILE *file = fopen("input.txt", "r");
    char c;
    struct vec2 size;
    int width = 0;
    int height = 0;

    while (c != EOF) {
        c = fgetc(file);

        if (c != '\n') {
            if (height == 0)
                width++;
        }
        else {
            height++;
        }
    }

    size.x = width;
    size.y = height;

    return size;
}

struct cell **load_map(int width, int height) {
    FILE *file = fopen("input.txt", "r");
    char c;

    struct cell **map = malloc(sizeof(struct cell *) * height);
    map[0] = malloc(sizeof(struct cell) * width);
    int x = 0;
    int y = 0;

    while (c != EOF) {
        c = fgetc(file);

        if (c != '\n') {
            map[y][x].tag = c;
            map[y][x].hp = 200;
            map[y][x].attack = c == 'E' ? 40 : 3;
            x++;
        }
        else {
            x = 0;
            y++;
            map[y] = malloc(sizeof(struct cell) * width);
        }
    }

    return map;
}

void render(struct cell **map, int width, int height) { 
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            printf("%c", map[y][x].tag);
        }

        printf("\n");
    }
}

void render_canvas(struct map *canvas) { 
    for (int y = 0; y < canvas->height; y++) {
        for (int x = 0; x < canvas->width; x++) {
            printf("%3d", canvas->areas[y][x].value);
        }

        printf("\n");
    }
}

void clean_canvas(struct cell **map, struct map *canvas) {
    for (int y = 0; y < canvas->height; y++) {
        for (int x = 0; x < canvas->width; x++) {
            struct area *this_area = &canvas->areas[y][x];

            if (map[y][x].tag == '.')
                this_area->value = 0;
            else
                this_area->value = -1;

            this_area->from = (struct vec2) { 0, 0 };
        }
    }
}

void point_out(struct vec2 point, int mark, struct map *canvas) {
    int x = point.x;
    int y = point.y;

    if (canvas->areas[y][x + 1].value == 0) {
        canvas->areas[y][x + 1].value = mark;
        canvas->areas[y][x + 1].from = point;
    }

    if (canvas->areas[y + 1][x].value == 0) {
        canvas->areas[y + 1][x].value = mark;
        canvas->areas[y + 1][x].from = point;
    }

    if (canvas->areas[y][x - 1].value == 0) {
        canvas->areas[y][x - 1].value = mark;
        canvas->areas[y][x - 1].from = point;
    }

    if (canvas->areas[y - 1][x].value == 0) {
        canvas->areas[y - 1][x].value = mark;
        canvas->areas[y - 1][x].from = point;
    }
}

void points_out(struct map *canvas, int mark, struct point_list *src, struct point_list *dst) {
    int c = 0;
    
    for (int i = 0; i < src->length; i++) {
        point_out(src->data[i], mark, canvas);
    }

    // Collect
    for (int y = 0; y < canvas->height; y++) {
        for (int x = 0; x < canvas->width; x++) {
            if (canvas->areas[y][x].value == mark) {
                struct vec2 point = { x, y };
                dst->data[c] = point;
                c++;
            }
        }
    }

    dst->length = c;
}

void list_copy(struct point_list *dst, struct point_list *src) {
    for (int i = 0; i < src->length; i++) {
        dst->data[i] = src->data[i];
    }

    dst->length = src->length;
}

int touches(struct cell **main_map, char chr, struct vec2 point) {
    if (main_map[point.y][point.x + 1].tag == chr)
        return 1;
    if (main_map[point.y + 1][point.x].tag == chr)
        return 1;
    if (main_map[point.y][point.x - 1].tag == chr)
        return 1;
    if (main_map[point.y - 1][point.x].tag == chr)
        return 1;

    return 0;
}

int any_touches(struct cell **main_map, char chr, struct point_list *points) {
    for (int i = 0; i < points->length; i++)
        if (touches(main_map, chr, points->data[i]))
            return 1;

    return 0;
}

void filter_touches(struct cell **main_map, char chr, struct point_list *src, struct point_list *dst) {
    int c = 0;

    for (int i = 0; i < src->length; i++) {
        if (touches(main_map, chr, src->data[i])) {
            dst->data[c] = dst->data[i];
            c++;
        }
    }

    dst->length = c;
}

struct vec2 minimum_point(struct point_list *points) {
    struct vec2 min_point;
    int min_value = 1000000;

    for (int i = 0; i < points->length; i++) {
        struct vec2 point = points->data[i];
        int value = point.y * 1000 + point.x;
        if (value < min_value) {
            min_value = value;
            min_point = point;
        }
    }

    return min_point;
}

struct vec2 move(struct cell **main_map, struct unit this_unit, struct map *canvas) {
    char enemy_tag =  this_unit.tag == 'E' ? 'G' : 'E';

    if (touches(main_map, enemy_tag, this_unit.position))
        return this_unit.position;

    struct point_list points;
    points.data[0] = this_unit.position;
    points.length = 1;

    struct point_list buffer;
    buffer.length = 0;

    // Shortest paths
    // Given some points, mark all available adjacent points with distance + 1
    // return those points, and repeat with those points
    // Stop when hit enemy or empty
    int mark = 1;
    while (!any_touches(main_map, enemy_tag, &points) && points.length != 0) {
        points_out(canvas, mark, &points, &buffer);
        list_copy(&points, &buffer);
        mark++;
    }

    if (points.length == 0)
        return this_unit.position;

    // From all shortest paths, the one of the first point
    filter_touches(main_map, enemy_tag, &points, &buffer);
    list_copy(&points, &buffer);
    struct vec2 last = minimum_point(&points);
    
    // The start of the path
    struct area path;
    for (int i = 2; i < mark; i++) {
        path = canvas->areas[last.y][last.x];
        last = path.from;
    }

    return last;
}

void load_units(struct cell **map, int width, int height, struct unit_list *units) {
    int c = 0;

    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (map[y][x].tag == 'E') {
                units->data[c] = (struct unit) { 'E', { x,  y } };
                c++;
            } else if (map[y][x].tag == 'G') {
                units->data[c] = (struct unit) { 'G', { x,  y } };
                c++;
            }
        }
    }

    units->length = c;
}

void delete_unit(int index, struct unit_list *units) {
    for (int i = 0; i < units->length - 1; i++) {
        if (i >= index) {
            units->data[i] = units->data[i + 1];
        }
    }

    units->length -= 1;
}

int has_enemies(struct unit u, struct unit_list *units) {
    char enemy = u.tag == 'E' ? 'G' : 'E';

    for (int i = 0; i < units->length; i++) {
        if (units->data[i].tag == enemy) {
            return 1;
        }
    }

    return 0;
}

struct vec2 get_target(struct cell **main_map, struct unit this_unit, int *success) {
    struct cell current;
    struct vec2 p = this_unit.position;
    char enemy = this_unit.tag == 'E' ? 'G' : 'E';
    int min_hp = 200;
    struct vec2 target;

    *success = 0;

    current = main_map[p.y + 1][p.x];
    if (current.tag == enemy && current.hp <= min_hp) {
        *success = 1;
        min_hp = current.hp;
        target = (struct vec2) { p.x, p.y + 1 };
    }

    current = main_map[p.y][p.x + 1];
    if (current.tag == enemy && current.hp <= min_hp) {
        *success = 1;
        min_hp = current.hp;
        target = (struct vec2) { p.x + 1, p.y };
    }

    current = main_map[p.y][p.x - 1];
    if (current.tag == enemy && current.hp <= min_hp) {
        *success = 1;
        min_hp = current.hp;
        target = (struct vec2) { p.x - 1, p.y };
    }

    current = main_map[p.y - 1][p.x];
    if (current.tag == enemy && current.hp <= min_hp) {
        *success = 1;
        min_hp = current.hp;
        target = (struct vec2) { p.x, p.y - 1 };
    }

    return target;
}

int index_of_unit(struct vec2 position, struct unit_list *units) {
    for (int i = 0; i < units->length; i++) {
        struct vec2 this_pos = units->data[i].position;
        if (this_pos.x == position.x && this_pos.y == position.y) {
            return i;
        }
    }

    return -1;
}
