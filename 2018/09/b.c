#include <stdio.h>
#include <stdlib.h>

typedef unsigned int u32;

struct Node {
    struct Node* prev;
    u32 value;
    struct Node* next;
};

struct Node* insert_node(struct Node* node, u32 value);
struct Node* score(struct Node* node, u32 turn, u32* scores);

u32 players = 413;
u32 marbles_amount = 7108200;

u32 main() {
    u32 scores[players];

    for (u32 i = 0; i < players; i++)
        scores[i] = 0;

    struct Node first;
    first.prev = &first;
    first.value = 0;
    first.next = &first;

    struct Node* current;
    current = &first;

    for (u32 i = 1; i <= marbles_amount; i++) {
        if (i % 23 == 0) {
            current = score(current, i, scores);
        } else {
            current = insert_node(current, i);
        }
    }

    /*current = &first;

    for (u32 i = 1; i <= marbles_amount + 1; i++) {
        printf("%d ", current->value);
        current = current->next;
    }*/

    u32 max = 0;

    for (u32 i = 0; i < players; i++)
        if (scores[i] > max)
            max = scores[i];

    printf("%u", max);
}

struct Node* insert_node(struct Node* node, u32 value) {
    struct Node* new_node = malloc(sizeof(struct Node));
    struct Node* pivot = node->next;

    new_node->prev = pivot;
    new_node->value = value;
    new_node->next = pivot->next;

    pivot->prev = node;
    pivot->next = new_node;

    return new_node;
}

struct Node* score(struct Node* node, u32 turn, u32* scores) {
    u32 current_player = turn % players;
    scores[current_player] += turn;

    struct Node* current;
    for (u32 i = 0; i < 7; i++) {
        current = current->prev;
    }
    scores[current_player] += current->value;

    current->prev->next = current->next;

    return current->next;
}
