#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>

#define LINE_LENGTH 128
#define NUM_QUESTIONS 26
#define MAX_NUM_GROUPS 1024

int count_bits(uint32_t mask)
{
    int i;
    int n = 0;

    for (i = 0; i < NUM_QUESTIONS; i++) {
        if ((mask >> i) & 1) {
            n += 1;
        }
    }
    return n;
}
uint32_t get_letter_mask(char *line)
{
    char c;
    int i;
    int n;
    uint32_t mask = 0;

    i = 0;
    while ((c = line[i++]) != '\n') {
        n = c-'a';
        mask |= (1 << n);
    }
    /* printf("%5s %04x\n", line, mask); */
    return mask;
}

int main(int argc, const char *argv[]) 
{
    /* FILE *fp = fopen("test_input.txt", "r"); */
    FILE *fp = fopen("puzzle_input.txt", "r");
    char line[LINE_LENGTH];
    uint32_t letter_mask_anyone = 0;
    uint32_t letter_mask_everyone = 0xffffffff;
    uint32_t letter_mask_person = 0;
    int i;
    int n;

    int num_answers_anyone[MAX_NUM_GROUPS];
    int num_answers_everyone[MAX_NUM_GROUPS];
    int group_count = 0;

    while (fgets(line, LINE_LENGTH, fp)) {
        letter_mask_person = get_letter_mask(line);

        if (letter_mask_person == 0) {
            num_answers_anyone[group_count] = count_bits(letter_mask_anyone);
            num_answers_everyone[group_count] = count_bits(letter_mask_everyone);
            group_count += 1;
            letter_mask_anyone = 0;
            letter_mask_everyone = 0xffffffff;
        }
        else {
            letter_mask_anyone |= letter_mask_person;
            letter_mask_everyone &= letter_mask_person;
        }
    }
    num_answers_anyone[group_count] = count_bits(letter_mask_anyone);
    num_answers_everyone[group_count] = count_bits(letter_mask_everyone);
    group_count += 1;

    int sum_anyone = 0;
    int sum_everyone = 0;
    for (i = 0; i < group_count; i++) {
        sum_anyone += num_answers_anyone[i];
        sum_everyone += num_answers_everyone[i];

        /* printf("%3d: %3d\n", i, num_answers_anyone[i]); */
        /* printf("%3d: %3d\n", i, num_answers_everyone[i]); */
    }
    printf("Task 1 (sum with anyone):   %4d\n", sum_anyone);
    printf("Task 2 (sum with everyone): %4d\n", sum_everyone);

    fclose(fp);
    return 0;
}
