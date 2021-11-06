#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define LINE_LENGTH 32
#define BARCODE_LENGTH 10
#define NUM_ROWS 128
#define NUM_COLUMNS 8

typedef struct {
    int row;
    int column;
    int id;
} seat_t;

int parse_row(char *barcode)
{
    int min = 0;
    int max = 127;
    int n = 0;

    for (n = 0; n < 7; n++) {
        switch (barcode[n]) {
        case 'F':
            max = (max + min + 1)/2 - 1;
            break;
        case 'B':
            min = (max + min + 1)/2;
            break;
        default:
            printf("ERROR: Unknown row\n");
        }
        /* printf("%s %3d %3d\n", barcode, min, max); */
    }

    return min;
}

int parse_column(char *barcode)
{
    int min = 0;
    int max = 7;
    int n = 0;

    for (n = 7; n < 10; n++) {
        switch (barcode[n]) {
        case 'L':
            max = (max + min + 1)/2 - 1;
            break;
        case 'R':
            min = (max + min + 1)/2;
            break;
        default:
            printf("ERROR: Unknown column\n");
        }
        /* printf("%s %3d %3d\n", barcode, min, max); */
    }

    return min;
}

seat_t parse_barcode(char *barcode)
{
    seat_t s;

    s.row = parse_row(barcode);
    s.column = parse_column(barcode);
    s.id = s.row*8 + s.column;

    return s;
}

void print_seat(seat_t s)
{
    printf("row:    %5d\n", s.row);
    printf("column: %5d\n", s.column);
    printf("id:     %5d\n", s.id);
    printf("\n");
}

int main(int argc, const char *argv[]) 
{
    FILE *fp = fopen("puzzle_input.txt", "r");
    char line[LINE_LENGTH];
    int max_id = 0;
    int n, m;

    bool seats_taken[NUM_ROWS][NUM_COLUMNS];
    memset(seats_taken, false, sizeof(bool)*NUM_ROWS*NUM_COLUMNS);

    while (fgets(line, LINE_LENGTH, fp)) {
        seat_t s = parse_barcode(line);
        if (max_id < s.id)
            max_id = s.id;

        seats_taken[s.row][s.column] = true;

    }
    printf("Max id: %d\n", max_id);

    printf("Empty seats:\n");
    for (m = 0; m < NUM_ROWS; m++) {
        for (n = 0; n < NUM_COLUMNS; n++) {
            if (!seats_taken[m][n]) {
                printf("row %3d column %3d id %3d\n", m, n, 8*m+n);
            }
        }
    }

    fclose(fp);
    return 0;
}
