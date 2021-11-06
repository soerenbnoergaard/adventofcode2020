#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

/* #define PREAMBLE_LENGTH 5 */
/* #define INPUT_FILE "test_input.txt" */

#define PREAMBLE_LENGTH 25
#define INPUT_FILE "puzzle_input.txt"

vector<int> load_file(const char *filename)
{
    ifstream infile(filename);
    vector<int> v;
    int x;

    while (infile >> x) {
        v.push_back(x);
    }
    return v;
}

void print_vector(const vector<int> v)
{
    int i;
    for (i = 0; i < v.size(); i++) {
        cout << v[i] << endl;
    }
}

int find_bad_value(const vector<int> v, int preamble)
{
    int i, j, k;
    for (i = preamble; i < v.size(); i++) {
        // Check if the sum of any two numbers in the preamble equals the current value
        bool found = false;
        for (j = i-preamble; j < i; j++) {
            for (k = i-preamble; k < i; k++) {
                if (j == k) {
                    continue;
                }
                else if (v[j] + v[k] == v[i]) {
                    found = true;
                    break;
                }
            }

            if (found) {
                break;
            }
        }

        if (found) {
            cout << "Found match for v[" << i << "] = " << v[i] << endl;
        }
        else {
            cout << "No match for v[" << i << "] = " << v[i] << endl;
            return v[i];
        }
    }
    return 0;
}

int find_encryption_weakness(vector<int> v, int bad_value)
{
    int start, stop;
    int sum, min, max;

    for (start = 0; start < v.size(); start++) {
        sum = v[start];
        min = v[start];
        max = v[start];

        for (stop = start+1; stop < v.size(); stop++) {
            sum += v[stop];
            if (min > v[stop])
                min = v[stop];
            if (max < v[stop])
                max = v[stop];

            if (sum == bad_value) {
                cout << "start: v[" << start << "] = " << v[start] << endl;
                cout << "stop:  v[" << stop << "] = " << v[stop] << endl;
                cout << "sum: " << sum << endl;
                cout << "min: " << min << endl;
                cout << "max: " << max << endl;
                return min+max;
            }

        }
    }
    return -1;
}

int main(int argc, const char *argv[]) 
{

    vector<int> v = load_file(INPUT_FILE);
    // print_vector(v);
    int bad_value = find_bad_value(v, PREAMBLE_LENGTH);
    int weakness = find_encryption_weakness(v, bad_value);
    cout << "Weakness: " << weakness << endl;
}
