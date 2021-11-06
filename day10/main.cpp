#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <map>
#include <cstring>

using namespace std;

/* #define INPUT_FILE "test_input.txt" */
/* #define INPUT_FILE "test_input2.txt" */
/* #define INPUT_FILE "test_input3.txt" */
#define INPUT_FILE "puzzle_input.txt"

typedef uint64_t big_uint_t;

class AdjacencyMatrix {
private:
    void alloc(int n)
    {
        int i;
        size = n;
        mat = (big_uint_t **)calloc(size, sizeof(big_uint_t *));
        for (i = 0; i < size; i++) {
            mat[i] = (big_uint_t *)calloc(size, sizeof(big_uint_t));
        }
    }

public:
    big_uint_t **mat;
    int size;

    AdjacencyMatrix(const map<int, vector<int>> graph)
    {
        int n;

        // Allocate matrix memory and initialize to zero
        alloc(graph.size());

        // :w a map to convert from node ID to matrix index
        map<int, int> node_to_index;
        n = 0;
        for (auto const& x : graph) {
            node_to_index[x.first] = n++;
        }

        // Fill out the matrix
        for (auto const& x : graph) {
            int from = node_to_index[x.first];

            for (auto const& next : x.second) {
                int to = node_to_index[next];

                mat[from][to] = 1;
            }
        }
    }

    ~AdjacencyMatrix()
    {
        int n;
        for (n = 0; n < size; n++) {
            free(mat[n]);
        }
        free(mat);
    }

    big_uint_t get_num_paths(void)
    {
        return mat[0][size-1];
    }
};

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

int get_device_joltage(const vector<int> v)
{
    int i;
    int max = v[0];
    for (i = 0; i < v.size(); i++) {
        if (max < v[i]) {
            max = v[i];
        }
    }
    return max + 3;
}

int get_chain_value(const vector<int> v)
{
    int diff_count[4] = {0, 0, 0, 0};
    int i;

    for (i = 1; i < v.size(); i++) {
        int d = v[i] - v[i-1];
        
        if (d < 4) {
            diff_count[d] += 1;
        }
    }

    cout << "diff1 = " << diff_count[1] << endl;
    cout << "diff2 = " << diff_count[2] << endl;
    cout << "diff3 = " << diff_count[3] << endl;

    return diff_count[3] * diff_count[1];
}

// Implement adjecency matrix multiplication
void matrix_multiply(const AdjacencyMatrix& a, const AdjacencyMatrix& b, AdjacencyMatrix& result)
{
    int i, j, k;

    for (i = 0; i < result.size; i++) {
        for (j = 0; j < result.size; j++) {
            result.mat[i][j] = 0;
            for (k = 0; k < result.size; k++) {
                result.mat[i][j] += a.mat[i][k] * b.mat[k][j];
            }
        }
    }
}

int count_routes_slow(map<int, vector<int>> graph, int start, int target)
{
    // Slow method
    // - Traverse every path in the graph.
    // - Count up every time an the stop node is reached.
    // - After reaching a dead end, re-trace step back to previous branch
    //   point.

    int num_iter = 0;
    int num_routes = 0;
    int i;
    vector<int> q;

    q.push_back(start);
    while (q.size() > 0) {
        int here = q.back();
        q.pop_back();

        vector<int> next = graph[here];

        for (i = 0; i < next.size(); i++) {
            q.push_back(next[i]);
        }

        if (here == target) {
            num_routes += 1;
        }

        num_iter += 1;
    }

    return num_routes;
}


big_uint_t count_routes_fast(map<int, vector<int>> graph)
{
    // Fast method:
    // - Generate an Adjacency Matrix, A.
    // - A**n contains the number of paths of lenth n between any two points in
    //   the graph.
    // - By summing the value of A**n in the cell corresponding to the path
    //   between start and stop node for 0 <= n < graph.size(), we get the total
    //   number of paths between 

    auto a = AdjacencyMatrix(graph);
    auto b = AdjacencyMatrix(graph);

    int i;
    big_uint_t sum = 0;

    for (i = 0; i < graph.size(); i++) {
        matrix_multiply(a, b, b);

        big_uint_t n = b.get_num_paths();
        sum += n;
    }

    return sum;
}

void print_num_arrangements(const vector<int> v)
{
    map<int, vector<int>> graph;
    int i, j;

    for (i = 0; i < v.size(); i++) {
        for (j = i+1; j < v.size(); j++) {
            int d = v[j] - v[i];

            if (d > 3) {
                break;
            }
            else {
                graph[v[i]].push_back(v[j]);
            }
        }
    }

    // Add the final node for completeness
    vector<int> empty;
    graph[v[v.size()-1]] = empty;

    big_uint_t num_paths_total;

    // Slow
    /* num_paths_total = count_routes_slow(graph, 0, v[v.size()-1]); */
    /* cout << "Number of paths: " << num_paths_total << endl; */

    // Fast
    num_paths_total = count_routes_fast(graph);
    cout << "Number of paths: " << num_paths_total << endl;
}

int main(int argc, const char *argv[]) 
{

    vector<int> v = load_file(INPUT_FILE);
    sort(v.begin(), v.end());

    v.insert(v.begin(), 0); // Outlet joltage
    v.push_back(get_device_joltage(v));

    int n = get_chain_value(v);
    cout << "diff3 * diff1 = " << n << endl;

    print_num_arrangements(v);
    /* cout << "num_arrangements: " << num_arrangements << endl; */
}
