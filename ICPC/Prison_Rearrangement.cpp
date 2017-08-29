// Prison Rearrangement - 2936 (ACM-ICPC Live Archive)
// Accepted - Run Time: 0.124

#include <iostream>
#include <vector>
#include <stack>
using namespace std;

// Function prototype: 'solve'
int solve(int m, int r, vector<int> pairs);

// Function: 'main'
int main(void) {

	// Declare required variables
    int cases, m, r, next, result;
	vector<int> pairs;

	// Read number of test cases
    cin >> cases;

	// Loop over all test cases
    for(int i = 1; i <= cases; i++) {
        
		// Read input
		cin >> m >> r;
        
		// Clear vector 'vec'
		while(!pairs.empty())
			pairs.pop_back();

		// Save all pairs in vector
        for(int j = 1; j <= r*2; j++) {
            cin >> next;
            pairs.push_back(next);
        }

		// Call 'solve' and save the return value as result
        result = solve(m, r, pairs);

		// Print result
        cout << result << endl;
    }

	// Exit programm
    return 0;
}

// Function: 'solve'
int solve(int m, int r, vector<int> pairs) {
    
	// Declare local variables
	bool adjacency[m][m], besucht[2*m]; 
	int tempInt, inA, inB, result;
	int matrix[(m/2)+1][(2*m)+1], matrix2[(m/2)+1][(2*m)+1];
	vector<int>::iterator iter;
	stack<int> tempStack;
    vector<int> cc;

	// Initialize adjacency matrix to all 'false'
    for(int i = 0; i < m; i++) {
        for(int j = 0; j < m; j++) {
            adjacency[i][j] = false;
        }
    }

	// Mark pairs in adjacency matrix
    for(iter = pairs.begin(); iter != pairs.end(); iter++) {
        int i = (*iter) - 1;
        iter++;
        int j = (*iter) - 1;
        adjacency[i][j] = true;
    }
    
	// Initialize 'besucht' to all 'false'
    for(int i = 0; i < 2*m; i++)
        besucht[i] = false;

	// Initialize connected component weights to '0'
	inA = 0;
    inB = 0;
    
	// Look for connected components via BFS and save weights for both prisons
    for(int i = 0; i < 2*m; i++) {
        if(besucht[i] == 1)
            continue;
        tempStack.push(i);
        while(!tempStack.empty()) {
            tempInt = tempStack.top();
            tempStack.pop();
            if(besucht[tempInt] == 1)
                continue;
            besucht[tempInt] = 1;
            if(tempInt < m) {
                inA++;
                for(int j = 0; j < m; j++) {
                    if(adjacency[tempInt][j] == 1) {
                        tempStack.push(j+m);
                    }
                }
            }
            else {
                inB++;
                for(int j = 0; j < m; j++) {
                    if(adjacency[j][tempInt - m] == 1) {
                        tempStack.push(j);
                    }
                }
            }
        }
        cc.push_back(inA);
        cc.push_back(inB);
        inA = 0;
        inB = 0;
    }

	// Look for best result via dynamic programming using the CC weights
    for (int i = 0; i < (m / 2) + 1; i++)
        for (int j = 0; j < (2 * m) + 1; j++)
            matrix[i][j] = 0;
    matrix[0][m] = 1;
    
    for (int i = 0; i < (m / 2) + 1; i++)
        for (int j = 0; j < (2 * m) + 1; j++)
            matrix2[i][j] = 0;
    matrix2[0][m] = 1;
    
    for(iter = cc.begin(); iter != cc.end(); iter++) {
        int a = *iter;
        iter++;
        int b = *iter;
        for (int i = 0; i < (m/2)+1; i++) {
            for(int j = 0; j < (2*m)+1; j++) {
                if(matrix[i][j] == 1) {
                    int kr1 = i + a;
                    int kr2 = j + a - b;
                    if ((kr1 <= (m / 2)) && (kr2 <= 2 * m) && (kr1 >= 0) && (kr2 >= 0)) {
                        matrix2[kr1][kr2] = 1;
                    }
                }
            }
        }
        for (int i = m / 2; i >= 0; i--) {
            for (int j = 2 * m; j >= 0; j--) {
                if (matrix2[i][j] == 1) {
                    matrix[i][j] = 1;
                }
            }
        }
    }
    
	// Lookup for result
    result = 0;
    for (int i = 0; i < (m / 2) + 1; i++) {
        if (matrix[i][m] == 1)
            result = i;
    }

	// Return result
    return result;
}