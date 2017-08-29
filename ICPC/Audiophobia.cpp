// Audiophobia - 10048 (UVa Online Judge)
// Accepted - Run Time: 0.052

#include <iostream>
#include <algorithm>
using namespace std;

// Function: 'main'
int main(void) {
    
	// Declare required variables
    int crossings, streets, queries, count = 1;
    int c1, c2, d;
    int adjacency[100][100];
    
	// Read input
    cin >> crossings >> streets >> queries;
    
	// Endless loop
    while(true) {
        
		// Break
        if(crossings == 0 && streets == 0 && queries == 0)
            break;
        
		// Initialize adjacency matrix
        for(int i = 0; i < 100; i++)
            for(int j = 0; j < 100; j++)
                adjacency[i][j] = 100000;
    
		// Read streets and fill adjacency matrix with intesity levels
        for(int i = 0; i < streets; i++) {
            cin >> c1 >> c2 >> d;
            c1--; c2--;
            adjacency[c1][c2] = d;
            adjacency[c2][c1] = d;
        }
	
		// Floyd-Warshall algorithm for APSP (All Pairs Shortest Path)
        for(int k = 0; k < crossings; k++)
            for(int i = 0; i < crossings; i++)
                for(int j = 0; j < crossings; j++) {
                    adjacency[i][j] = min(adjacency[i][j], max(adjacency[i][k], adjacency[k][j]));
                }
        
		// Print case number
        cout << "Case #" << count << endl;
        count++;
        
		// Lookup for every query
        for(int i = 0; i < queries; i++) {
            cin >> c1 >> c2;
            c1--; c2--;
            int tmp = adjacency[c1][c2];
            if(tmp == 100000)
                cout << "no path" << endl;
            else
                cout << adjacency[c1][c2] << endl;
        
        }
        
		// Read next input and check, if endline is necesarry
        cin >> crossings >> streets >> queries;
        if(!(crossings == 0 && streets == 0 && queries == 0))
            cout << endl;
    }
	
	// Exit programm
    return 0;
}