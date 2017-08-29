// Partitioning by Palindromes - 11584 (UVa Online Jugde)
// Accepted - Run Time: 0.440

#include <iostream>
#include <string>
using namespace std;

// Function prototype: 'check'
int check(string a);

// Fuction: 'main'
int main(void) {
    
    // Declare required variables
    int cases, count;
    int dynamic[1001];
    string a, b;
    
    // Read number of cases
    cin >> cases;
    
    // Loop over all cases
    for(int k = 0; k < cases; k++) {
        
        // Read string
        cin >> a;
        
        // Initiate first two values for dynamic progress
        dynamic[0] = 0;
        dynamic[1] = 1;
        
        // Look for minimal number of palindromes via dynamic programming
        for(int i = 2; i <= a.size(); i++) {
            b = a.substr(a.size()-i, -1);
            count = dynamic[b.size()-check(b)] + 1;
            if(count < (dynamic[i-1]+1)) {
                dynamic[i] = count;
            }
            else {
                dynamic[i] = dynamic[i-1]+1;
            }
        }
        
        // Print result
        cout << dynamic[a.size()] << endl;
    }
    
    // Exit programm
    return 0;
}

// Fuction: 'check'
//  -> Returns the length of the largest palindrome of string, read from left to right
int check(string a) {
    
    bool flag;
    for(int j = static_cast<int>(a.size())-1; j >=0; j--) {
        flag = true;
        if(a[j] == a[0]) {
            for(int i = 1; i <= j/2; i++) {
                if(a[i] != a[j-i]) {
                    flag = false;
                }
            }
            if(flag)
                return j+1;
        }
    }
    
    // Returns '-99999999' when failing
    return -99999999;
}