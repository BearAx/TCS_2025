#include <iostream>
#include <string>
#include <vector>

using namespace std;

class Medvedev_Aleksandr_FST {
    string substring;
    vector<int> prefix;
    int state;

public:
    // Constructor: Initializes the FST with the given substring.
    Medvedev_Aleksandr_FST(string sub) : substring(sub), state(0) {
        int n = sub.size(); // Length of the substring.
        prefix.resize(n); // Resize the prefix table to match the substring length.
        prefix[0] = 0; // The first value of the prefix table is always 0.

        // Compute the prefix function for the substring.
        for (int i = 1; i < n; ++i) {
            int j = prefix[i - 1]; // Start with the previous value in the prefix table.

            // Adjust 'j' until we find a matching character or reach the beginning of the substring.
            while (j > 0 && sub[i] != sub[j]) {
                j = prefix[j - 1];
            }

            // If characters match, increment 'j'.
            if (sub[i] == sub[j]) {
                j++;
            }

            // Store the computed value in the prefix table.
            prefix[i] = j;
        }
    }

    // Process a single character from the input string.
    int process(char c) {
        while (true) {
            // If the current character matches the next character in the substring, move to the next state.
            if (state < substring.size() && c == substring[state]) {
                state++;

                // If we have matched the entire substring, return 1 (indicating a match was found).
                if (state == substring.size()) {
                    int result = 1;
                    state = prefix[state - 1]; // Reset the state using the prefix table.
                    return result;
                } else {
                    return 0; // No complete match yet, so return 0.
                }
            } else {
                // If no match and we are at the start of the substring, return 0.
                if (state == 0) {
                    return 0;
                }

                // Otherwise, use the prefix table to reset the state.
                state = prefix[state - 1];
            }
        }
    }
};

int main() {
    string C, S;
    cin >> C >> S;

    Medvedev_Aleksandr_FST fst(C); // Create an FST object with the substring.
    int count = 0;

    // Process each character in the main string.
    for (char c : S) {
        count += fst.process(c); // Add the result of processing each character to the counter.
    }

    // If the substring was found at least once, print the count. Otherwise, print "NOT DETECTED".
    if (count > 0) {
        cout << count;
    } else {
        cout << "NOT DETECTED";
    }

    return 0;
}
