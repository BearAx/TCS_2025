#include <iostream>
#include <vector>
#include <string>

using namespace std;

int main() {
    // Input number of languages
    int totalLanguages;
    cin >> totalLanguages;

    // Store language-specific words for each language
    vector<vector<string>> langWords(totalLanguages);
    for (int lang = 0; lang < totalLanguages; ++lang) {
        int wordCount;
        cin >> wordCount;
        langWords[lang].resize(wordCount);
        for (int idx = 0; idx < wordCount; ++idx) {
            cin >> langWords[lang][idx];
        }
    }

    // Input sentence details
    int sentenceLength;
    cin >> sentenceLength;
    vector<string> phrase(sentenceLength);
    for (int i = 0; i < sentenceLength; ++i) {
        cin >> phrase[i];
    }

    // Match counts for each language
    vector<int> matchCounts(totalLanguages, 0);

    // Iterate through each language and check matches with the sentence
    for (int langId = 0; langId < totalLanguages; ++langId) {
        for (const string& term : phrase) {
            bool foundMatch = false;
            for (const string& langTerm : langWords[langId]) {
                if (term == langTerm) {
                    matchCounts[langId]++;
                    foundMatch = true;
                    break;
                }
            }
            if (foundMatch) continue;
        }
    }

    // Find the maximum match count
    int highestMatch = -1;
    for (int count : matchCounts) {
        if (count > highestMatch) {
            highestMatch = count;
        }
    }

    // Output languages with the highest match count
    bool isFirst = true;
    for (int langId = 0; langId < totalLanguages; ++langId) {
        if (matchCounts[langId] == highestMatch) {
            if (!isFirst) cout << " ";
            cout << (langId + 1);
            isFirst = false;
        }
    }
    cout << endl;

    return 0;
}