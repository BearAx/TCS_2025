#include <iostream>
#include <vector>

using namespace std;

// Function to process the cards and distribute them to Ahmed and Karim
void processCards(const vector<int>& N, const vector<int>& M, vector<int>& Ahmed, vector<int>& Karim)
{
    vector<int> copy_N = N; // Create a copy of Ahmed's cards
    vector<int> copy_M = M; // Create a copy of Karim's cards
    vector<int> temp(copy_M.size(), 0); // Vector to hold Karim's cards in order

    // Loop through both arrays to find matching cards
    for (size_t i = 0; i < copy_N.size(); ++i)
        {
        for (size_t j = 0; j < copy_M.size(); ++j)
            {
            if (copy_N[i] == copy_M[j] && copy_N[i] != 0 && copy_M[j] != 0)
                {
                if (i < j)
                {
                    Ahmed.push_back(copy_N[i]);
                }
                else if (i > j)
                {
                    temp[j] = copy_M[j];
                }
                copy_N[i] = 0;
                copy_M[j] = 0;
            }
        }
    }


    // Add remaining cards of Karim that are not taken to Karim's final array
    for (int card : temp)
        {
        if (card != 0)
        {
            Karim.push_back(card);
        }
    }
}

// Function to print the final cards of each player
void finalCards(const vector<int>& cards)
{
    // If no cards, print a dash
    if (cards.size() == 0)
    {
        cout << "-";
    }
    else
    {
        // Otherwise, print the cards
        for (int card : cards)
        {
            cout << card << " ";
        }
    }
    cout << endl;
}

// Function to determine the winner based on the number of cards
void winner(const vector<int>& a, const vector<int>& k)
{
    if (a.size() > k.size())
    {
        cout << "Ahmed" << endl;  // Ahmed wins if he has more cards
    }
    else if (a.size() < k.size())
    {
        cout << "Karim" << endl;  // Karim wins if he has more cards
    }
    else
    {
        cout << "Tie" << endl;  // It's a tie if they have the same number of cards
    }
}

int main()
{
    vector<int> N;  // Array to hold Ahmed's cards
    vector<int> M;  // Array to hold Karim's cards
    int card;

    // Read Ahmed's cards until 0 is entered
    while (cin >> card && card != 0)
    {
        N.push_back(card);
    }

    // Read Karim's cards until 0 is entered
    while (cin >> card && card != 0)
    {
        M.push_back(card);
    }

    vector<int> Ahmed_Final;  // Array to store Ahmed's final cards
    vector<int> Karim_Final;  // Array to store Karim's final cards

    // Process the cards to distribute them
    processCards(N, M, Ahmed_Final, Karim_Final);

    // Print Ahmed's final cards
    finalCards(Ahmed_Final);

    // Print Karim's final cards
    finalCards(Karim_Final);

    // Determine and print the winner
    winner(Ahmed_Final, Karim_Final);

    return 0;
}
