#pragma once

#include <vector>
#include <string>
#include <map>

struct letterStruct;

extern std::vector<std::string> MasterList;
extern std::vector<std::string> Perms;

extern std::vector<letterStruct> returnAlphabet();
extern std::string getGuess();
extern std::string getFeedback();
extern void incorporateInfo(const std::string guess, const std::string feedback, std::vector<letterStruct>* alphabet);
extern void setAlphaInfo(std::map<char, std::vector<int>>* ref_yellows, std::map<char, std::vector<int>>* ref_greens, std::string* ref_grays, std::string* ref_doubles, const std::vector<letterStruct> alphabet);
extern std::vector<std::string> getIncorrectGuesses(const std::vector<std::string> possible_guesses, const std::vector<letterStruct> alphabet);
extern void removeIncorrectGuesses(std::vector<std::string>* possible_guesses, const std::vector<std::string> wrong_guesses);
extern std::vector<int> getGuessLogistics(const std::string guess, const std::vector<std::string> guesses_possible);
extern void sortGuessLogistics(std::vector<int>* guess_log);
extern void pruneGuessLogistics(std::vector<int>* guess_log, const int len_of_wrong_guess_length);
extern int getGuessLogisticStatistics(const std::vector<int> pruned_guess_log);
extern bool compareSecondValues(std::pair<std::string, int>& a, std::pair<std::string, int>& b);
extern std::vector<std::pair<std::string, int>> compareAllWords(const std::vector<std::string> possible_words);

