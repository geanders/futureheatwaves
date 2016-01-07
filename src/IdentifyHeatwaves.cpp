#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using namespace std;

// Function prototypes. Still not 100% sure I need these.
void addHeatwaveEntry(int, int, vector<int>&, vector<int>&, vector<int>&);

// [[Rcpp::export]]
DataFrame IDHeatwavesCPP(int heatwaveLength, NumericVector tempsExceedingCutoff){

        // Initialize the variables that will form the columns of the dataset
        vector<int> hw, hwNumber, firstHwDay;

        // Allocate memory
        int dataLength = tempsExceedingCutoff.size();
        hw.reserve(dataLength); hwNumber.reserve(dataLength); firstHwDay.reserve(dataLength);

        // Holder variable for examining individual instances of cases that could be heatwaves
        vector<int> potentialHeatwave;

        // Counter of # of heatwaves
        int hwCounter = 1;


        for(NumericVector::iterator i = tempsExceedingCutoff.begin(); i != tempsExceedingCutoff.end(); i++){

                if(*i == 1){
                        potentialHeatwave.push_back(1);
                }else{
                        if(potentialHeatwave.size() >= heatwaveLength){
                                addHeatwaveEntry(potentialHeatwave.size(), hwCounter, hw, hwNumber, firstHwDay);
                                hwCounter++;
                        } else if (potentialHeatwave.size() == 1){
                                hw.push_back(0);
                                hwNumber.push_back(0);
                                firstHwDay.push_back(0);
                        }
                        hw.push_back(0);
                        hwNumber.push_back(0);
                        firstHwDay.push_back(0);
                        potentialHeatwave.clear();
                }
        }

        // wrap the C++ datastructures to analogous R forms
        NumericVector heat = wrap(hw);
        NumericVector hwNum = wrap(hwNumber);
        NumericVector firstDay = wrap(firstHwDay);

        return DataFrame::create(_["hw"] = heat, _["hw.number"] = hwNum, _["first.hw.day"] = firstDay);
}

void addHeatwaveEntry(int hwSize, int hwCounter, vector<int>& hw, vector<int>& hwNumber, vector<int>& firstHwDay){

        // firstHwDay gets "1" pushed onto it first because that is the only piece of information
        // we require of that column
        firstHwDay.push_back(1);


        for(int i = 0; i < hwSize; i++){
                // Push back 1 hwSize number of times onto hw
                hw.push_back(1);

                // Push back hwCounter value of hwSize number of times onto hwNumber
                hwNumber.push_back(hwCounter);

                // Push back 0 hwSize - 1 number of times onto firstHwDay
                if(i < (hwSize - 1)){
                        firstHwDay.push_back(0);
                }
        }
}
