#include <Rcpp.h>
#include <vector.h>
using namespace Rcpp;

//TODO: TEST

// [[Rcpp::interfaces(r, cpp)]]

//Prototypes
//Note: The code may work without prototypes on certain systems, but not on systems like Solaris. It
//is best to keep the prototypes in.
DataFrame IDHeatwavesCPP(int, NumericVector);
void storeHeatwaveEntry(int, int, int, std::vector<int> &, std::vector<int> &);
void storeZeroes(int, int, std::vector<int> &, std::vector<int> &);

//' Identify heatwaves that are a certain number of days long
//'
//' @param heatwaveLength Number of days that a heatwave lasts under the
//'    user's definition
//' @param tempsExceedingCutoff A vector of 1s and 0s that is the size of the
//'     number of days in the time series. 1 means the temperature for that
//'     day exceeds the calculated threshold. 0 means it does not exceed the
//'     threshold.
//'
//' @return A dataframe containing information about the heatwaves for this series. It contains three columns with
//'    names "hw", "hw.number", and "first.hw.day" respectively.
//' @export
// [[Rcpp::export]]
DataFrame IDHeatwavesCPP(int heatwaveLength, NumericVector tempsExceedingCutoff){

        // Declare vectors that will form columns of the dataframe; Allocate memory.
        int dataLength = tempsExceedingCutoff.size();
        std::vector<int> hw(dataLength); std::vector<int> hwNumber(dataLength);

        // Holder variable for examining individual instances of cases that could be heatwaves
        int potentialHeatwave = 0;

        // Counter of # of heatwaves. Initialize to 1 for first heatwave.
        int hwCounter = 0;

        // Size of the time series we're dealing with
        int size = tempsExceedingCutoff.size();

        for(int i = 0; i < size; i++){

                // If a 1 is encountered, increment the potentialHeatwave counter
                if(tempsExceedingCutoff[i] == 1){
                        potentialHeatwave++;
                }else{
                        //Check if the potential heatwave represents an actual heatwave
                        if(potentialHeatwave >= heatwaveLength){
                                //If heatwave is detected, store it's entry.
                                hwCounter++;
                                storeHeatwaveEntry(i, potentialHeatwave, hwCounter, hw, hwNumber);

                                //Store the zero line at the end of the heatwave
                                storeZeroes(i, 0, hw, hwNumber);

                                //Push back zeroes on the column vectors if it didn't turn out to be a heatwave.
                        } else if (potentialHeatwave < heatwaveLength){
                                storeZeroes(i, potentialHeatwave, hw, hwNumber);
                        }

                        potentialHeatwave = 0;
                }
                Rcpp::checkUserInterrupt();
        }

        // wrap the C++ datastructures to analogous R forms
        NumericVector heat = wrap(hw);
        NumericVector hwNum = wrap(hwNumber);

        return DataFrame::create(_["hw"] = heat, _["hw.number"] = hwNum);
}

//' Helper function for IDHeatwavescpp that addes entries to the data structures that track
//' heatwave information
//'
//' @param index An index into the various
//' @param hwSize Size of the heatwave to be added.
//' @param hwCounter Current number of heatwaves.
//' @param hw A reference of the vector that contains the heatwaves
//' @param hwNumber A reference of the vector that contains the heatwave numbers.
//'
//' @export
// [[Rcpp::export]]
void storeHeatwaveEntry(int index, int hwSize, int hwCounter, std::vector<int>& hw, std::vector<int>& hwNumber){

        for(int i = 0; i < hwSize; i++){
                // Push back 1 hwSize number of times onto hw.
                hw[index + i] = 1;

                // Push back hwCounter value of hwSize number of times onto hwNumber.
                hwNumber[index + i] = hwCounter;
        }
}

//' Helper function that adds zeroes to the data structures that track heatwave information
//'
//' @param potentialHeatwave Size of the potential heatwave that turned out not to be a heatwave.
//' @param hw A reference of the vector that contains the heatwaves
//' @param hwNumber A reference of the vector that contains the heatwave numbers.
//'
//' @export
// [[Rcpp::export]]
void storeZeroes(int index, int potentialHeatwave, std::vector<int>& hw, std::vector<int>& hwNumber){

        //Increment potentialHeatwave by 1, since we want to add potentialHeatwave + 1 number of zeroes to the column variables
        //This is because the potentialHeatwave variable counts only the rows that were a part of the potential
        //heatwave, but excludes the row that was skipped over in the outer if/else if clause of the IDHeatwavesCPP
        //function.
        int zercount = potentialHeatwave + 1;

        //Store the zeroes.
        for(int i = 0; i < zercount; i++){
                hw[index + i] = 0;
                hwNumber[index + i] = 0;
        }
}
