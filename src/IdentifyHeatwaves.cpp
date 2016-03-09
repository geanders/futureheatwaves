#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//Prototypes
//Note: The code may work without prototypes on certain systems, but not on
// systems like Solaris. It is best to keep the prototypes in.
DataFrame IDHeatwavesCPP(int, NumericVector);
void storeHeatwaveEntry(int, int, int, std::vector<int> &, std::vector<int> &);
void storeZeroes(int, int, std::vector<int> &, std::vector<int> &);

//' Identify heat waves that are a certain number of days long
//'
//' This function identifies heat waves that are a certain number of days long.
//'
//' @param heatwaveLength Number of days that a heat wave lasts under the
//'    user's definition
//' @param tempsExceedingCutoff A vector of 1s and 0s the length of the
//'     number of days in the time series. 1 means the temperature for that
//'     day exceeds the calculated threshold; 0 means it does not exceed the
//'     threshold.
//'
//' @return A dataframe containing information about the heat waves for this
//'    series. It contains two columns: \code{hw} and \code{hw.number}.
//' @export
// [[Rcpp::export]]
DataFrame IDHeatwavesCPP(int heatwaveLength, NumericVector tempsExceedingCutoff){

        // Declare vectors that will form columns of the dataframe; Allocate memory.
        int dataLength = tempsExceedingCutoff.size();
        std::vector<int> hw(dataLength);
        std::vector<int> hwNumber(dataLength);

        // Holder variable for examining individual instances of cases that
        // could be heat waves
        int potentialHeatwave = 0;

        // Counter of # of heat waves. Initialize to 0 for first heat wave.
        int hwCounter = 0;

        for(int i = 0; i < dataLength; i++){

                // If a 1 is encountered, increment the potentialHeatwave counter
                if(tempsExceedingCutoff[i] == 1){
                        potentialHeatwave++;
                }else{
                        //Check if the potential heat wave represents an actual
                        // heat wave
                        if(potentialHeatwave >= heatwaveLength){
                                //If heat wave is detected, store its entry.
                                hwCounter++;
                                storeHeatwaveEntry(i, potentialHeatwave,
                                                   hwCounter,
                                                   hw, hwNumber);

                                //Store the zero line at the end of the heat wave
                                storeZeroes(i, 0, hw, hwNumber);

                                //Push back zeroes on the column vectors if it
                                // didn't turn out to be a heat wave.
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

//' Helper function that adds entries
//'
//' This is a helper function for IDHeatwavesCPP that adds entries
//' to the data structures that track heatwave information
//'
//' @param index A running index.
//' @param hwSize Size of the heat wave to be added.
//' @param hwCounter Current number of heat waves.
//' @param hw A reference of the vector that contains the heat waves
//' @param hwNumber A reference of the vector that contains the heat wave
//'    numbers.
//'
// [[Rcpp::export]]
void storeHeatwaveEntry(int index, int hwSize, int hwCounter, std::vector<int>& hw, std::vector<int>& hwNumber){
        index = index - hwSize;
        for(int i = 0; i < hwSize; i++){
                // Push back 1 hwSize number of times onto hw.
                hw[index + i] = 1;

                // Push back hwCounter value of hwSize number of times onto
                // hwNumber.
                hwNumber[index + i] = hwCounter;
        }
}

//' Helper function that adds zeroes
//'
//' This function is a helper function that adds zeros to the data structures
//' that track heat wave information
//'
//' @param potentialHeatwave Size of the potential heat wave that turned out
//'    not to be a heat wave.
//' @param hw A reference of the vector that contains the heat waves
//' @param hwNumber A reference of the vector that contains the heat wave
//'    numbers.
//' @inheritParams storeHeatwaveEntry
//'
// [[Rcpp::export]]
void storeZeroes(int index, int potentialHeatwave, std::vector<int>& hw, std::vector<int>& hwNumber){
        index = index - potentialHeatwave;
        //Increment potentialHeatwave by 1, since we want to add
        // potentialHeatwave + 1 number of zeroes to the column variables
        //This is because the potentialHeatwave variable counts only the rows
        //that were a part of the potential
        //heat wave, but excludes the row that was skipped over in the outer
        //if/else if clause of the IDHeatwavesCPP
        //function.
        int zercount = potentialHeatwave + 1;

        //Store the zeroes.
        for(int i = 0; i < zercount; i++){
                hw[index + i] = 0;
                hwNumber[index + i] = 0;
        }
}

/*** R
data(datafr, package = "futureheatwaves")
threshold <- 84.632
tempsExceedingthreshold <- as.numeric(datafr[ , 2] >= threshold)
tempsExceedingthreshold <- c(tempsExceedingthreshold, 0)
heatwaves <- IDHeatwavesCPP(heatwaveLength = 2,
                            tempsExceedingCutoff = tempsExceedingthreshold)
heatwaves <- heatwaves[-nrow(heatwaves), ]
heatwaves <- cbind(datafr, heatwaves)
colnames(heatwaves) <- c("date", "tmpd", "hw", "hw.number")
*/
