#include <string>

class plotResults {
 private:
  int numberOfRuns;
  std::string *pathToData;
 public:
  // plotResults(char InputFileA[], char InputFileB[], const char OutPutName[]="pretty.plots.root");
   plotResults(const char InputFileA[],const char InputFileB[]);
  ~plotResults();
  bool run();
  void printNoRuns();

};
