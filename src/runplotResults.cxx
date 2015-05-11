#include <plotResults.h>

int main(){

  plotResults myplotResults("../input/lots_of_numbers.txt", "../input/neutron_energies.txt");
  bool result = myplotResults.run();
  return result;
}
 
