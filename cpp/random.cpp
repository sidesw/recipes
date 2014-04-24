/* Random array generator */
/* Copyright 2011 Siddharth Eswaran */

#include <cstdlib>
#include <iostream>

namespace sid
{
  void swap(int &a, int &b)
  {
    int temp = a;
    a = b;
    b = temp;
  }

  void rand_array(int *arr, int n)
  {
    int begin = 0;
    int range = n;

    while(begin < n)
    {
      int pos = std::rand()%range + begin;
      swap(arr[begin], arr[pos]);
      ++begin;
      --range;
    }
  }
}

int main(int argc, char *argv[])
{
  std::srand(std::time(NULL));

  if(argc < 2)
  {
    return -1;
  }

  unsigned int n = std::atoi(argv[1]);

  int *arr = new int[n];
  for(int i=0;i<n;++i)
  {
    arr[i] = i+1;
  }

  sid::rand_array(arr, n);

  for(int i=0;i<n;++i)
  {
    std::cout<<arr[i]<<" ";
  }
  std::cout<<std::endl;

  delete arr;
  return 0;
}
