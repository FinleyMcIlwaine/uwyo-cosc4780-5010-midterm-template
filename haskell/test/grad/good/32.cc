#include <string>

using std::string;

int main()
{
    int x = 1;
    if (x == 1)
    {
        return 3;
    } 
    else 
    {
        x = 2;
    }
    // not valid C++, but valid CPP
    for (string y = x < 10; x < 10; x++ )
    {
        return x;
    }
    return x;
}