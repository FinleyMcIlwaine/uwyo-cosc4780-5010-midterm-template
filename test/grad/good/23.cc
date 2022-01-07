#include <iostream>

using std::cout;
using std::endl;

int main()
{
    double y = x();
    for (int i = 0; i < 10; i++)
    {
        double x = i + 4.35;
        return x == 4.253;
    }
}

bool x()
{
    return false;
}

int y()
{
    return 3;
}
