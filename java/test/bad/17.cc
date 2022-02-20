#include <string>

using std::string;

int main()
{
    int x = 10;
    string y = "ayyy";
    bool test;
    int z = 15;
    while (z = isGreater(x, z))
    {
        x--;
    }

    return x;
}

bool isGreater(int x, int y)
{
    return x > y;
}