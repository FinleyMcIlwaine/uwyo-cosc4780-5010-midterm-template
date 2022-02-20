#include <string>

using std::string;

int main()
{
    int x = 10;
    string y = "ayyy";
    int z = 15;
    while (isGreater(x, z))
    {
        x--;
    }
    return x;
}

bool isGreater(int x, int y)
{
    return x > y;
}