#include <string>

using std::string;

bool compareStr(string x, string y)
{
    return x > y;
}

bool compareDbl(double x, double y)
{
    return x == y;
}

bool compareInt(int x, int y)
{
    return x != y;
}
bool compareBool(bool x, bool y)
{
    return x <= compareInt(5, 6);
}

int main()
{
    bool i = compareStr("hi", "hello");
    i = compareDbl(3.14, 5.0);
    i = compareInt(10, 11);
    i = compareBool(true, false);
    return 0;
}