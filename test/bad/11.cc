#include <string>

using std::string;

string hello(int x)
{
    {
        string x = "hey";
        return x;
    }
    return;
}

int main()
{
    return 0;
}
