#include <string>

using std::string;

string hello(string x)
{
    {
        string x = "hey";
        return;
    }
    return x;
}

int main()
{
    return 0;
}
