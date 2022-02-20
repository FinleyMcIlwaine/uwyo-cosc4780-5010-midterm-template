#include <iostream>
#include <string>

using std::string;

int main()
{
    string x = "hi";
    {
        int x = 5;
        return x;
    }
    {
        int y = 4;
        {
            return y;
        }
    }
    return 0;
}
