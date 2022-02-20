#include <string>

using std::string;

int main()
{
    int x = 10;
    {
        int x = x + 1;
        {
            string x = "hi";
            return x;
        }
        int y = x;
    }
    return x;
}
