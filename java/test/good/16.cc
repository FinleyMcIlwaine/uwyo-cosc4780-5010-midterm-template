#include <stdio.h>
#include <string>

using std::string;

void printf(string s)
{
    // print
}

void printfmt(string fmt, int val)
{
    // print
}

void scanf(string fmt, int val)
{
    // read
}

int modulo(int num, int div) {
    // dummy modulo
    return 0;
}

int main()
{
    int n, reversed, remainder, original;
    reversed = 0;
    printf("Enter an integer: ");
    scanf("%d", n);
    original = n;

    while (n != 0)
    {
        remainder = modulo(n, 10);
        reversed = reversed * 10 + remainder;
        n = n / 10;
    }

    if (original == reversed)
        printfmt("%d is a palindrome.", original);
    else
        printfmt("%d is not a palindrome.", original);

    return 0;
}