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

int modulo(int num, int div)
{
    // dummy modulo
    return 0;
}

int main()
{
    int rows, coef, space, i, j;
    coef = 1;
    i = 0;
    printf("Enter the number of rows: ");
    scanf("%d", rows);
    while (i++ < rows)
    {
        space = 1;
        while (space++ <= rows - i)
            printf("  ");
        j = 0;
        while (j++ <= i)
        {
            if (j == 0 || i == 0)
                coef = 1;
            else
                coef = coef * (i - j + 1) / j;
            printfmt("%4d", coef);
        }
        printf("\n");
    }
    return 0;
}

int factr(int n)
{
    if (n < 2)
        return 1;
    else
        return (n * factr(n - 1));
    return 0;
}

int main1()
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

    int x = 10;
    string y = "ayyy";
    bool test;
    int z = 15;
    while (test = isGreater(x, z))
    {
        x--;
    }

    return x;
}

bool isGreater(int x, int y)
{
    return x > y;
}
