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
