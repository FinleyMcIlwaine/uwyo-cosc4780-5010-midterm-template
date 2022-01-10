int main()
{
    int x, y, z;
    x = 4;
    y = 5;
    z++;
    x = add(y, z) - z * y / x;
    return x;
}

int add(int x, int y)
{
    return x + y;
}