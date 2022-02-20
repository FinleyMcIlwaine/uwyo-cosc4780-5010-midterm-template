int runIt(int start)
{
    return go1(start);
}

int main()
{
    int start = 100;
    return runIt(start);
}

int go1(int x)
{
    if (x < 0)
        return x;
    else
        return go2(x - 2);
}

int go2(int x)
{
    if (x < 0)
        return x;
    else
        return go11(x + 1);
}