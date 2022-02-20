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
    return 0;
}

int go2(int x)
{
    if (x < 0)
        return x;
    else
        return go1(x + 1);
    return 0;
}