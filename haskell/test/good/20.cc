double test3(int t)
{
    return 4.0209;
}

bool test2(double t)
{
    return false;
}

int test1(bool t) {
    return 0;
}

int main()
{
    double x;
    {
        x = 5.24533;
        int x = 5;
        return x;
    }
    x = 5.03857;
    x++;
    x--;
    --x;
    ++x;
    bool y = test2(x--);
    return test1(test2(test3(main())));
}