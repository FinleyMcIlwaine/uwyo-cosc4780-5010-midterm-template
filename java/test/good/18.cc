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
    return test1(test2(test3(main())));
}