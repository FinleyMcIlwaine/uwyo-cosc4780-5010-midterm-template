int fn()
{
    int x = 10;
    if (x == 10)
    {
        return x;
    }
    else
    {
        x = 42;
    }
    // No (generally statically verifiably guaranteed) return! :o
}

int main()
{
    return 0;
}