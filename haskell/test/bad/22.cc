int main()
{
    int x = 10;
    return x;
}

int fn()
{
    int x = 10;
    while (x >= 0)
    {
        x--;
        return x;
    }
    // No (generally statically verifiably guaranteed) return! :o
}