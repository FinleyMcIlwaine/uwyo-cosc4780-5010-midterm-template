int main()
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