double fn()
{
    {
        while(true) 
        {
            return 1.0;
        }
    }
    // No (generally statically verifiably guaranteed) return! :o
}

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
}